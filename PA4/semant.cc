#include "semant.h"
#include "utilities.h"
#include <sstream>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include <set>

extern int semant_debug;
extern char *curr_filename;

static bool TESTING = false;
static std::ostringstream nop_sstream;
static std::ostream &log = TESTING ? std::cout : nop_sstream;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol arg, arg2, Bool, concat, cool_abort, copy, Int, in_int, in_string,
    IO, length, Main, main_meth, No_class, No_type, Object, out_int, out_string,
    prim_slot, self, SELF_TYPE, Str, str_field, substr, type_name, val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void) {
  arg = idtable.add_string("arg");
  arg2 = idtable.add_string("arg2");
  Bool = idtable.add_string("Bool");
  concat = idtable.add_string("concat");
  cool_abort = idtable.add_string("abort");
  copy = idtable.add_string("copy");
  Int = idtable.add_string("Int");
  in_int = idtable.add_string("in_int");
  in_string = idtable.add_string("in_string");
  IO = idtable.add_string("IO");
  length = idtable.add_string("length");
  Main = idtable.add_string("Main");
  main_meth = idtable.add_string("main");
  //   _no_class is a symbol that can't be the name of any
  //   user-defined class.
  No_class = idtable.add_string("_no_class");
  No_type = idtable.add_string("_no_type");
  Object = idtable.add_string("Object");
  out_int = idtable.add_string("out_int");
  out_string = idtable.add_string("out_string");
  prim_slot = idtable.add_string("_prim_slot");
  self = idtable.add_string("self");
  SELF_TYPE = idtable.add_string("SELF_TYPE");
  Str = idtable.add_string("String");
  str_field = idtable.add_string("_str_field");
  substr = idtable.add_string("substr");
  type_name = idtable.add_string("type_name");
  val = idtable.add_string("_val");
}

static Class_ curclass = NULL;
static ClassTable *classtable;
// ObjectId to Class(Type)
static SymbolTable<Symbol, Symbol> objectEnv;
// methods map along inheritance path
static SymbolTable<Symbol, Feature_class> inheritEnv;
// ClassName to methodName to methods
static std::map<Symbol, SymbolTable<Symbol, Feature_class>> methodEnv;

ClassTable::ClassTable(Classes classes) : semant_errors(0), error_stream(cerr) {

  install_basic_classes();

  /*
   * std::map<Symbol, Class_> ClassTable::m_classes
   * a map from Symbol to Class_
   * ==============================================
   * std::map<Symbol, std::list<Symbol>> edges;
   * a map from Symbol to its child Symbols
   * building inheritance tree
   */

  // dfs use
  std::map<Class_, int> visited;

  log << "Start checking inheritance" << std::endl;

  /* Fill this in */
  for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
    curclass = classes->nth(i);
    Symbol curname = curclass->get_name();

    if (curname == SELF_TYPE || curname == Int || curname == IO ||
        curname == Bool || curname == Str || curname == Object) {
      semant_error(curclass)
          << "Error! Redefinition of basic class " << curname << std::endl;
      return;
    }

    if (mp_class.find(curname) != mp_class.end()) {
      semant_error(curclass)
          << "Error! Class " << curname << " was previously defined\n";
      return;
    }

    // install map;
    mp_class.insert(std::make_pair(curname, curclass));
    visited.insert(std::make_pair(curclass, 0));
    edges[curclass->get_parent()].push_back(curname);
  }

  if (mp_class.find(Main) == mp_class.end()) {
    semant_error() << "Class Main is not defined.\n";
    return;
  }

  log << "DFS begin\n";

  // dfs for asyclic
  bool err = false, cycle = false;
  for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
    curclass = classes->nth(i);

    if (visited[curclass] == 0) {
      DFS(curclass, classes, visited, err, cycle);
      if (err)
        return;
    }
  }
}

/*
 * find cycles in inheritance path
 * using dfs topologic sort
 */
void ClassTable::DFS(Class_ cur, Classes classes, std::map<Class_, int> &vis,
                     bool &err, bool &cycle) {

  log << "DFS class " << cur->get_name() << "\n";

  if (vis[cur] == 1) {
    cycle = true;
    err = true;
    return;
  }

  Symbol parent_name = cur->get_parent();
  if (mp_class.find(parent_name) == mp_class.end()) {
    semant_error(cur) << "Error! Class " << cur->get_name()
                      << " inherits from an undefined class " << parent_name
                      << std::endl;
    err = true;
    return;
  }

  if (parent_name == Int || parent_name == Str || parent_name == Bool ||
      parent_name == SELF_TYPE) {
    semant_error(cur) << "Error! Class " << cur->get_name()
                      << " cannot inherit " << parent_name << std::endl;
    err = true;
    return;
  }

  vis[cur] = 1;
  if (parent_name != Object)
    DFS(mp_class[parent_name], classes, vis, err, cycle);
  if (cycle) {
    // print all classes involved in a cycle
    semant_error(cur) << "Error! Class " << cur->get_name()
                      << " is involved in an inheritance cycle\n";
  }
  if (err)
    return;
  vis[cur] = 2;
}

void ClassTable::install_basic_classes() {

  // The tree package uses these globals to annotate the classes built below.
  // curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

  // The following demonstrates how to create dummy parse trees to
  // refer to basic Cool classes.  There's no need for method
  // bodies -- these are already built into the runtime system.

  // IMPORTANT: The results of the following expressions are
  // stored in local variables.  You will want to do something
  // with those variables at the end of this method to make this
  // code meaningful.

  //
  // The Object class has no parent class. Its methods are
  //        abort() : Object    aborts the program
  //        type_name() : Str   returns a string representation of class name
  //        copy() : SELF_TYPE  returns a copy of the object
  //
  // There is no need for method bodies in the basic classes---these
  // are already built in to the runtime system.

  Class_ Object_class = class_(
      Object, No_class,
      append_Features(
          append_Features(single_Features(method(cool_abort, nil_Formals(),
                                                 Object, no_expr())),
                          single_Features(method(type_name, nil_Formals(), Str,
                                                 no_expr()))),
          single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
      filename);

  //
  // The IO class inherits from Object. Its methods are
  //        out_string(Str) : SELF_TYPE       writes a string to the output
  //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
  //        in_string() : Str                 reads a string from the input
  //        in_int() : Int                      "   an int     "  "     "
  //
  Class_ IO_class = class_(
      IO, Object,
      append_Features(
          append_Features(
              append_Features(single_Features(method(
                                  out_string, single_Formals(formal(arg, Str)),
                                  SELF_TYPE, no_expr())),
                              single_Features(method(
                                  out_int, single_Formals(formal(arg, Int)),
                                  SELF_TYPE, no_expr()))),
              single_Features(
                  method(in_string, nil_Formals(), Str, no_expr()))),
          single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
      filename);

  //
  // The Int class has no methods and only a single attribute, the
  // "val" for the integer.
  //
  Class_ Int_class = class_(
      Int, Object, single_Features(attr(val, prim_slot, no_expr())), filename);

  //
  // Bool also has only the "val" slot.
  //
  Class_ Bool_class = class_(
      Bool, Object, single_Features(attr(val, prim_slot, no_expr())), filename);

  //
  // The class Str has a number of slots and operations:
  //       val                                  the length of the string
  //       str_field                            the string itself
  //       length() : Int                       returns length of the string
  //       concat(arg: Str) : Str               performs string concatenation
  //       substr(arg: Int, arg2: Int): Str     substring selection
  //
  Class_ Str_class = class_(
      Str, Object,
      append_Features(
          append_Features(
              append_Features(
                  append_Features(
                      single_Features(attr(val, Int, no_expr())),
                      single_Features(attr(str_field, prim_slot, no_expr()))),
                  single_Features(
                      method(length, nil_Formals(), Int, no_expr()))),
              single_Features(method(concat, single_Formals(formal(arg, Str)),
                                     Str, no_expr()))),
          single_Features(
              method(substr,
                     append_Formals(single_Formals(formal(arg, Int)),
                                    single_Formals(formal(arg2, Int))),
                     Str, no_expr()))),
      filename);

  mp_class.insert(std::make_pair(Object, Object_class));
  mp_class.insert(std::make_pair(IO, IO_class));
  mp_class.insert(std::make_pair(Int, Int_class));
  mp_class.insert(std::make_pair(Bool, Bool_class));
  mp_class.insert(std::make_pair(Str, Str_class));

  edges[No_class].push_back(Object);
  edges[Object].push_back(Int);
  edges[Object].push_back(Bool);
  edges[Object].push_back(Str);
  edges[Object].push_back(IO);
}

/*
 * ClassTable::CommonAncestor
 * ============================
 * return the least common ancestor in inheritance tree
 * mark all subtypes of Symbol a as visited
 * find the first visited Symbol in b's subtypes
 */
Symbol ClassTable::CommonAncestor(Symbol a, Symbol b) {
  if (a == SELF_TYPE)
    a = curclass->get_name();
  if (b == SELF_TYPE)
    b = curclass->get_name();

  std::map<Symbol, bool> vis;
  std::map<Symbol, Class_>::iterator iter;
  for (iter = mp_class.begin(); iter != mp_class.end(); ++iter) {
    vis.insert(std::make_pair(iter->first, false));
  }

  Symbol path = a;
  while (path != Object) {
    vis[path] = true;
    path = mp_class[path]->get_parent();
  }
  vis[path] = true;

  path = b;
  while (vis[path] == false) {
    path = mp_class[path]->get_parent();
  }
  return path;
}

/*
 * ClassTable::isSubtype
 * ============================
 * check whether ancestor is a (direct or indirect) ancestor of child
 *
 * input:
 *     Symbol ancestor, Symbol child
 *
 * output:
 *     bool
 *
 * note on SELF_TYPE:
 *     When some object o in class C is of SELF_TYPE,
 *     it means the real(dynamic) type of o might be C,
 *     or any subclass of C, depending on the dynamic type of the containing
 *     object.
 *     Then, how do we check the inheritance in case of SELF_TYPE?
 *
 *     1. ancestor = child = SELF_TYPE
 *        In this case, we know that the 2 objects have the same dynamic type.
 *
 *     2. ancestor = A, child = SELF_TYPE
 *        In this case, we don't know what the dynamic type of child.
 *        So we just assume child is C.
 *        If we know that C <= A, then even child's dynamic type isn't C,
 *        it can only be a subclass of C. so we are still safe.
 *
 *        However, this makes the type checker more strict than the real world.
 *        Consider this scenario:
 *        A < C, and child's dynamic type is A (but the type check can't know
 *        this!)
 *        then the type checker will complain, even though the program should
 *        work.
 *
 *     3. ancestor = SELF_TYPE, child = A
 *        In this case, we have to say that it doesn't type check in any case.
 *        Even if A <= C, ancestor's dynamic type could be a subclass of C,
 *        which might not be an ancestor of A.
 *
 *     To sum up, the type checker is more strict than the real world: it might
 *     reject some valid programs, but it will not tolerate any invalid program.
 */

bool ClassTable::isSubtype(Symbol child, Symbol father) {
  if (child == SELF_TYPE && father == SELF_TYPE)
    return true;
  if (father == SELF_TYPE)
    return false;
  if (child == SELF_TYPE)
    child = curclass->get_name();

  log << "\t\t\t isSubtype";
  while (child != father && child != No_class) {
    child = mp_class[child]->get_parent();
  }
  if (child == father)
    return true;

  return false;
}

Symbol ClassTable::findMethodInAncestor(Symbol classname, Symbol methodname) {
  if (classname == SELF_TYPE)
    classname = curclass->get_name();

  if (classname == No_class)
    return No_class;
  if (methodEnv[classname].lookup(methodname) != NULL)
    return classname;

  return findMethodInAncestor(mp_class[classname]->get_parent(), methodname);
}

bool ClassTable::isBasicClass(Symbol s) {
  if (s != Object && s != Int && s != Str && s != IO && s != Bool)
    return false;
  return true;
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic<<classname<<" "<<methodname<<std::endl; analysis.  There are
// three versions:
//
//    ostream& ClassTable::semant_error()
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream &ClassTable::semant_error(Class_ c) {
  return semant_error(c->get_filename(), c);
}

ostream &ClassTable::semant_error(Symbol filename, tree_node *t) {
  error_stream << filename << ":" << t->get_line_number() << ": ";
  return semant_error();
}

ostream &ClassTable::semant_error() {
  semant_errors++;
  return error_stream;
}

/*----------------------------------------------------------------*/

// no use
void method_class::AddAttribute() {}
void attr_class::AddMethod() {}

/*
 * utils fuction for AddMethod
 * compare two formal list of methods in different classes
 */
Symbol attr_class::matchFormals(Formals actual) { return No_type; }
Symbol method_class::matchFormals(Formals actual) {
  Symbol type = return_type;
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    Symbol actual_type = actual->nth(i)->get_typedecl();
    if (formals->more(i) == false) {
      return No_type;
    }
    Symbol declar_type = formals->nth(i)->get_typedecl();
    if (declar_type != actual_type) {
      return No_type;
    }
  }
  return type;
}

/*
 * If a class C inherits a method f from an ancestor class P, then C may
 * override the inherited definition of f provided the number of arguments, the
 * types of the formal parameters, and the return type are exactly the same in
 * both definitions.
 */
void method_class::AddMethod() {
  log << "addMethod: " << name << " in class " << curclass->get_name()
      << std::endl;
  if (inheritEnv.lookup(name) != NULL) {
    Feature oldmethod = inheritEnv.lookup(name);
    Symbol oldret_type = oldmethod->matchFormals(formals);
    if (oldret_type == No_type || oldret_type != return_type) {
      classtable->semant_error(curclass) << "Invalid override\n";
    }
  }

  inheritEnv.addid(name, copy_Feature());
  methodEnv[curclass->get_name()].addid(name, copy_Feature());
}

void attr_class::AddAttribute() {
  log << "addAttribute: " << name << " " << type_decl << std::endl;
  if (name == self)
    classtable->semant_error(curclass)
        << "'self' cannot be the name of an attribute\n";
  else if (objectEnv.lookup(name) != NULL)
    classtable->semant_error(curclass)
        << "Attribute " << name << " is an attribute of an inherited class\n"
        << std::endl;
  objectEnv.addid(name, new Symbol(type_decl));
}

/*----------------------------------------------------------------------------------*/

void formal_class::AddFormal() {
  if (name == self) {
    classtable->semant_error(curclass)
        << "'self' cannot be the name of a formal parameter\n";
  }
  if (type_decl == SELF_TYPE) {
    classtable->semant_error(curclass)
        << "Formal parameter " << name << " cannot have type SELF_TYPE\n";
  }

  log << "addFormal: " << name << " " << type_decl << std::endl;
  if (objectEnv.probe(name) != NULL)
    classtable->semant_error(curclass)
        << "Formal parameter " << name << " is multiply defined\n";
  objectEnv.addid(name, new Symbol(type_decl));
}

Symbol method_class::CheckFeatureType() {
  objectEnv.enterscope();
  for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
    Formal f = formals->nth(i);
    f->AddFormal();
  }

  Symbol expr_type = expr->CheckExprType();
  Symbol type = expr_type;

  log << "check Method: " << name << "  " << expr_type << "  " << return_type
      << std::endl;

  if (classtable->isSubtype(expr_type, return_type) == false) {
    classtable->semant_error(curclass)
        << "Inferred return type " << expr_type << " of method " << name
        << " does not conform to declared return type " << return_type
        << std::endl;
    type = Object;
  }

  objectEnv.exitscope();

  return type;
}

Symbol attr_class::CheckFeatureType() {
  log << "\t attr_class \n";
  Symbol expr_type = init->CheckExprType();
  if (expr_type == No_type)
    return type_decl;

  Symbol type = expr_type;
  if (classtable->isSubtype(expr_type, type_decl) == false) {
    classtable->semant_error(curclass) << "Error! feature assign invalid\n";
    type = Object;
  }
  return type;
}

/*-----------------------------------------------------------------------------*/

Symbol assign_class::CheckExprType() {
  log << "\t assign\n";
  Symbol *lval = objectEnv.lookup(name);
  Symbol rval = expr->CheckExprType();

  type = rval;

  if (lval == NULL || name == self) {
    classtable->semant_error(curclass)
        << "Cannot assign to " << name << std::endl;
    type = Object;
    return type;
  }

  log << "\t\t lval " << *lval << " rval " << type << std::endl;

  if (classtable->isSubtype(rval, *lval) == false) {
    classtable->semant_error(curclass) << "Error! assign rval not child\n";
    type = Object;
  }

  return type;
}

Symbol attr_class::matchFormals(Expressions e) { return No_type; }
Symbol method_class::matchFormals(Expressions actual) {

  Symbol type = return_type;
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    Symbol actual_type = actual->nth(i)->CheckExprType();
    if (formals->more(i) == false) {
      return Object;
    }
    Symbol declar_type = formals->nth(i)->get_typedecl();
    if (classtable->isSubtype(actual_type, declar_type) == false) {
      classtable->semant_error(curclass) << "Error! unmatched formal type\n";
      type = Object;
    }
  }
  return type;
}

Symbol static_dispatch_class::CheckExprType() {
  log << "\t static dispatch\n";
  Symbol expr_type = expr->CheckExprType();

  if (classtable->isSubtype(expr_type, type_name) == false) {
    classtable->semant_error(curclass)
        << "Expression type " << expr_type
        << " does not conform to declared static dispatch type " << type_name
        << std::endl;
    type = Object;
    return type;
  }

  Symbol ancestor = classtable->findMethodInAncestor(type_name, name);

  if (ancestor == No_class) {
    classtable->semant_error(curclass)
        << "Error! cannot find method " << name << std::endl;
    type = Object;
    return type;
  }

  Feature_class *method = methodEnv[ancestor].lookup(name);

  type = method->matchFormals(actual);

  if (type == SELF_TYPE)
    type = expr_type;
  return type;
}

Symbol dispatch_class::CheckExprType() {
  log << "\t dispatch\n";

  Symbol expr_type = expr->CheckExprType();

  Symbol ancestor = classtable->findMethodInAncestor(expr_type, name);

  if (ancestor == No_class) {
    classtable->semant_error(curclass)
        << "Error! cannot find method " << name << std::endl;
    return Object;
  }

  Feature_class *method = methodEnv[ancestor].lookup(name);

  type = method->matchFormals(actual);

  if (type == SELF_TYPE)
    type = expr_type;
  return type;
}

Symbol cond_class::CheckExprType() {
  log << "\t cond(if else)\n";

  Symbol pred_type = pred->CheckExprType();
  if (pred_type != Bool)
    classtable->semant_error(curclass)
        << "Error! condition has non-bool predicate\n";

  Symbol then_type = then_exp->CheckExprType();
  Symbol else_type = else_exp->CheckExprType();
  type = then_type;
  if (else_type != No_type)
    type = classtable->CommonAncestor(then_type, else_type);
  return type;
}

Symbol loop_class::CheckExprType() {
  log << "\t loop \n";

  type = pred->CheckExprType();
  if (type != Bool) {
    classtable->semant_error(curclass) << "Error! loop pred\n";
  }
  body->CheckExprType();
  type = Object;
  return type;
}

Symbol typcase_class::CheckExprType() {
  log << "\t cases \n";

  expr->CheckExprType();

  std::set<Symbol> s;

  int i = cases->first();
  type = cases->nth(i)->CheckBranchType();
  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    Case oneBranch = cases->nth(i);

    if (s.find(oneBranch->get_typedecl()) != s.end()) {
      classtable->semant_error(curclass) << "Error! branch has same type\n";
    }
    s.insert(oneBranch->get_typedecl());

    Symbol branch_type = oneBranch->CheckBranchType();
    type = classtable->CommonAncestor(type, branch_type);
  }
  return type;
}

Symbol branch_class::CheckBranchType() {
  log << "\t branch\n";

  objectEnv.enterscope();
  objectEnv.addid(name, new Symbol(type_decl));

  Symbol type = expr->CheckExprType();

  objectEnv.exitscope();
  return type;
}

Symbol block_class::CheckExprType() {
  log << "\t block\n";
  type = Object;
  for (int i = body->first(); body->more(i); i = body->next(i)) {
    Expression e = body->nth(i);
    type = e->CheckExprType();
  }
  return type;
}

Symbol let_class::CheckExprType() {
  log << "\t let\n";

  objectEnv.enterscope();
  if (identifier == self)
    classtable->semant_error(curclass)
        << "'self' cannot be bound in a 'let' expression\n";
  objectEnv.addid(identifier, new Symbol(type_decl));

  Symbol ini_type = init->CheckExprType();
  type = body->CheckExprType();

  if (ini_type != No_type)
    if (classtable->isSubtype(ini_type, type_decl) == false)
      classtable->semant_error(curclass) << "Error! let init val not child\n";
  objectEnv.exitscope();

  return type;
}

Symbol plus_class::CheckExprType() {
  log << "\t plus\n";

  Symbol lval = e1->CheckExprType();
  Symbol rval = e2->CheckExprType();

  type = Int;
  if (lval != Int || rval != Int) {
    classtable->semant_error(curclass) << "Error! plus has non-Int arg\n";
    type = Object;
  }
  return type;
}

Symbol sub_class::CheckExprType() {
  log << "\t sub\n";

  Symbol lval = e1->CheckExprType();
  Symbol rval = e2->CheckExprType();

  type = Int;
  if (lval != Int || rval != Int) {
    classtable->semant_error(curclass) << "Error! sub has non-Int arg\n";
    type = Object;
  }
  return type;
}

Symbol mul_class::CheckExprType() {
  log << "\t mul\n";

  Symbol lval = e1->CheckExprType();
  Symbol rval = e2->CheckExprType();

  type = Int;
  if (lval != Int || rval != Int) {
    classtable->semant_error(curclass)
        << "Error! multiplication has non-Int arg\n";
    type = Object;
  }
  return type;
}

Symbol divide_class::CheckExprType() {
  log << "\t divide\n";

  Symbol lval = e1->CheckExprType();
  Symbol rval = e2->CheckExprType();

  type = Int;
  if (lval != Int || rval != Int) {
    classtable->semant_error(curclass) << "Error! divide has non-Int arg\n";
    type = Object;
  }
  return type;
}

Symbol neg_class::CheckExprType() {
  log << "\t neg\n";

  type = e1->CheckExprType();
  if (type != Int) {
    classtable->semant_error(curclass) << "Error! neg type not int\n";
    type = Object;
  }
  return type;
}

Symbol lt_class::CheckExprType() {
  log << "\t lt\n";

  Symbol lval = e1->CheckExprType();
  Symbol rval = e2->CheckExprType();
  type = Bool;
  if (lval != Int || rval != Int) {
    classtable->semant_error(curclass) << "Error! lt has non-Int value\n";
    type = Object;
  }
  return type;
}

Symbol eq_class::CheckExprType() {
  log << "\t eq\n";

  Symbol lval = e1->CheckExprType();
  Symbol rval = e2->CheckExprType();

  type = Bool;
  if (classtable->isBasicClass(lval) || classtable->isBasicClass(rval))
    if (lval != rval) {
      classtable->semant_error(curclass) << "Error in equal class\n";
      type = Object;
    }

  return type;
}

Symbol leq_class::CheckExprType() {
  log << "\t leq\n";

  Symbol lval = e1->CheckExprType();
  Symbol rval = e2->CheckExprType();
  type = Bool;
  if (lval != Int || rval != Int) {
    classtable->semant_error(curclass) << "Error! leq has non-Int value\n";
    type = Object;
  }
  return type;
}

Symbol comp_class::CheckExprType() {
  log << "\t comp\n";

  Symbol e1val = e1->CheckExprType();
  type = Bool;
  if (e1val != Bool) {
    classtable->semant_error(curclass) << "Error! not has non-Bool value\n";
    type = Object;
  }
  return type;
}

Symbol int_const_class::CheckExprType() {
  log << "\t int_const\n";
  type = Int;
  return type;
}
Symbol bool_const_class::CheckExprType() {
  log << "\t bool_const\n";
  type = Bool;
  return type;
}
Symbol string_const_class::CheckExprType() {
  log << "\t str_const\n";
  type = Str;
  return type;
}

Symbol new__class::CheckExprType() {
  log << "\t new_class \n";
  type = type_name;
  if (type != SELF_TYPE && classtable->getClassByName(type_name) == NULL) {
    classtable->semant_error(curclass)
        << " Undefined return type " << type_name << std::endl;
    type = Object;
  }
  return type;
}

Symbol isvoid_class::CheckExprType() {
  e1->CheckExprType();
  type = Bool;
  return type;
}

Symbol no_expr_class::CheckExprType() {
  type = No_type;
  return type;
}

Symbol object_class::CheckExprType() {
  log << "\t object class\n";

  Symbol *found_type = objectEnv.lookup(name);

  if (found_type == NULL) {
    classtable->semant_error(curclass)
        << "Undeclared identifier " << name << std::endl;
    type = Object;
  } else
    type = *found_type;

  return type;
}

/*
 * DFS as above
 */
void program_class::checkClassesType(Symbol classname) {

  curclass = classtable->getClassByName(classname);
  objectEnv.enterscope();
  objectEnv.addid(self, new Symbol(SELF_TYPE));
  Features fs = curclass->get_features();
  for (int i = fs->first(); fs->more(i); i = fs->next(i)) {
    Feature curFeature = fs->nth(i);
    curFeature->AddAttribute();
  }

  /*
   * start type checking under local object environment
   */
  if (!classtable->isBasicClass(classname)) {

    for (int i = fs->first(); fs->more(i); i = fs->next(i)) {
      Feature curFeature = fs->nth(i);
      curFeature->CheckFeatureType();
    }
  }

  std::list<Symbol>::iterator iter;
  std::list<Symbol> nexClass = classtable->getChildClasses(classname);
  for (iter = nexClass.begin(); iter != nexClass.end(); ++iter)
    checkClassesType(*iter);

  objectEnv.exitscope();
}

/*
 * DFS from Object to all child classes
 * use inheritEnv to record methods along current inheritance path
 */
void program_class::installMethods(Symbol classname) {

  curclass = classtable->getClassByName(classname);
  inheritEnv.enterscope();
  methodEnv[classname].enterscope();
  Features fs = curclass->get_features();
  for (int i = fs->first(); fs->more(i); i = fs->next(i)) {
    Feature curFeature = fs->nth(i);
    curFeature->AddMethod();
  }

  // dfs to children
  std::list<Symbol>::iterator iter;
  std::list<Symbol> nexClass = classtable->getChildClasses(classname);
  for (iter = nexClass.begin(); iter != nexClass.end(); ++iter)
    installMethods(*iter);

  inheritEnv.exitscope();
}

/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant() {
  initialize_constants();

  /* ClassTable constructor may do some semantic analysis */
  classtable = new ClassTable(classes);

  /* some semantic analysis code may go here */
  installMethods(Object);
  checkClassesType(Object);

  if (classtable->errors()) {
    cerr << "Compilation halted due to static semantic errors." << endl;
    exit(1);
  }
}
