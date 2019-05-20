#include "cool-tree.h"
#include "emit.h"
#include "symtab.h"
#include <assert.h>
#include <stdio.h>

#include <algorithm>
#include <map>
#include <stack>
#include <string>
#include <vector>

enum Basicness { Basic, NotBasic };
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol, CgenNode> {
private:
  List<CgenNode> *nds;
  ostream &str;
  int stringclasstag;
  int intclasstag;
  int boolclasstag;

  // The following methods emit code for
  // constants and global declarations.

  void code_global_data();
  void code_global_text();
  void code_bools(int);
  void code_select_gc();
  void code_constants();

  void code_class_nameTab();
  void code_class_objTab();
  void code_class_dispTab();
  void code_class_protObj();
  void code_class_init();
  void code_methods();

  // The following creates an inheritance graph from
  // a list of classes.  The graph is implemented as
  // a tree of `CgenNode', and class names are placed
  // in the base class symbol table.

  void install_basic_classes();
  void install_class(CgenNodeP nd);
  void install_classes(Classes cs);
  void build_inheritance_tree();
  void set_relations(CgenNodeP nd);

public:
  CgenClassTable(Classes, ostream &str);
  void code();
  void execute();
  CgenNodeP root();
};

class CgenNode : public class__class {
private:
  CgenNodeP parentnd;       // Parent of class
  List<CgenNode> *children; // Children of class
  Basicness basic_status;   // `Basic' if class is basic
                            // `NotBasic' otherwise

public:
  int tag;
  std::map<Symbol, int> attr_index;
  std::map<Symbol, int> method_index;

  void code_nameTab(int &, ostream &);
  void code_objTab(ostream &);
  void code_protObj(std::vector<Symbol>, ostream &);
  void code_dispTab(std::vector<std::pair<Symbol, Symbol>>, ostream &);
  void code_init(ostream &);

  method_class *find_method(Symbol);
  void install_args(Symbol);

  CgenNode(Class_ c, Basicness bstatus, CgenClassTableP class_table);

  void add_child(CgenNodeP child);
  List<CgenNode> *get_children() { return children; }
  void set_parentnd(CgenNodeP p);
  CgenNodeP get_parentnd() { return parentnd; }
  int basic() { return (basic_status == Basic); }
};

class BoolConst {
private:
  int val;

public:
  BoolConst(int);
  void code_def(ostream &, int boolclasstag);
  void code_ref(ostream &) const;
};

class Environment {
public:
  int args_size = 0, var_scopes = 0;
  std::map<Symbol, int> attr_index;
  SymbolTable<Symbol, int> args_index;
  SymbolTable<Symbol, int> var_index;

  Environment() {
    args_index.enterscope();
    var_index.enterscope();
  }

  void add_Attribute(CgenNodeP p) { attr_index = p->attr_index; }

  void add_Argument(Symbol s) {
    // args_index.insert(std::make_pair(s, args_index.size()));
    args_index.addid(s, new int(args_size));
    args_size++;
  }

  void add_Variable(Symbol s, int id) { var_index.addid(s, new int(id)); }

  //-----------------------------------------

  int get_Attribute(Symbol s) {
    if (attr_index.find(s) == attr_index.end())
      return -1;
    return attr_index[s];
  }

  int get_Argument(Symbol s) {
    if (args_index.probe(s) == NULL)
      return -1;
    return args_size - *args_index.probe(s) - 1;
  }

  int get_Var(Symbol s) {
    if (var_index.lookup(s) == NULL)
      return -1;
    return *var_index.lookup(s);
  }

  void args_enter() { args_index.enterscope(); }
  void vars_enter() {
    var_index.enterscope();
    var_scopes++;
  }

  void args_exit() {
    args_index.exitscope();
    args_size = 0;
  }

  void vars_exit() { var_index.exitscope(); }

  void vars_dump() {
    while (var_scopes > 1) {
      vars_exit();
      var_scopes--;
    }
  }
};
