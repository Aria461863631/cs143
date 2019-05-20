
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"

extern void emit_string_constant(ostream &str, char *s);
extern int cgen_debug;

int label_num = 0, nodesize = 0;
int temp_size, used_temp = 0;

std::map<Symbol, int> class_tags;
Symbol current_class;
Environment env;
CgenClassTable *codegen_classtable = nullptr;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol arg, arg2, Bool, concat, cool_abort, copy, Int, in_int, in_string, IO,
    length, Main, main_meth, No_class, No_type, Object, out_int, out_string,
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

static char *gc_init_names[] = {"_NoGC_Init", "_GenGC_Init", "_ScnGC_Init"};
static char *gc_collect_names[] = {"_NoGC_Collect", "_GenGC_Collect",
                                   "_ScnGC_Collect"};

//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) {
  // spim wants comments to start with '#'
  os << "# start of generated code\n";
  cgen_debug = 0;

  initialize_constants();
  codegen_classtable = new CgenClassTable(classes, os);
  codegen_classtable->execute();

  os << "\n# end of generated code\n";
}

//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////
static void emit_load(char *dest_reg, int offset, char *source_reg,
                      ostream &s) {
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")"
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg,
                       ostream &s) {
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
    << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream &s) {
  s << LI << dest_reg << " " << val << endl;
}

static void emit_load_address(char *dest_reg, char *address, ostream &s) {
  s << LA << dest_reg << " " << address << endl;
}

static void emit_partial_load_address(char *dest_reg, ostream &s) {
  s << LA << dest_reg << " ";
}

static void emit_load_bool(char *dest, const BoolConst &b, ostream &s) {
  emit_partial_load_address(dest, s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream &s) {
  emit_partial_load_address(dest, s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream &s) {
  emit_partial_load_address(dest, s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream &s) {
  s << MOVE << dest_reg << " " << source_reg << endl;
}

static void emit_neg(char *dest, char *src1, ostream &s) {
  s << NEG << dest << " " << src1 << endl;
}

static void emit_add(char *dest, char *src1, char *src2, ostream &s) {
  s << ADD << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addu(char *dest, char *src1, char *src2, ostream &s) {
  s << ADDU << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addiu(char *dest, char *src1, int imm, ostream &s) {
  s << ADDIU << dest << " " << src1 << " " << imm << endl;
}

static void emit_div(char *dest, char *src1, char *src2, ostream &s) {
  s << DIV << dest << " " << src1 << " " << src2 << endl;
}

static void emit_mul(char *dest, char *src1, char *src2, ostream &s) {
  s << MUL << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sub(char *dest, char *src1, char *src2, ostream &s) {
  s << SUB << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sll(char *dest, char *src1, int num, ostream &s) {
  s << SLL << dest << " " << src1 << " " << num << endl;
}

static void emit_jalr(char *dest, ostream &s) {
  s << JALR << "\t" << dest << endl;
}

static void emit_jal(char *address, ostream &s) { s << JAL << address << endl; }

static void emit_return(ostream &s) { s << RET << endl; }

static void emit_gc_assign(ostream &s) { s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream &s) {
  s << sym << DISPTAB_SUFFIX;
}

static void emit_init_ref(Symbol sym, ostream &s) {
  s << sym << CLASSINIT_SUFFIX;
}

static void emit_label_ref(int l, ostream &s) { s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream &s) {
  s << sym << PROTOBJ_SUFFIX;
}

static void emit_method_ref(Symbol classname, Symbol methodname, ostream &s) {
  s << classname << METHOD_SEP << methodname;
}

static void emit_label_def(int l, ostream &s) {
  emit_label_ref(l, s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s) {
  s << BEQZ << source << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s) {
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s) {
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s) {
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s) {
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s) {
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s) {
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_branch(int l, ostream &s) {
  s << BRANCH;
  emit_label_ref(l, s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream &str) {
  emit_store(reg, 0, SP, str);
  emit_addiu(SP, SP, -4, str);
}

static void emit_pop(char *reg, ostream &str) {
  emit_load(reg, 1, SP, str);
  emit_addiu(SP, SP, 4, str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream &s) {
  emit_load(dest, DEFAULT_OBJFIELDS, source, s);
}

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream &s) {
  emit_store(source, DEFAULT_OBJFIELDS, dest, s);
}

static void emit_test_collector(ostream &s) {
  emit_push(ACC, s);
  emit_move(ACC, SP, s);  // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP, SP, 4, s);
  emit_load(ACC, 0, SP, s);
}

static void emit_gc_check(char *source, ostream &s) {
  if (source != (char *)A1)
    emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}

//--------------------------------------------------------------------

static void emit_push_stack(int args, ostream &s) {
  emit_addiu(SP, SP, -12 - 4 * temp_size, s);
  s << "\t# push fp,s0,ra" << endl;
  emit_store(FP, 3 + temp_size, SP, s);
  emit_store(SELF, 2 + temp_size, SP, s);
  emit_store(RA, 1 + temp_size, SP, s);

  s << "\t# fp point to return addr" << endl;
  emit_addiu(FP, SP, 4, s);
  s << "\t# a0 has the current self" << endl;
  emit_move(SELF, ACC, s);
}

static void emit_pop_stack(int args, ostream &s) {
  emit_load(FP, 3 + temp_size, SP, s);
  emit_load(SELF, 2 + temp_size, SP, s);
  emit_load(RA, 1 + temp_size, SP, s);
  emit_addiu(SP, SP, 12 + 4 * args, s);
  emit_return(s);
}

static void emit_push_temp(char *source_reg, ostream &s) {
  emit_store(source_reg, used_temp++, FP, s);

  if (cgen_debug)
    if (used_temp > temp_size) {
      cout << "Stack overflow!!!!!!!!!!!!!!!!!" << endl;
      s << "# stack overflow !!!!!!!!!!!!!!!!!!!!\n";
    }
}

static void emit_pop_temp(char *dest_reg, ostream &s) {
  emit_load(dest_reg, --used_temp, FP, s);

  if (cgen_debug)
    if (used_temp < 0)
      cout << "Stack emmmm flow!!!!!!!!!!!!!!!!!" << endl;
}

///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream &s) { s << STRCONST_PREFIX << index; }

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream &s, int stringclasstag) {
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);
  s << LABEL                          // label
    << WORD << stringclasstag << endl // tag
    << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len + 4) / 4)
    << endl // size
    << WORD;

  /***** Add dispatch information for class String ******/
  emit_disptable_ref(Str, s);

  s << endl; // dispatch table
  s << WORD;
  lensym->code_ref(s);
  s << endl;                    // string length
  emit_string_constant(s, str); // ascii string
  s << ALIGN;                   // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the
// stringtable.
//
void StrTable::code_string_table(ostream &s, int stringclasstag) {
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s, stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s) { s << INTCONST_PREFIX << index; }

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag) {
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);
  s << LABEL                                           // label
    << WORD << intclasstag << endl                     // class tag
    << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl // object size
    << WORD;

  /***** Add dispatch information for class Int ******/
  emit_disptable_ref(Int, s);

  s << endl;                // dispatch table
  s << WORD << str << endl; // integer value
}

//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag) {
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s, intclasstag);
}

//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream &s) const { s << BOOLCONST_PREFIX << val; }

//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream &s, int boolclasstag) {
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);
  s << LABEL                                            // label
    << WORD << boolclasstag << endl                     // class tag
    << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl // object size
    << WORD;

  /***** Add dispatch information for class Bool ******/
  emit_disptable_ref(Bool, s);

  s << endl;                // dispatch table
  s << WORD << val << endl; // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data() {
  Symbol main = idtable.lookup_string(MAINNAME);
  Symbol string = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL;
  emit_protobj_ref(main, str);
  str << endl;
  str << GLOBAL;
  emit_protobj_ref(integer, str);
  str << endl;
  str << GLOBAL;
  emit_protobj_ref(string, str);
  str << endl;
  str << GLOBAL;
  falsebool.code_ref(str);
  str << endl;
  str << GLOBAL;
  truebool.code_ref(str);
  str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL << WORD << stringclasstag << endl;
}

//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text() {
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"), str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag) {
  falsebool.code_def(str, boolclasstag);
  truebool.code_def(str, boolclasstag);
}

void CgenClassTable::code_select_gc() {
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}

//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants() {
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str, stringclasstag);
  inttable.code_string_table(str, intclasstag);
  code_bools(boolclasstag);
}

void CgenNode::code_nameTab(int &nodesize, ostream &s) {
  class_tags.insert(std::make_pair(name, nodesize));
  tag = nodesize++;
  s << WORD;
  stringtable.lookup_string(name->get_string())->code_ref(s);
  s << endl;

  for (List<CgenNode> *l = children; l; l = l->tl())
    l->hd()->code_nameTab(nodesize, s);
}

void CgenClassTable::code_class_nameTab() {
  str << CLASSNAMETAB << LABEL;
  int nodesize = 0;
  root()->code_nameTab(nodesize, str);
}

void CgenNode::code_objTab(ostream &s) {
  StringEntry *e = stringtable.lookup_string(name->get_string());
  s << WORD;
  emit_protobj_ref(e, s);
  s << endl << WORD;
  emit_init_ref(e, s);
  s << endl;

  for (List<CgenNode> *l = children; l; l = l->tl())
    l->hd()->code_objTab(s);
}

void CgenClassTable::code_class_objTab() {
  str << CLASSOBJTAB << LABEL;
  root()->code_objTab(str);
}

void CgenNode::code_protObj(std::vector<Symbol> mp, ostream &s) {

  attr_index = get_parentnd()->attr_index;
  int a_size = attr_index.size();

  for (int i = features->first(); features->more(i); i = features->next(i)) {
    if (!features->nth(i)->is_attr())
      continue;

    attr_class *a = (attr_class *)features->nth(i);
    attr_index.insert(std::make_pair(a->name, a_size++));
    mp.push_back(a->type_decl);
  }

  s << WORD << "-1" << endl;
  emit_protobj_ref(name, s);
  s << LABEL;
  s << WORD << tag << endl << WORD << (DEFAULT_OBJFIELDS + a_size) << endl;
  s << WORD;
  emit_disptable_ref(name, s);
  s << endl;

  if (name == Int || name == Bool) {
    s << WORD << "0" << endl;
  } else if (name == Str) {
    s << WORD;
    inttable.lookup_string("0")->code_ref(s);
    s << endl << WORD << "0" << endl;

  } else {
    for (int i = 0; i < (int)mp.size(); ++i) {
      Symbol type = mp[i];
      if (type == Int) {
        s << WORD;
        inttable.lookup_string("0")->code_ref(s);
        s << "\t# int(0)" << endl;
      } else if (type == Bool) {
        s << WORD;
        falsebool.code_ref(s);
        s << "\t# bool(0)" << endl;
      } else if (type == Str) {
        s << WORD;
        stringtable.lookup_string("")->code_ref(s);
        s << "\t# s(\"\")" << endl;
      } else {
        s << WORD << "0\t# void" << endl;
      }
    }
  }

  for (List<CgenNode> *l = children; l; l = l->tl())
    l->hd()->code_protObj(mp, s);
}

void CgenClassTable::code_class_protObj() {
  std::vector<Symbol> mp;
  root()->code_protObj(mp, str);
}

//----------------
void CgenNode::code_dispTab(std::vector<std::pair<Symbol, Symbol>> mp,
                            ostream &s) {

  emit_disptable_ref(name, s);
  s << LABEL;

  method_index = get_parentnd()->method_index;
  int m_size = method_index.size();

  for (int i = features->first(); features->more(i); i = features->next(i)) {
    if (!features->nth(i)->is_method())
      continue;

    method_class *m = (method_class *)features->nth(i);
    Symbol method_name = m->name;
    if (method_index.find(method_name) == method_index.end()) {
      method_index.insert(std::make_pair(method_name, m_size++));
      mp.push_back(std::make_pair(method_name, name));
    } else {
      // override
      int index = method_index[method_name];
      mp[index].second = name;
    }
  }

  for (int i = 0; i < (int)mp.size(); ++i) {
    s << WORD;
    emit_method_ref(mp[i].second, mp[i].first, s);
    s << endl;
  }

  for (List<CgenNode> *l = children; l; l = l->tl())
    l->hd()->code_dispTab(mp, s);
}

void CgenClassTable::code_class_dispTab() {
  std::vector<std::pair<Symbol, Symbol>> mp;
  root()->code_dispTab(mp, str);
}

// init-----------
void CgenNode::code_init(ostream &s) {
  current_class = name;
  env.add_Attribute(this);
  if (cgen_debug)
    cout << "current class is " << name << endl;

  temp_size = 0;
  for (int i = features->first(); features->more(i); i = features->next(i)) {
    if (!features->nth(i)->is_attr())
      continue;
    attr_class *a = (attr_class *)features->nth(i);
    temp_size = std::max(temp_size, a->tmp_Num());
  }
  if (cgen_debug)
    cout << "temp size " << temp_size << endl;
  used_temp = 0;

  emit_init_ref(name, s);
  s << LABEL;

  emit_push_stack(temp_size, s);

  Symbol parent_name = parentnd->name;
  if (parent_name != No_class) {
    s << JAL;
    emit_init_ref(parent_name, s);
  }

  for (int i = features->first(); features->more(i); i = features->next(i)) {
    if (!features->nth(i)->is_attr())
      continue;
    attr_class *a = (attr_class *)features->nth(i);

    if (a->init->isempty()) {
    } else {
      s << "\t# initialize argument " << a->name << endl;
      a->init->code(s);

      int indx = attr_index[a->name];
      emit_store(ACC, DEFAULT_OBJFIELDS + indx, SELF, s);
      if (cgen_Memmgr == 1) {
        emit_addiu(A1, SELF, 4 * (indx + DEFAULT_OBJFIELDS), s);
        emit_gc_assign(s);
      }
    }
  }

  s << "\t# return self" << endl;
  emit_move(ACC, SELF, s);
  emit_pop_stack(temp_size, s);

  for (List<CgenNode> *l = children; l; l = l->tl())
    l->hd()->code_init(s);
}

void CgenClassTable::code_class_init() { root()->code_init(str); }

// methods-----------------------

void CgenClassTable::code_methods() {
  for (List<CgenNode> *l = nds; l; l = l->tl()) {
    CgenNodeP cur = l->hd();
    env.add_Attribute(cur);
    current_class = cur->name;
    if (cgen_debug)
      cout << "current class is " << current_class << endl;

    if (cur->basic() == true)
      continue;

    Features f = cur->features;
    for (int i = f->first(); f->more(i); i = f->next(i))
      if (f->nth(i)->is_method()) {
        str << cur->name << METHOD_SEP;
        f->nth(i)->code(str);
      }
  }
}

//--------------------------------------------------------------------------

CgenClassTable::CgenClassTable(Classes classes, ostream &s)
    : nds(NULL), str(s) {
  stringclasstag = 3 /* Change to your String class tag here */;
  intclasstag = 1 /* Change to your Int class tag here */;
  boolclasstag = 2 /* Change to your Bool class tag here */;

  enterscope();
  if (cgen_debug)
    cout << "Building CgenClassTable" << endl;
  install_basic_classes();
  install_classes(classes);
  build_inheritance_tree();

  code();
}

void CgenClassTable::install_basic_classes() {

  // The tree package uses these globals to annotate the classes built below.
  // curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

  //
  // A few special class names are installed in the lookup table but not
  // the class list.  Thus, these classes exist, but are not part of the
  // inheritance hierarchy.
  // No_class serves as the parent of Object and the other special classes.
  // SELF_TYPE is the self class; it cannot be redefined or inherited.
  // prim_slot is a class known to the code generator.
  //
  addid(No_class,
        new CgenNode(class_(No_class, No_class, nil_Features(), filename),
                     Basic, this));
  addid(SELF_TYPE,
        new CgenNode(class_(SELF_TYPE, No_class, nil_Features(), filename),
                     Basic, this));
  addid(prim_slot,
        new CgenNode(class_(prim_slot, No_class, nil_Features(), filename),
                     Basic, this));

  //
  // The Object class has no parent class. Its methods are
  //        cool_abort() : Object    aborts the program
  //        type_name() : Str        returns a string representation of class
  //        name copy() : SELF_TYPE       returns a copy of the object
  //
  // There is no need for method bodies in the basic classes---these
  // are already built in to the runtime system.
  //
  //
  install_class(new CgenNode(
      class_(
          Object, No_class,
          append_Features(
              append_Features(single_Features(method(cool_abort, nil_Formals(),
                                                     Object, no_expr())),
                              single_Features(method(type_name, nil_Formals(),
                                                     Str, no_expr()))),
              single_Features(
                  method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
          filename),
      Basic, this));

  //
  // The Int class has no methods and only a single attribute, the
  // "val" for the integer.
  //
  install_class(new CgenNode(
      class_(Int, Object, single_Features(attr(val, prim_slot, no_expr())),
             filename),
      Basic, this));

  //
  // Bool also has only the "val" slot.
  //
  install_class(new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),
             filename),
      Basic, this));

  //
  // The class Str has a number of slots and operations:
  //       val                                  ???
  //       str_field                            the string itself
  //       length() : Int                       length of the string
  //       concat(arg: Str) : Str               string concatenation
  //       substr(arg: Int, arg2: Int): Str     substring
  //
  install_class(new CgenNode(
      class_(Str, Object,
             append_Features(
                 append_Features(
                     append_Features(
                         append_Features(
                             single_Features(attr(val, Int, no_expr())),
                             single_Features(
                                 attr(str_field, prim_slot, no_expr()))),
                         single_Features(
                             method(length, nil_Formals(), Int, no_expr()))),
                     single_Features(method(concat,
                                            single_Formals(formal(arg, Str)),
                                            Str, no_expr()))),
                 single_Features(
                     method(substr,
                            append_Formals(single_Formals(formal(arg, Int)),
                                           single_Formals(formal(arg2, Int))),
                            Str, no_expr()))),
             filename),
      Basic, this));

  // The IO class inherits from Object. Its methods are
  //        out_string(Str) : SELF_TYPE          writes a string to the output
  //        out_int(Int) : SELF_TYPE               "    an int    "  "     "
  //        in_string() : Str                    reads a string from the input
  //        in_int() : Int                         "   an int     "  "     "
  //
  install_class(new CgenNode(
      class_(
          IO, Object,
          append_Features(
              append_Features(
                  append_Features(
                      single_Features(method(out_string,
                                             single_Formals(formal(arg, Str)),
                                             SELF_TYPE, no_expr())),
                      single_Features(method(out_int,
                                             single_Formals(formal(arg, Int)),
                                             SELF_TYPE, no_expr()))),
                  single_Features(
                      method(in_string, nil_Formals(), Str, no_expr()))),
              single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
          filename),
      Basic, this));

  //
}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd) {
  Symbol name = nd->get_name();

  if (probe(name)) {
    return;
  }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd, nds);
  addid(name, nd);
}

void CgenClassTable::install_classes(Classes cs) {
  for (int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i), NotBasic, this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree() {
  for (List<CgenNode> *l = nds; l; l = l->tl()) {
    set_relations(l->hd());
    // class_tags.insert(std::make_pair(l->hd()->name, nodesize++));
  }
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd) {
  // nd->tag = nodesize;
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n) {
  children = new List<CgenNode>(n, children);
}

void CgenNode::set_parentnd(CgenNodeP p) {
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}

void CgenClassTable::code() {
  str << "# coding global data" << endl;
  code_global_data();

  str << "# choosing gc" << endl;
  code_select_gc();

  str << "# coding constants" << endl;
  code_constants();

  //                 Add your code to emit
  //                   - prototype objects
  //                   - class_nameTab
  //                   - dispatch tables
  //
  str << "# coding class name table" << endl;
  code_class_nameTab();
  str << "# coding class obj table" << endl;
  code_class_objTab();
  str << "# coding dispatch tables" << endl;
  code_class_dispTab();
  str << "# coding prototype object" << endl;
  code_class_protObj();

  str << "# coding global text" << endl;
  code_global_text();
}

void CgenClassTable::execute() {
  //                 Add your code to emit
  //                   - object initializer
  //                   - the class methods
  //                   - etc...
  str << "# coding init" << endl;
  code_class_init();
  str << "# code method" << endl;
  code_methods();

  exitscope();
}

CgenNodeP CgenClassTable::root() { return probe(Object); }

///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct)
    : class__class((const class__class &)*nd), parentnd(NULL), children(NULL),
      basic_status(bstatus) {
  stringtable.add_string(name->get_string()); // Add class name to string table
}

method_class *CgenNode::find_method(Symbol method_name) {
  if (name == No_class)
    return NULL;

  for (int i = features->first(); features->more(i); i = features->next(i)) {
    if (!features->nth(i)->is_method())
      continue;
    method_class *tmp = (method_class *)features->nth(i);
    if (tmp->name == method_name)
      return tmp;
  }

  return parentnd->find_method(method_name);
}

void method_class::install_args() {
  for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
    formal_class *f = (formal_class *)formals->nth(i);
    env.add_Argument(f->name);
    if (cgen_debug)
      cout << "\tinstall arg " << f->name << endl;
  }
}

//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void method_class::code(ostream &s) {
  if (cgen_debug)
    cout << "start method " << name << endl;
  used_temp = 0;
  temp_size = tmp_Num();
  if (cgen_debug)
    cout << "temps: " << temp_size << endl << endl;
  s << name << LABEL;
  emit_push_stack(temp_size, s);

  if (cgen_debug)
    cout << "args enter scope\n";
  env.args_enter();
  install_args();

  expr->code(s);

  env.args_exit();
  if (cgen_debug)
    cout << "args exit scope\n";

  int formal_size = formals->len();
  emit_pop_stack(formal_size + temp_size, s);
}

void attr_class::code(ostream &s) {}

void assign_class::code(ostream &s) {
  s << "# assign expr\n";
  expr->code(s);

  s << "\t# look up lval addr\n";
  int indx;
  if ((indx = env.get_Var(name)) != -1) {
    s << "\t# find in " << indx << " position in let variables\n";
    emit_store(ACC, indx, FP, s);
    if (cgen_Memmgr == 1) {
      emit_addiu(A1, FP, 4 * indx, s);
      emit_gc_assign(s);
    }

  } else if ((indx = env.get_Argument(name)) != -1) {
    s << "\t# find in " << indx << " position in argument\n";
    emit_store(ACC, DEFAULT_FPFIELDS + temp_size + indx, FP, s);
    if (cgen_Memmgr == 1) {
      emit_addiu(A1, FP, 4 * (DEFAULT_FPFIELDS + temp_size + indx), s);
      emit_gc_assign(s);
    }

  } else if ((indx = env.get_Attribute(name)) != -1) {
    s << "\t# find in " << indx << " position in attribute\n";
    emit_store(ACC, DEFAULT_OBJFIELDS + indx, SELF, s);
    if (cgen_Memmgr == 1) {
      emit_addiu(A1, SELF, 4 * (DEFAULT_OBJFIELDS + indx), s);
      emit_gc_assign(s);
    }

  } else {
    s << "\t#bad assign! ERRORRRRRRRRRRRRRRRRRRRRRRRR\n";
    if (cgen_debug)
      cout << "\t#bad assign! ERRORRRRRRRRRRRRRRRRRRRRRRRR\n";
  }
}

void static_dispatch_class::code(ostream &s) {
  s << "# static dispatch exp " << name << endl;

  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->code(s);
    s << "\t# push arg on stack\n";
    emit_push(ACC, s);
  }

  s << "\t# then eval e0 and save in a0\n";
  expr->code(s);

  s << "\t# if e0 is void \n";
  int e0_not_zero = label_num++;
  emit_bne(ACC, ZERO, e0_not_zero, s);
  s << "\t# abort\n\t#loading filename to a0 and linenumber to t1\n";
  s << LA << ACC << " str_const0" << endl;
  emit_load_imm(T1, 1, s);
  emit_jal("_dispatch_abort", s);

  s << "\t# continue dispatching\n";
  emit_label_def(e0_not_zero, s);

  Symbol old_class = current_class;
  current_class = type_name;
  // get static dispatch class
  CgenNodeP c = codegen_classtable->probe(current_class);

  int indx = c->method_index[name];

  s << "\t# locate method " << name << " of class " << type_name << endl;
  s << "\t# get " << type_name << "''s dispatch table\n";
  s << LA << T1 << " " << type_name << DISPTAB_SUFFIX << endl;

  s << "\t# find method in index " << indx << endl;
  emit_load(T1, indx, T1, s);

  current_class = old_class;

  s << "\t# jump to method " << name << endl;
  emit_jalr(T1, s);
}

void dispatch_class::code(ostream &s) {
  s << "# dispatch exp " << name << "\n";

  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->code(s);
    s << "\t# push arg on stack\n";
    emit_push(ACC, s);
  }

  s << "\t# then eval e0 and save in a0\n";
  expr->code(s);

  s << "\t# if e0 is void \n";
  int e0_not_zero = label_num++;
  emit_bne(ACC, ZERO, e0_not_zero, s);
  s << "\t# abort\n\t#loading filename to a0 and linenumber to t1\n";
  s << LA << ACC << " str_const0" << endl;
  emit_load_imm(T1, 1, s);
  emit_jal("_dispatch_abort", s);

  s << "\t# continue dispatching\n";
  emit_label_def(e0_not_zero, s);

  //------------------------

  Symbol old_class = current_class;

  if (expr->get_type() != SELF_TYPE)
    current_class = expr->get_type();
  CgenNodeP c = codegen_classtable->probe(current_class);
  if (cgen_debug)
    cout << "dispatch class " << current_class << endl;

  int indx = c->method_index[name];

  s << "\t# locate method " << name << " of class " << current_class << endl;
  s << "\t# get self's dispatch table\n";
  emit_load(T1, 2, ACC, s);

  s << "\t# find method in index " << indx << endl;
  emit_load(T1, indx, T1, s);

  current_class = old_class;

  s << "\t# jump to method " << name << endl;
  emit_jalr(T1, s);
}

void cond_class::code(ostream &s) {
  s << "# if_else exp\n";
  s << "\t# eval predicate\n";
  pred->code(s);
  s << "\t# extract bool value of predicate to T1\n";
  emit_fetch_int(T1, ACC, s);

  int false_label = label_num++;
  int finish_label = label_num++;
  s << "\t# if false, goto else\n";
  emit_beq(T1, ZERO, false_label, s);

  s << "\t# coding then exp\n";
  then_exp->code(s);
  s << "\t# jump to finish label\n";
  emit_branch(finish_label, s);

  s << "\t# coding else exp\n";
  emit_label_def(false_label, s);
  else_exp->code(s);

  s << "\t# finish\n";
  emit_label_def(finish_label, s);
}

void loop_class::code(ostream &s) {
  int start_label = label_num++;
  int finish_label = label_num++;

  s << "# while exp\n";
  emit_label_def(start_label, s);
  s << "\t# eval predicate\n";
  pred->code(s);
  s << "\t# fetch predicate result\n";
  emit_fetch_int(T1, ACC, s);
  s << "\t# if false => finish\n";
  emit_beqz(T1, finish_label, s);

  s << "\t# eval body\n";
  body->code(s);
  s << "\t# jump to pred\n";
  emit_branch(start_label, s);
  s << "\t# while finish\n";
  emit_label_def(finish_label, s);

  s << "\t# while result is void\n";
  emit_move(ACC, ZERO, s);
}

int branch_class::get_child_tag() {
  int tag = class_tags[type_decl];

  CgenNodeP c = codegen_classtable->probe(type_decl);
  while (c->get_children() != NULL) {
    for (List<CgenNode> *l = c->get_children(); l; l = l->tl()) {
      c = l->hd();
      tag = std::max(tag, class_tags[c->name]);
    }
  }
  return tag;
}

void typcase_class::code(ostream &s) {
  s << "# typcase expr\n";
  int finish_label = label_num++;

  s << "\t# eval e0\n";
  expr->code(s);
  s << "\t# save expr on temp\n";
  emit_push_temp(ACC, s);

  s << "\t# abort if void\n";
  emit_bne(ACC, ZERO, label_num, s);
  emit_load_address(ACC, "str_const0", s);
  emit_load_imm(T1, 1, s);
  emit_jal("_case_abort2", s);
  emit_label_def(label_num++, s);

  s << "\t# T2 = class tag of e0\n";
  emit_load(T2, 0, ACC, s);

  //-------------------------------
  //计算所有case 的tag
  //计算所有case的孩子最大tag
  //按照大到小的顺序排序,输出
  std::vector<std::pair<int, int>> case_tag;
  std::map<int, Expression> mp;
  env.vars_enter();
  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    branch_class *b = (branch_class *)cases->nth(i);

    //局部变量,let覆盖?
    env.add_Variable(b->name, used_temp - 1);
    int fs = class_tags[b->type_decl];
    int se = b->get_child_tag();
    if (cgen_debug)
      cout << "[ " << fs << " , " << se << " ]\n";
    case_tag.push_back(std::make_pair(fs, se));
    mp.insert(std::make_pair(fs, b->expr));
  }

  auto sortbysec = [&](const std::pair<int, int> &a,
                       const std::pair<int, int> &b) {
    return (a.first > b.first);
  };

  std::sort(case_tag.begin(), case_tag.end(), sortbysec);

  for (int i = 0; i < (int)case_tag.size(); ++i) {
    int fs = case_tag[i].first, se = case_tag[i].second;
    int next_case = label_num++;
    emit_blti(T2, fs, next_case, s);
    emit_bgti(T2, se, next_case, s);

    mp[fs]->code(s);

    emit_branch(finish_label, s);
    emit_label_def(next_case, s);
  }
  emit_jal("_case_abort", s);

  used_temp--;
  env.vars_exit();
  emit_label_def(finish_label, s);
}

void block_class::code(ostream &s) {
  s << "# block exp\n";
  for (int i = body->first(); body->more(i); i = body->next(i)) {
    body->nth(i)->code(s);
  }
}

void let_class::code(ostream &s) {
  s << "# let exp\n";
  s << "\t# first eval init exps\n";
  init->code(s);

  if (init->isempty()) {
    // We still need to deal with basic types.
    if (type_decl == Str) {
      emit_load_string(ACC, stringtable.lookup_string(""), s);
    } else if (type_decl == Int) {
      emit_load_int(ACC, inttable.lookup_string("0"), s);
    } else if (type_decl == Bool) {
      emit_load_bool(ACC, BoolConst(0), s);
    }
  }

  s << "\t# push let var " << identifier << " onto temp\n";
  // emit_push(ACC, s);
  emit_push_temp(ACC, s);
  env.vars_enter();
  env.add_Variable(identifier, used_temp - 1);
  if (cgen_debug)
    cout << "var enter scope\n";

  s << "\t# eval let body\n";
  body->code(s);

  env.vars_exit();
  if (cgen_debug)
    cout << "var exit scope\n";

  s << "\t# pop let var " << endl;
  s << "\t# used_temp--;" << endl;
  // emit_addiu(SP, SP, 4, s);
  used_temp--;
}

void plus_class::code(ostream &s) {
  s << "# Plus\n";
  s << "\t# start eval e1" << endl;
  e1->code(s);
  s << "\t# e1 save to temp var" << endl;
  emit_push_temp(ACC, s);

  s << "\t# start eval e2 and copy" << endl;
  e2->code(s);
  emit_jal("Object.copy", s);

  s << "\t# pop e1 to t1, move e2 to t2\n"
    << "\t# a0 has true and a1 has false to call equality_test\n";
  emit_pop_temp(T1, s);
  // emit_move(T2, ACC, s);

  s << "\t# extract int value inside the object\n";
  emit_load(T1, 3, T1, s);
  emit_load(T2, 3, ACC, s);

  s << "\t# modify int into t1\n";
  emit_add(T1, T1, T2, s);
  emit_store(T1, 3, ACC, s);
}

void sub_class::code(ostream &s) {
  s << "# Subtract\n";
  s << "\t# start eval e1" << endl;
  e1->code(s);
  s << "\t# e1 save to temp var" << endl;
  emit_push_temp(ACC, s);

  s << "\t# start eval e2 and copy" << endl;
  e2->code(s);
  emit_jal("Object.copy", s);

  s << "\t# pop e1 to t1, move e2 to t2\n"
    << "\t# a0 has true and a1 has false to call equality_test\n";
  emit_pop_temp(T1, s);
  // emit_move(T2, ACC, s);

  s << "\t# extract int value inside the object\n";
  emit_load(T1, 3, T1, s);
  emit_load(T2, 3, ACC, s);

  s << "\t# modify int into t1\n";
  emit_sub(T1, T1, T2, s);
  emit_store(T1, 3, ACC, s);
}

void mul_class::code(ostream &s) {
  s << "# Multiplication\n";
  s << "\t# start eval e1" << endl;
  e1->code(s);
  s << "\t# e1 save to temp var" << endl;
  emit_push_temp(ACC, s);

  s << "\t# start eval e2 and copy" << endl;
  e2->code(s);
  emit_jal("Object.copy", s);

  s << "\t# pop e1 to t1, move e2 to t2\n"
    << "\t# a0 has true and a1 has false to call equality_test\n";
  emit_pop_temp(T1, s);
  // emit_move(T2, ACC, s);

  s << "\t# extract int value inside the object\n";
  emit_load(T1, 3, T1, s);
  emit_load(T2, 3, ACC, s);

  s << "\t# modify int into t1\n";
  emit_mul(T1, T1, T2, s);
  emit_store(T1, 3, ACC, s);
}

void divide_class::code(ostream &s) {
  s << "# Divide \n";
  s << "\t# start eval e1" << endl;
  e1->code(s);
  s << "\t# e1 save to temp var" << endl;
  emit_push_temp(ACC, s);

  s << "\t# start eval e2 and copy" << endl;
  e2->code(s);
  emit_jal("Object.copy", s);

  s << "\t# pop e1 to t1, move e2 to t2\n"
    << "\t# a0 has true and a1 has false to call equality_test\n";
  emit_pop_temp(T1, s);
  // emit_move(T2, ACC, s);

  s << "\t# extract int value inside the object\n";
  emit_load(T1, 3, T1, s);
  emit_load(T2, 3, ACC, s);

  s << "\t# modify int into t2\n";
  emit_div(T1, T1, T2, s);
  emit_store(T1, 3, ACC, s);
}

void neg_class::code(ostream &s) {
  s << "\t# negative exp\n";
  s << "\t# eval e1 and copy\n";
  e1->code(s);
  emit_jal("Object.copy", s);

  s << "\t# eval neg to a0\n";
  emit_load(T1, 3, ACC, s);
  emit_neg(T1, T1, s);
  emit_store(T1, 3, ACC, s);
}

void lt_class::code(ostream &s) {
  s << "# Int operation: less than\n";
  s << "\t# start eval e1" << endl;
  e1->code(s);
  s << "\t# e1 save to temp var" << endl;
  emit_push_temp(ACC, s);
  s << "\t# start eval e2" << endl;
  e2->code(s);
  s << "\t# pop e1 to t1, move e2 to t2\n"
    << "\t# a0 has true and a1 has false to call equality_test\n";
  emit_pop_temp(T1, s);
  // emit_move(T2, ACC, s);

  s << "\t# extract int value inside the object\n";
  emit_load(T1, 3, T1, s);
  emit_load(T2, 3, ACC, s);

  s << "\t# pretend t1 < t2\n";
  emit_load_bool(ACC, BoolConst(1), s);
  s << "\t# if t1 < t2 return true and finish\n";
  emit_blt(T1, T2, label_num, s);
  emit_load_bool(ACC, BoolConst(0), s);
  emit_label_def(label_num++, s);
}

void eq_class::code(ostream &s) {

  s << "# equality_test exp" << endl;
  s << "\t# start eval e1" << endl;
  e1->code(s);

  s << "\t# e1 save to temp var" << endl;
  // emit_push(ACC, s);
  emit_push_temp(ACC, s);

  s << "\t# start eval e2" << endl;
  e2->code(s);

  s << "\t# pop e1 to t1, move e2 to t2\n"
    << "\t# a0 has true and a1 has false to call equality_test\n";
  // emit_pop(T1, s);
  emit_pop_temp(T1, s);
  emit_move(T2, ACC, s);

  if (e1->type == Int || e1->type == Str || e1->type == Bool)
    if (e2->type == Int || e2->type == Str || e2->type == Bool) {
      emit_load_bool(ACC, BoolConst(1), s);
      emit_load_bool(A1, BoolConst(0), s);
      emit_jal("equality_test", s);
      return;
    }

  s << "\t# Pretend that t1 = t2" << endl;
  emit_load_bool(ACC, BoolConst(1), s);
  s << "\t# Compare the two pointers." << endl;
  emit_beq(T1, T2, label_num, s);
  emit_load_bool(ACC, BoolConst(0), s);
  emit_label_def(label_num++, s);
}

void leq_class::code(ostream &s) {
  s << "# Int operation: less and euqal\n";
  s << "\t# start eval e1" << endl;
  e1->code(s);
  s << "\t# e1 save to temp var" << endl;
  emit_push_temp(ACC, s);
  s << "\t# start eval e2" << endl;
  e2->code(s);
  s << "\t# pop e1 to t1, move e2 to t2\n"
    << "\t# a0 has true and a1 has false to call equality_test\n";
  emit_pop_temp(T1, s);
  // emit_move(T2, ACC, s);

  s << "\t# extract int value inside the object\n";
  emit_load(T1, 3, T1, s);
  emit_load(T2, 3, ACC, s);

  s << "\t# pretend t1 <= t2\n";
  emit_load_bool(ACC, BoolConst(1), s);
  s << "\t# if t1 <= t2 return true and finish\n";
  emit_bleq(T1, T2, label_num, s);
  emit_load_bool(ACC, BoolConst(0), s);
  emit_label_def(label_num++, s);
}

void comp_class::code(ostream &s) {
  s << "\t# not expr" << endl;
  s << "\t# First eval the bool" << endl;
  e1->code(s);

  emit_load(T1, 3, ACC, s);

  s << "\t# Pretend ACC = false, then we need to construct true" << endl;
  emit_load_bool(ACC, BoolConst(1), s);

  s << "\t# If ACC = false, jumpto finish" << endl;
  emit_beq(T1, ZERO, label_num, s);

  s << "\t# Load false" << endl;
  emit_load_bool(ACC, BoolConst(0), s);

  s << "\t# finish:" << endl;
  emit_label_def(label_num, s);

  ++label_num;
}

void int_const_class::code(ostream &s) {
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC, inttable.lookup_string(token->get_string()), s);
}

void string_const_class::code(ostream &s) {
  emit_load_string(ACC, stringtable.lookup_string(token->get_string()), s);
}

void bool_const_class::code(ostream &s) {
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s) {
  s << "# new exp\n";
  if (type_name == SELF_TYPE) {
    s << "\t# new selftype, find it out\n";
    emit_load_address(T1, "class_objTab", s);
    s << "\t# get class by tag\n";
    emit_load(T2, 0, SELF, s);
    emit_sll(T2, T2, 3, s);
    s << "\t# get self prototype addr\n";
    emit_addu(T1, T1, T2, s);

    s << "\t# push addr on temp\n";
    emit_push_temp(T1, s);
    emit_load(ACC, 0, T1, s);
    emit_jal("Object.copy", s);

    s << "\t# pop addr from temp\n";
    emit_pop_temp(T1, s);
    s << "\t# get self init addr\n";
    emit_load(T1, 1, T1, s);
    s << "\t# goto init\n";
    emit_jalr(T1, s);

    return;
  }

  s << LA << ACC << " " << type_name << PROTOBJ_SUFFIX << endl;
  emit_jal("Object.copy", s);
  s << JAL << type_name << CLASSINIT_SUFFIX << endl;
}

void isvoid_class::code(ostream &s) {
  s << "# isvoid expr\n";
  e1->code(s);
  emit_move(T1, ACC, s);
  emit_load_bool(ACC, BoolConst(1), s);
  emit_beqz(T1, label_num, s);
  emit_load_bool(ACC, BoolConst(0), s);
  emit_label_def(label_num++, s);
}

void no_expr_class::code(ostream &s) { emit_move(ACC, ZERO, s); }

void object_class::code(ostream &s) {
  s << "\t# It's Object " << name << endl;
  int indx;

  if ((indx = env.get_Var(name)) != -1) {
    s << "\t# find in " << indx << " position in let variables\n";
    emit_load(ACC, indx, FP, s);
    if (cgen_Memmgr == 1) {
      emit_addiu(A1, FP, 4 * indx, s);
      emit_gc_assign(s);
    }

  } else if ((indx = env.get_Argument(name)) != -1) {
    s << "\t# find in " << indx << " position in argument\n";
    emit_load(ACC, DEFAULT_FPFIELDS + temp_size + indx, FP, s);
    if (cgen_Memmgr == 1) {
      emit_addiu(A1, FP, 4 * (DEFAULT_FPFIELDS + temp_size + indx), s);
      emit_gc_assign(s);
    }

  } else if ((indx = env.get_Attribute(name)) != -1) {
    s << "\t# find in " << indx << " position in attribute\n";
    emit_load(ACC, DEFAULT_OBJFIELDS + indx, SELF, s);
    if (cgen_Memmgr == 1) {
      emit_addiu(A1, SELF, 4 * (DEFAULT_OBJFIELDS + indx), s);
      emit_gc_assign(s);
    }

  } else if (name == self) {
    s << "\t# It is self." << endl;
    emit_move(ACC, SELF, s);
  } else {
    s << "\t#obj not found! ERRORRRRRRRRRRRRRRRRRRRRRRRR\n";
    if (cgen_debug)
      cout << "\t#obj not found! " << name << " ERRORRRRRRRRRRRRRRRRRRRRRRRR\n";
  }
}

//--------------------------------------------------------------------------------

int method_class::tmp_Num() { return expr->tmp_Num(); }
int attr_class::tmp_Num() { return init->tmp_Num(); }

int assign_class::tmp_Num() { return expr->tmp_Num(); }

int static_dispatch_class::tmp_Num() {
  int mx = expr->tmp_Num();
  for (int i = actual->first(); actual->more(i); i = actual->next(i))
    mx = std::max(mx, actual->nth(i)->tmp_Num());
  return mx;
}

int dispatch_class::tmp_Num() {
  int mx = expr->tmp_Num();
  for (int i = actual->first(); actual->more(i); i = actual->next(i))
    mx = std::max(mx, actual->nth(i)->tmp_Num());
  return mx;
}

int cond_class::tmp_Num() {
  return std::max(pred->tmp_Num(),
                  std::max(then_exp->tmp_Num(), else_exp->tmp_Num()));
}

int loop_class::tmp_Num() { return std::max(pred->tmp_Num(), body->tmp_Num()); }

int typcase_class::tmp_Num() {
  int mx = expr->tmp_Num();
  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    mx = std::max(mx, 1 + cases->nth(i)->tmp_Num());
  }
  return mx;
}

int branch_class::tmp_Num() { return expr->tmp_Num(); }

int block_class::tmp_Num() {
  int mx = 0;
  for (int i = body->first(); body->more(i); i = body->next(i)) {
    mx = std::max(mx, body->nth(i)->tmp_Num());
  }
  return mx;
}

int let_class::tmp_Num() {
  return std::max(init->tmp_Num(), body->tmp_Num() + 1);
}

int plus_class::tmp_Num() { return std::max(e1->tmp_Num(), e2->tmp_Num() + 1); }
int sub_class::tmp_Num() { return std::max(e1->tmp_Num(), e2->tmp_Num() + 1); }
int mul_class::tmp_Num() { return std::max(e1->tmp_Num(), e2->tmp_Num() + 1); }
int divide_class::tmp_Num() {
  return std::max(e1->tmp_Num(), e2->tmp_Num() + 1);
}
int lt_class::tmp_Num() { return std::max(e1->tmp_Num(), e2->tmp_Num() + 1); }
int eq_class::tmp_Num() { return std::max(e1->tmp_Num(), e2->tmp_Num() + 1); }
int leq_class::tmp_Num() { return std::max(e1->tmp_Num(), e2->tmp_Num() + 1); }

int neg_class::tmp_Num() { return e1->tmp_Num(); }
int comp_class::tmp_Num() { return e1->tmp_Num(); }

int int_const_class::tmp_Num() { return 0; }
int bool_const_class::tmp_Num() { return 0; }
int string_const_class::tmp_Num() { return 0; }
int new__class::tmp_Num() {
  if (type_name == SELF_TYPE)
    return 1;
  return 0;
}
int object_class::tmp_Num() { return 0; }
int no_expr_class::tmp_Num() { return 0; }

int isvoid_class::tmp_Num() { return e1->tmp_Num(); }
