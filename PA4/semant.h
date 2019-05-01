#ifndef SEMANT_H_
#define SEMANT_H_

#include "cool-tree.h"
#include "list.h"
#include "stringtab.h"
#include "symtab.h"
#include <assert.h>
#include <iostream>

#include <list>
#include <map>
#include <vector>

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  int semant_errors;

  void install_basic_classes();
  ostream &error_stream;

  // add my own map
  std::map<Symbol, Class_> mp_class;
  std::map<Symbol, std::list<Symbol>> edges;
  void DFS(Class_ cur, Classes classes, std::map<Class_, int> &mp, bool &err,
           bool &cycle);

public:
  Class_ getClassByName(Symbol classname) {
    if (mp_class.find(classname) == mp_class.end())
      return NULL;
    return mp_class[classname];
  }

  std::list<Symbol> getChildClasses(Symbol classname) {
    return edges[classname];
  }

  bool isBasicClass(Symbol);
  bool isSubtype(Symbol, Symbol);
  Symbol CommonAncestor(Symbol, Symbol);
  Symbol findMethodInAncestor(Symbol, Symbol);

  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream &semant_error();
  ostream &semant_error(Class_ c);
  ostream &semant_error(Symbol filename, tree_node *t);
};

#endif
