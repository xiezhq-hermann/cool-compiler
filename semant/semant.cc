#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"

#include <map>
#include <queue>

extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}

Class_ curr_class;
ClassTable* classtable;
SymbolTable<char*, Entry>* symboltable;

ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr)
{
    this->class_list = classes;
    this->class_table = new SymbolTable<Symbol, Class__class>();
    this->class_table->enterscope();

    install_basic_classes();
    construct_class_table();

    // Halt the compilation if the program have trivial errors in class definitions.
    if(this->errors())  return;

    check_inheritance();
}

void ClassTable::install_basic_classes()
{

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

    Class_ Object_class =
	class_(Object,
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    //
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class =
	class_(IO,
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.
    //
    Class_ Int_class =
	class_(Int,
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //
    Class_ Str_class =
	class_(Str,
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat,
								      single_Formals(formal(arg, Str)),
								      Str,
								      no_expr()))),
			       single_Features(method(substr,
						      append_Formals(single_Formals(formal(arg, Int)),
								     single_Formals(formal(arg2, Int))),
						      Str,
						      no_expr()))),
	       filename);

    // Add the 5 basic classes to the list of classes
    this->class_list = append_Classes(single_Classes(Object_class), this->class_list);
    this->class_list = append_Classes(single_Classes(IO_class), this->class_list);
    this->class_list = append_Classes(single_Classes(Int_class), this->class_list);
    this->class_list = append_Classes(single_Classes(Bool_class), this->class_list);
    this->class_list = append_Classes(single_Classes(Str_class), this->class_list);
}

void ClassTable::construct_class_table()
{
    Class_ cls;
    Symbol class_name_symbol;
    Symbol parent_name_symbol;

    bool is_main_defined = false;

    // First add symbol No_class to the class table
    // TODO: Is this necessary? Commenting out this line seems to be OK, but perserving it seems to be more sound?
    this->class_table->addid(No_class, NULL);

    // Iterate through the class list and construct the class table
    // Several verifications are done during this iteration
    for(int i = this->class_list->first(); this->class_list->more(i); i = this->class_list->next(i))
    {
        cls = this->class_list->nth(i);
        class_name_symbol = cls->get_name_symbol();
        parent_name_symbol = cls->get_parent_name_symbol();

        if(this->class_table->lookup(class_name_symbol) != NULL)
        {
            semant_error(cls);
            this->error_stream << " CRITICAL: Class \'" << class_name_symbol->get_string() << "\' redefinition." << endl;
            continue;
        }

        if(class_name_symbol == SELF_TYPE)
        {
            semant_error(cls);
            this->error_stream << " CRITICAL: Cannot define a class named SELF_TYPE." << endl;
            continue;
        }

        if(parent_name_symbol == Int || parent_name_symbol == Bool || parent_name_symbol == Str || parent_name_symbol == SELF_TYPE)
        {
            semant_error(cls);
            this->error_stream << " CRITICAL: Class \'" << class_name_symbol->get_string() << "\' inherits from Int, Bool, Str or SELF_TYPE." << endl;
            continue;
        }

        if(class_name_symbol == Main)
            is_main_defined = true;

        this->class_table->addid(class_name_symbol, cls);
    }

    if(!is_main_defined)
    {
        semant_error();
        this->error_stream << " CRITICAL: The Main class required but not defined." << endl;
    }
}

void ClassTable::check_inheritance()
{
    // First check if all parents are defined.
    if(!check_parents())    return;

    // Then construct the inheritance graph and verify if it is a DAG
    check_DAG();
}

bool ClassTable::check_parents()
{
    bool ret_val = true;

    Class_ cls;
    Symbol parent_name_symbol;

    // Iterate through the class list and query the class table for existence of parent classes
    for(int i = this->class_list->first(); this->class_list->more(i); i = this->class_list->next(i))
    {
        cls = this->class_list->nth(i);
        parent_name_symbol = cls->get_parent_name_symbol();

        if(this->class_table->lookup(parent_name_symbol) == NULL && parent_name_symbol != No_class)
        {
            semant_error(cls);
            this->error_stream << " CRITICAL: Parent class \'" << cls->get_name_symbol()->get_string() << "\' of class \'" << parent_name_symbol->get_string() << "\' not defined!" << endl;
            ret_val = false;
        }
    }

    return ret_val;
}

bool ClassTable::check_DAG()
{
    bool ret_val = true;

    // First build the inheritance graph
    // Since only single inheritance is allowed in cool, using map<Symbol, Symbol> is enough
    std::map<Symbol, Symbol> inheritance_graph;
    std::map<Symbol, int> indegree_list;

    Class_ cls;
    Symbol class_name_symbol;
    Symbol parent_name_symbol;

    for(int i = this->class_list->first(); this->class_list->more(i); i = this->class_list->next(i))
    {
        cls = this->class_list->nth(i);
        class_name_symbol = cls->get_name_symbol();
        parent_name_symbol = cls->get_parent_name_symbol();

        // If the class symbol is not in the inheritance graph, add it in
        if(inheritance_graph.find(class_name_symbol) == inheritance_graph.end())
        {
            inheritance_graph.insert(std::pair<Symbol, Symbol>(class_name_symbol, parent_name_symbol));

            // Now fill in the list of indegrees, first update the list with this class
            if(indegree_list.find(class_name_symbol) == indegree_list.end())
                indegree_list.insert(std::pair<Symbol, int>(class_name_symbol, 0));

            // then update the parent's indegree, still first check whether it exists
            if(indegree_list.find(parent_name_symbol) == indegree_list.end())
                indegree_list.insert(std::pair<Symbol, int>(parent_name_symbol, 1));
            else
                indegree_list[parent_name_symbol]++;
        }
    }

    // Now check whether the inheritance graph is a DAG
    std::queue<Symbol> no_indegree_symbols;

    // First pick out all symbols with indegree 0
    for(int i = this->class_list->first(); this->class_list->more(i); i = this->class_list->next(i))
    {
        cls = this->class_list->nth(i);
        class_name_symbol = cls->get_name_symbol();
        if(indegree_list[class_name_symbol] == 0)
            no_indegree_symbols.push(class_name_symbol);
    }

    // Perform the topological sort
    while(!no_indegree_symbols.empty())
    {
        class_name_symbol = no_indegree_symbols.front();
        parent_name_symbol = inheritance_graph[class_name_symbol];

        indegree_list[parent_name_symbol] --;
        if(indegree_list[parent_name_symbol] == 0)
            no_indegree_symbols.push(parent_name_symbol);

        no_indegree_symbols.pop();
        inheritance_graph.erase(class_name_symbol);
    }

    if(!inheritance_graph.empty())
    {
        semant_error();
        this->error_stream << " CRITICAL: Cyclic inheritance detected!" << endl;
        ret_val = false;
    }

    return ret_val;
}


////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
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

ostream& ClassTable::semant_error(Class_ c)
{
    return semant_error(c->get_filename(),c);
}

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()
{
    semant_errors++;
    return error_stream;
}

/////////////////////////////////////////////////////////////////////////////////////
///////////// Auxiliary things

class inheritance_graph {
public:
    std::map<Symbol, Symbol> graph;
    bool conform(const Symbol&, const Symbol&);
    Symbol lca(Symbol, Symbol);
    // lowest common ancestor
} * g;

bool inheritance_graph::conform(const Symbol& a, const Symbol& b)
{
    Symbol cur = a;
    if (a == b)
        return true;
    if (cur == SELF_TYPE)
        cur = curr_class->get_name_symbol();
    while (cur != Object) {
        if (cur == b) {
            return true;
        }
        cur = graph[cur];
    }
    return cur == b;
}

Symbol inheritance_graph::lca(Symbol a, Symbol b)
{
    if (a == b)
        return a;

    if (a == SELF_TYPE)
        a = curr_class->get_name_symbol();

    if (b == SELF_TYPE)
        b = curr_class->get_name_symbol();

    int hight_a = 0;
    int hight_b = 0;
    Symbol tem_a = a;
    Symbol tem_b = b;

    while (tem_a != Object) {
        tem_a = graph[tem_a];
        hight_a++;
    }
    while (tem_b != Object) {
        tem_b = graph[tem_b];
        hight_b++;
    }

    tem_a = a;
    tem_b = b;
    if (hight_a >= hight_b) {
        for (int i = hight_a - hight_b; i > 0; i--) {
            tem_a = graph[tem_a];
        }
    }
    else {
        for (int i = hight_b - hight_a; i > 0; i--) {
            tem_b = graph[tem_b];
        }
    }

    while (tem_a != tem_b) {
        tem_a = graph[tem_a];
        tem_b = graph[tem_b];
    }

    return tem_a;
}


Feature class__class::get_attr(char* feature_name){
    Feature f;
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        f = features->nth(i);
        if (f->get_formals() == NULL && strcmp(const_cast<char*>(f->get_name_symbol()->get_string()), feature_name) == 0) {
            return f;
        }
    }
    return NULL;
}

Feature class__class::get_method(char* feature_name){
    Feature f;
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        f = features->nth(i);
        if (f->get_formals() != NULL && strcmp(const_cast<char*>(f->get_name_symbol()->get_string()), feature_name) == 0) {
            return f;
        }
    }
    return NULL;
}

bool Expression_class::check_arith(Expression e1, Expression e2 = NULL)
{
    e1->check();
    if (e2) {
        e2->check();
    }

    if (e1->get_type() == Int && (e2 == NULL || e2->get_type() == Int)) {
        type = Int;
        return true;
    }
    else {
        type = Object;
        return false;
    }
}

bool Expression_class::check_comp(Expression e1, Expression e2 = NULL)
{
    e1->check();
    if (e2) {
        e2->check();

        if (e1->get_type() == Int && e2->get_type() == Int) {
            type = Bool;
            return true;
        }
        else {
            type = Object;
            return false;
        }
    }
    else {

        if (e1->get_type() == Bool) {
            type = Bool;
            return true;
        }
        else {
            type = Object;
            return false;
        }
    }
}

Symbol branch_class::get_expr_type()
{
    return expr->get_type();
}
///////////////////////////////////////////////////////////////////////////////////
////////////////////////// semantic check implementation for different classes
// As for this type checking part, refer this work a lot for type checking
// https://github.com/luyi0619/cool-compiler

void program_class::check(){
    for(int i = classes->first(); classes->more(i); i = classes->next(i)){
        curr_class = classes->nth(i);
        symboltable->enterscope();
        curr_class->check();
        symboltable->exitscope();
    }
}

void class__class::check(){
    Feature f;
    for(int i = features->first(); features->more(i); i = features->next(i)){
        f = features->nth(i);
        f->check();
    }
}

void method_class::check()
{
    Feature feature;
    Class_ target_class;
    Symbol parent;
    Formals parent_formals;

    symboltable->enterscope();

    if (classtable->class_table->lookup(return_type) == NULL && return_type != SELF_TYPE) {
        throw "Unknown return type";
    }

    feature = NULL;
    target_class = classtable->class_table->lookup(curr_class->get_parent_name_symbol());

    while (true) {
        // iterate to search the valid feature line
        feature = target_class->get_method(const_cast<char*>(name->get_string()));
        if (feature != NULL) {
            break;
        }
        parent = target_class->get_parent_name_symbol();
        if (parent == No_class)
            break;
        target_class = classtable->class_table->lookup(parent);
    }

    if (feature != NULL) {
        parent_formals = feature->get_formals();
        if (parent_formals->len() != formals->len()) {
            throw "invalid formals";
        }

        for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
            Formal f = formals->nth(i);
            f->check();
            Formal parent_f = parent_formals->nth(i);
            if (f->get_formal_type() != parent_f->get_formal_type()) {
                throw "formal type mismatch";
            }
        }

        if (feature->get_type() != return_type) {
            throw "formal type mismatch";
        }
    }
    else {
        for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
            Formal f = formals->nth(i);
            f->check();
        }
    }

    expr->check();

    if (g->conform(expr->get_type(), return_type) == false) {
        throw "types don't conform";
    }

    symboltable->exitscope();
}

void attr_class::check()
{
    Feature feature;
    Class_ target_class;
    Symbol parent;

    if (type_decl == SELF_TYPE) {
        type_decl = curr_class->get_name_symbol();
    }

    if (strcmp(const_cast<char*>(name->get_string()), "self") == 0) {
        throw "the self is preserved";
    }

    feature = NULL;
    target_class = classtable->class_table->lookup(curr_class->get_parent_name_symbol());

    while (true) {
        feature = target_class->get_attr(const_cast<char*>(name->get_string()));
        if (feature != NULL) {
            throw "override occur";
            break;
        }
        parent = target_class->get_parent_name_symbol();
        if (parent == No_class)
            break;
        target_class = classtable->class_table->lookup(parent);
    }

    init->check();

    Symbol init_type = init->get_type();
    if (init_type != No_type && g->conform(init_type, type_decl) == false) {
        throw "type error in attr_class";
    }
    symboltable->addid(const_cast<char*>(name->get_string()), type_decl);
}

void formal_class::check(){
    if (symboltable->probe(const_cast<char*>(name->get_string())) != NULL) {
        throw "Duplicated name";
    }
    if (name == self) {
        throw "self is preserved";
    }
    if (type_decl == SELF_TYPE) {
        throw "self_type is also preserved in rule";
    }
    symboltable->addid(const_cast<char*>(name->get_string()), type_decl);
}

void branch_class::check(){
    symboltable->addid(const_cast<char*>(name->get_string()), type_decl);
    expr->check();
    Symbol expr_type = expr->get_type();
}

void assign_class::check()
{
    expr->check();
    Symbol exprtype = expr->get_type();
    Symbol mytype = symboltable->lookup(const_cast<char*>(name->get_string()));

    if (mytype == NULL) {
        Class_ now_c = curr_class;
        while (true) {

            Feature attr = now_c->get_attr(const_cast<char*>(name->get_string()));

            if (attr != NULL) {
                mytype = attr->get_type();
                break;
            }

            Symbol parent = now_c->get_parent_name_symbol();
            if (parent == No_class)
                break;
            now_c = classtable->class_table->lookup(parent);
        }
    }
    if (mytype == NULL) {
        throw "type error in object_class";
    }

    if (g->conform(exprtype, mytype) == false) {
        throw "type error in assign_class";
    }
    type = exprtype;
}

void static_dispatch_class::check()
{
    expr->check();

    Symbol expr_type = expr->get_type();

    if (g->conform(expr_type, type_name) == false) {
        throw "type error in static_dispatch_class";
    }

    Feature feature = NULL;
    Class_ target_class = classtable->class_table->lookup(type_name);
    while (true) {
        feature = target_class->get_method(const_cast<char*>(name->get_string()));
        if (feature != NULL) {
            break;
        }
        Symbol parent = target_class->get_parent_name_symbol();
        if (parent == No_class)
            break;
        target_class = classtable->class_table->lookup(parent);
    }

    if (feature == NULL) {
        throw "type error in dispatch_class";
    }

    Formals formals_type = feature->get_formals();
    Symbol fun_type = feature->get_type();

    if (fun_type == SELF_TYPE)
        fun_type = expr->get_type();

    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        Expression act = actual->nth(i);
        act->check();
        Symbol nth_formal_type = formals_type->nth(i)->get_formal_type();

        if (g->conform(act->get_type(), nth_formal_type) == false) {
            throw "type error in dispatch_class";
        }
    }

    type = fun_type;
}

void dispatch_class::check()
{
    expr->check();

    Symbol expr_type = expr->get_type();

    if (expr_type == SELF_TYPE)
        expr_type = curr_class->get_name_symbol();

    Feature feature = NULL;
    Class_ target_class = classtable->class_table->lookup(expr_type);
    while (true) {
        feature = target_class->get_method(const_cast<char*>(name->get_string()));
        if (feature != NULL) {
            break;
        }
        Symbol parent = target_class->get_parent_name_symbol();
        if (parent == No_class)
            break;
        target_class = classtable->class_table->lookup(parent);
    }

    if (feature == NULL) {
        throw "type error in dispatch_class";
    }

    Formals formals_type = feature->get_formals();
    Symbol fun_type = feature->get_type();

    if (fun_type == SELF_TYPE)
        fun_type = expr->get_type();

    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        Expression act = actual->nth(i);
        act->check();
        Symbol nth_formal_type = formals_type->nth(i)->get_formal_type();
        if (g->conform(act->get_type(), nth_formal_type) == false) {
            throw "type error in dispatch_class";
        }
    }

    type = fun_type;
}

void cond_class::check()
{
    pred->check();

    if (pred->get_type() != Bool) {
        throw "type error in cond_class";
    }
    then_exp->check();
    else_exp->check();

    Symbol join_type = g->lca(then_exp->get_type(), else_exp->get_type());
    type = join_type;
}

void loop_class::check()
{
    pred->check();
    if (pred->get_type() != Bool) {
        throw "type error in lt_class";
    }
    body->check();
    type = Object;
}

void typcase_class::check()
{
    expr->check();
    Symbol expr_type = expr->get_type();
    Symbol join_type = NULL;

    SymbolTable<char*, Entry>* casetable = new SymbolTable<char*, Entry>();
    casetable->enterscope();

    for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
        symboltable->enterscope();
        Case c = cases->nth(i);

        if (casetable->lookup(const_cast<char*>(c->get_decl_type()->get_string())) != NULL) {
            throw "Duplicate branch Int in case statement.";
        }
        casetable->addid(const_cast<char*>(c->get_decl_type()->get_string()), c->get_decl_type());

        c->check();

        if (g->conform(c->get_expr_type(), c->get_decl_type()) == false) {
            throw "type error in typcase_class";
        }
        if (join_type == NULL)
            join_type = c->get_expr_type();
        else
            join_type = g->lca(join_type, c->get_expr_type());

        symboltable->exitscope();
    }
    type = join_type;
}

void block_class::check()
{
    Expression last = NULL;
    for (int i = body->first(); body->more(i); i = body->next(i)) {
        Expression e = body->nth(i);
        e->check();
        last = e;
    }
    if (last != NULL) {
        type = last->get_type();
    }
}

void let_class::check()
{
    if (identifier == self) {
        throw "type error in let_class";
    }
    init->check();
    symboltable->enterscope();

    Symbol init_type = init->get_type();

    if (init_type != No_type && g->conform(init_type, type_decl) == false) {
        throw "type error in let_class";
    }
    symboltable->addid(const_cast<char*>(identifier->get_string()), type_decl);
    body->check();
    type = body->get_type();
    symboltable->exitscope();
}

void plus_class::check()
{
    if (!check_arith(e1, e2)) {
        throw "type error in plus_class";
    }
}

void sub_class::check()
{
    if (!check_arith(e1, e2)) {
        throw "type error in sub_class";
    }
}

void mul_class::check()
{
    if (!check_arith(e1, e2)) {
        throw "type error in mul_class";
    }
}

void divide_class::check()
{
    if (!check_arith(e1, e2)) {
        throw "type error in divide_class";
    }
}

void neg_class::check()
{
    if (!check_arith(e1)) {
        throw "type error in neg_class";
    }
}

void lt_class::check()
{
    if (!check_comp(e1, e2)) {
        throw "type error in lt_class";
    }
}

void eq_class::check()
{
    e1->check();
    e2->check();

    if (e1->get_type() == Int) {
        if (e2->get_type() == Int)
            type = Bool;
        else {
            type = Object;
            throw "type error in eq_class";
        }
    }
    if (e1->get_type() == Str) {
        if (e2->get_type() == Str)
            type = Bool;
        else {
            type = Object;
            throw "type error in eq_class";
        }
    }
    if (e1->get_type() == Bool) {
        if (e2->get_type() == Bool)
            type = Bool;
        else {
            type = Object;
            throw "type error in eq_class";
        }
    }
    type = Bool;
}

void leq_class::check()
{
    if (!check_comp(e1, e2)) {
        throw "type error in leq_class";
    }
}

void comp_class::check()
{
    if (!check_comp(e1)) {
        throw "type error in comp_class";
    }
}

void int_const_class::check()
{
    type = Int;
}

void bool_const_class::check()
{
    type = Bool;
}

void string_const_class::check()
{
    type = Str;
}

void new__class::check()
{
    Symbol new_type;

    if (type_name == SELF_TYPE) {
        new_type = curr_class->get_name_symbol();
    }
    else {
        new_type = type_name;
    }

    type = type_name;
}

void isvoid_class::check()
{
    e1->check();
    type = Bool;
}

void no_expr_class::check()
{
    type = No_type;
}

void object_class::check()
{
    if (name == self) {
        type = SELF_TYPE;
        return;
    }
    // 1. find object in symboltable
    // 2. find object in attr_node till Object

    Symbol mytype = symboltable->lookup(const_cast<char*>(name->get_string()));

    if (mytype == NULL) {
        Class_ now_c = curr_class;
        while (true) {

            Feature attr = now_c->get_attr(const_cast<char*>(name->get_string()));

            if (attr != NULL) {
                mytype = attr->get_type();
                break;
            }

            Symbol parent = now_c->get_parent_name_symbol();
            if (parent == No_class)
                break;
            now_c = classtable->class_table->lookup(parent);
        }
    }
    if (mytype == NULL) {
        throw "type error in object_class";
    }
    type = mytype;
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
void program_class::semant()
{
    // Phase I: Check class definitions
    initialize_constants();

    classtable = new ClassTable(classes);

    if (classtable->errors())
    {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }

    // Phase II: Check the rest
    symboltable = new SymbolTable<char*, Entry>();

    // build the inheritance graph quickly again
    g = new inheritance_graph();
    g->graph[IO] = Object;
    g->graph[Int] = Object;
    g->graph[Bool] = Object;
    g->graph[Str] = Object;
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        curr_class = classes->nth(i);
        Symbol a = curr_class->get_name_symbol();
        Symbol b = curr_class->get_parent_name_symbol();
        g->graph[a] = b;
    }

    // adapt the simpler error-handling machanism
    try {
        check();
    }
    catch (const char* msg) {
        classtable->semant_error(curr_class) << msg << endl;
    }
    if (classtable->errors())
    {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}