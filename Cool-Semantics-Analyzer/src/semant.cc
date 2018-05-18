

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
    this->class_table->addid(No_class, nullptr);

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

    ClassTable *classtable = new ClassTable(classes);

    if (classtable->errors()) 
    {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }

    // Phase II: Check the rest
}


