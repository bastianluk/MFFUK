%language "c++"
%require "3.0.4"
%defines
%define parser_class_name{ mlaskal_parser }
%define api.token.constructor
%define api.token.prefix{DUTOK_}
%define api.value.type variant
%define parse.assert
%define parse.error verbose

%locations
%define api.location.type{ unsigned }

%code requires
{
	// this code is emitted to du3456g.hpp

	// allow references to semantic types in %type
#include "dutables.hpp"
#include <deque>
#include <stdio.h>

	// avoid no-case warnings when compiling du3g.hpp
#pragma warning (disable:4065)

// adjust YYLLOC_DEFAULT macro for our api.location.type
#define YYLLOC_DEFAULT(res,rhs,N)	(res = (N)?YYRHSLOC(rhs, 1):YYRHSLOC(rhs, 0))
// supply missing YY_NULL in bfexpg.hpp
#define YY_NULL	0
#define YY_NULLPTR	0
}

%param{ mlc::yyscan_t2 yyscanner }	// formal name "yyscanner" is enforced by flex
%param{ mlc::MlaskalCtx* ctx }

%start mlaskal

%code
{
	// this code is emitted to du3456g.cpp

	// declare yylex here 
	#include "bisonflex.hpp"
	YY_DECL;

	// allow access to context 
	#include "dutables.hpp"

	// other user-required contents
	#include <assert.h>
	#include <stdlib.h>

	#include "du3456sem.hpp"

    /* local stuff */
    using namespace mlc;

}

%token EOF	0	"end of file"

%token PROGRAM			/* program */
%token LABEL			    /* label */
%token CONST			    /* const */
%token TYPE			    /* type */
%token VAR			    /* var */
%token BEGIN			    /* begin */
%token END			    /* end */
%token PROCEDURE			/* procedure */
%token FUNCTION			/* function */
%token ARRAY			    /* array */
%token OF				    /* of */
%token GOTO			    /* goto */
%token IF				    /* if */
%token THEN			    /* then */
%token ELSE			    /* else */
%token WHILE			    /* while */
%token DO				    /* do */
%token REPEAT			    /* repeat */
%token UNTIL			    /* until */
%token FOR			    /* for */
%token OR				    /* or */
%token NOT			    /* not */
%token RECORD			    /* record */

/* literals */
%token<mlc::ls_id_index> IDENTIFIER			/* identifier */
%token<mlc::ls_int_index> UINT			    /* unsigned integer */
%token<mlc::ls_real_index> REAL			    /* real number */
%token<mlc::ls_str_index> STRING			    /* string */

/* delimiters */
%token SEMICOLON			/* ; */
%token DOT			    /* . */
%token COMMA			    /* , */
%token EQ				    /* = */
%token COLON			    /* : */
%token LPAR			    /* ( */
%token RPAR			    /* ) */
%token DOTDOT			    /* .. */
%token LSBRA			    /* [ */
%token RSBRA			    /* ] */
%token ASSIGN			    /* := */

/* grouped operators and keywords */
%token<mlc::DUTOKGE_OPER_REL> OPER_REL			    /* <, <=, <>, >=, > */
%token<mlc::DUTOKGE_OPER_SIGNADD> OPER_SIGNADD		    /* +, - */
%token<mlc::DUTOKGE_OPER_MUL> OPER_MUL			    /* *, /, div, mod, and */
%token<mlc::DUTOKGE_FOR_DIRECTION> FOR_DIRECTION		    /* to, downto */

%type<mlc::parameter_list_ptr> formal_parameters formal_parameters_cycle
%type<std::deque<mlc::ls_id_index>> identifiers
%type<mlc::ls_id_index> function_header procedure_header procfunc_header 
%type<mlc::type_pointer> type record_type
%type<mlc::field_list_ptr> field_list;

%%

/* START - PROGRAM */
mlaskal:	    PROGRAM IDENTIFIER SEMICOLON block_p DOT;

/* BLOCK P */
block_p:		label_block const_block type_block var_block procfunc_decl_block code_block;
/* LABEL */
label_block:			| LABEL UINT label_blocks SEMICOLON
						{
							/* add current label */
							ctx->tab->add_label_entry(@2, $2, ctx->tab->new_label());
						};
label_blocks:			| COMMA UINT label_blocks
						{
							/* add current label + recursion */
							ctx->tab->add_label_entry(@2, $2, ctx->tab->new_label());
						};
/* CONST */
const_block:			| CONST constant SEMICOLON const_blocks;
const_blocks:			| constant SEMICOLON const_blocks;
/* TYPE */
type_block:			| TYPE type_blocks
type_blocks:		IDENTIFIER EQ type SEMICOLON
					{
						/* add the type */
						ctx->tab->add_type(@1, $1, $3);						
					}
					|  type_blocks IDENTIFIER EQ type SEMICOLON
					{
						/* recursion block - add the type */
						ctx->tab->add_type(@2, $2, $4);
					};
/* VAR */
var_block:			| VAR IDENTIFIER identifiers COLON type SEMICOLON var_blocks
					{
						/* add the current identifier and then all the ones in the identifiers queue with the correct type */
						ctx->tab->add_var(@2, $2, $5);
						for(mlc::ls_id_index idx : $3)
						{
							ctx->tab->add_var(@2, idx, $5);
						}
					};
var_blocks:			| IDENTIFIER identifiers COLON type SEMICOLON var_blocks
					{
						/* add the current identifier and then all the ones in the identifiers queue with the correct type */
						ctx->tab->add_var(@1, $1, $4);
						for(mlc::ls_id_index idx : $2)
						{
							ctx->tab->add_var(@1, idx, $4);
						}
					};
/* PROC/FUNC */
procfunc_decl_block:		| procfunc_header SEMICOLON { ctx->tab->enter(@2, $1); } block { ctx->tab->leave(@4); } SEMICOLON procfunc_decl_block; /* the enter and leave function for procedures and functions are necessary here */
procfunc_header:		procedure_header 
						{
							/* pass identifier for enter function above */
							$$ = $1;
						}
						| function_header
						{
							/* pass identifier for enter and leave above */
							$$ = $1;
						};
/* CODE */
code_block:		BEGIN statement statements END;
statements:		| SEMICOLON statement statements;

identifiers:	{
					/* create deque for identifiers */
					$$ = std::deque<mlc::ls_id_index>();
				}
				| COMMA IDENTIFIER identifiers
				{
					/* when going out of recursion add identifier to the deque */
					auto list = $3;
					list.push_front($2);
					$$ = list;
				};

/* BLOCK */
block:		label_block const_block type_block var_block code_block;

/* PROCEDURE HEADER */
procedure_header:		PROCEDURE IDENTIFIER LPAR formal_parameters RPAR
						{
							/* add procedure and return its name for enter function */
							ctx->tab->add_proc(@1, $2, $4);
							$$ = $2;
						}
						| PROCEDURE IDENTIFIER
						{
							/* add procedure with empty parameter list and return its name for enter function */
							ctx->tab->add_proc(@1, $2, mlc::create_parameter_list());
							$$ = $2;
						};
/* FUNCTION HEADER */
function_header:		FUNCTION IDENTIFIER LPAR formal_parameters RPAR COLON IDENTIFIER /* scalar type identifier */
						{
							/* add function with correct return type and parameters + return its name for enter function */
							ctx->tab->add_fnc(@1, $2, mlc::get_type_pointer(ctx->tab, $7, @7), $4);
							$$ = $2;
						}
						| FUNCTION IDENTIFIER COLON IDENTIFIER /* scalar type identifier */
						{
							/* add function with correct return type, empty parameter list + return its name for enter function */
							ctx->tab->add_fnc(@1, $2, mlc::get_type_pointer(ctx->tab, $4, @4), mlc::create_parameter_list());
							$$ = $2;
						};

/* FORMAL PARAMETERS */
formal_parameters:		VAR IDENTIFIER identifiers COLON IDENTIFIER formal_parameters_cycle
						{
							/* create new param list and add the current identifier with correct type, then add all the other identifiers from recursion; concat with other parameters and return the list */
							auto list = mlc::create_parameter_list();
							auto type = mlc::get_type_pointer(ctx->tab, $5, @5);
							list->append_parameter_by_reference($2, type);
							for(mlc::ls_id_index idx : $3)
							{
								list->append_parameter_by_reference(idx, type);
							}
							list->append_and_kill($6);
							$$ = list;
						}
						|  IDENTIFIER identifiers COLON IDENTIFIER /* type identifier */ formal_parameters_cycle
						{
							/* create new param list and add the current identifier with correct type, then add all the other identifiers from recursion; concat with other parameters and return the list */
							auto list = mlc::create_parameter_list();
							auto type = mlc::get_type_pointer(ctx->tab, $4, @4);
							list->append_parameter_by_value($1, type);
							for(mlc::ls_id_index idx : $2)
							{
								list->append_parameter_by_value(idx, type);
							}
							list->append_and_kill($5);
							$$ = list;
						};
formal_parameters_cycle:			{
										/* bottom of recursion for lists of different typed parameters */
										$$ = mlc::create_parameter_list();
									}
									| SEMICOLON formal_parameters
									{
										/* cycle for recursion for lists of different typed parameters */
										$$ = $2;
									};

/*TYPES*/
type:		record_type 
			{
				/* get type from below */
				$$ = $1;
			}
			| IDENTIFIER /* both ordinal type and type identifiers, by extension structured type indentifier */
			{
				/* literally call get type based on identifier name */
				$$ = mlc::get_type_pointer(ctx->tab, $1, @1);
			};

/* STRUCTURED TYPES */
/* RECORD */
record_type:	RECORD field_list END
				{
					/* create and return record type */
					$$ = ctx->tab->create_record_type($2, @1);
				}
				| RECORD field_list SEMICOLON END
				{
					/* create and return record type */
					$$ = ctx->tab->create_record_type($2, @1);
				};
field_list:		{
					/* bottom of recursion - create empty field parameter list */
					$$ = mlc::create_field_list();
				}
				| IDENTIFIER identifiers COLON type
				{
					/* bottom of recursion - create empty field parameter list and add current identifier + all the ones that share the type */
					auto list = mlc::create_field_list();
					list->append_field($1, $4);
					for(mlc::ls_id_index idx : $2)
					{
						list->append_field(idx, $4);
					}
					$$ = list;
				}
				| field_list SEMICOLON IDENTIFIER identifiers COLON type
				{
					/* recursion - create empty field parameter list; concat with previously created lists and add current identifier + all the ones that share the type */
					auto list = mlc::create_field_list();
					list->append_and_kill($1);
					list->append_field($3, $6);
					for(mlc::ls_id_index idx : $4)
					{
						list->append_field(idx, $6);
					}
					$$ = list;
				};

/* STATEMENT */
statement:		optional_statement statement_inner | statement_inner;
optional_statement:		UINT COLON;
/* From slides */
statement_inner:	| m_statement | u_statement;
m_statement:	IF expression /* boolean expression */ THEN m_statement ELSE m_statement
				| WHILE expression /* boolean expression */ DO m_statement
				| FOR IDENTIFIER /* ordinal type or variable identifier */ ASSIGN expression /* ordinal expression */ FOR_DIRECTION expression /* ordinal expression */ DO m_statement
				| o_statement;
u_statement:	IF expression /* boolean expression */ THEN statement
				| IF expression /* boolean expression */ THEN m_statement ELSE u_statement
				| WHILE expression /* boolean expression */ DO u_statement
				| FOR IDENTIFIER /* ordinal type or variable identifier */ ASSIGN expression /* ordinal expression */ FOR_DIRECTION expression /* ordinal expression */ DO u_statement;
o_statement:	BEGIN statement statements END
				| REPEAT statement statements UNTIL expression /* boolean expression */
				| variable ASSIGN expression
				| IDENTIFIER /* function identifier */ ASSIGN expression
				| IDENTIFIER /* procedure identifier */
				| IDENTIFIER /* procedure identifier */ LPAR real_parameters RPAR
				| GOTO UINT;

/* VARIABLE */
variable:		IDENTIFIER /* record variable id */ DOT IDENTIFIER /* field identifier */;
/* REAL PARAMETERS */
real_parameters:		expression real_parameters_cycle;
real_parameters_cycle:	| COMMA real_parameters;

/* EXPRESSION */
expression:		simple_expression OPER_REL simple_expression
				| simple_expression EQ simple_expression
				| simple_expression;
/* SIMPLE EXPRESSION */
simple_expression:		OPER_SIGNADD term terms
						| term terms;
/* TERM */
term:		factor factors;
terms:		| OPER_SIGNADD term terms
			| OR term terms;
/* FACTOR */
factor:		unsign_constant
			| variable
			| IDENTIFIER /* function identifier */
			| IDENTIFIER LPAR real_parameters RPAR
			| LPAR expression RPAR
			| NOT factor;
factors:	| OPER_MUL factor factors;

/* CONSTANT */
constant:	IDENTIFIER EQ IDENTIFIER /* unsigned constant identifier and constant identifier */
			{
				mlc::symbol_pointer sp = ctx->tab->find_symbol($3);
				/* Check for IsConstant following the documentation and lecture hints */
				if ( sp->kind() != SKIND_CONST )
				{
					message( DUERR_NOTCONST, @3, * $3);
				}
				/* Now following pattern is used in each (except bool since that has trivial data): 1/ access; add to list; add to table */
				if ( sp->access_const()->type()->cat() == TCAT_INT )
				{
					mlc::ls_int_index val = sp->access_const()->access_int_const()->int_value();
					mlc::ls_int_index nval = ctx->tab->ls_int().add(*val);
					ctx->tab->add_const_int( @1, $1, nval);
				}
				else if ( sp->access_const()->type()->cat() == TCAT_REAL )
				{
					mlc::ls_real_index val = sp->access_const()->access_real_const()->real_value();
					mlc::ls_real_index nval = ctx->tab->ls_real().add(*val);
					ctx->tab->add_const_real( @1, $1, nval);
				}
				else if ( sp->access_const()->type()->cat() == TCAT_STR )
				{
					mlc::ls_str_index val = sp->access_const()->access_str_const()->str_value();
					mlc::ls_str_index nval = ctx->tab->ls_str().add(*val);
					ctx->tab->add_const_str( @1, $1, nval);
				}
				else
				{
					bool val = sp->access_const()->access_bool_const()->bool_value();
					ctx->tab->add_const_bool( @1, $1, val);
				}
			}
			/* following rules are expanded from | IDENTIFIER EQ unsign_constant and no need for check since you can be certain what type it is */
			| IDENTIFIER EQ UINT
			{
				ctx->tab->add_const_int(@1, $1, $3);
			}
			| IDENTIFIER EQ REAL
			{
				ctx->tab->add_const_real(@1, $1, $3);
			}
			| IDENTIFIER EQ STRING
			{
				ctx->tab->add_const_str(@1, $1, $3);
			}
			| IDENTIFIER EQ OPER_SIGNADD UINT
			{
				if($3 == mlc::DUTOKGE_OPER_SIGNADD::DUTOKGE_MINUS)
				{
					mlc::ls_int_index nval = ctx->tab->ls_int().add(- *$4);
					ctx->tab->add_const_int(@1, $1, nval);
				}
				else
				{
					ctx->tab->add_const_int(@1, $1, $4);
				}



			}
			| IDENTIFIER EQ OPER_SIGNADD REAL
			{
				if($3 == mlc::DUTOKGE_OPER_SIGNADD::DUTOKGE_MINUS)
				{
					mlc::ls_real_index nval = ctx->tab->ls_real().add(- *$4);
					ctx->tab->add_const_real(@1, $1, nval);
				}
				else
				{				
					ctx->tab->add_const_real(@1, $1, $4);
				}
			};

/* UNSIGNED CONSTANT */
unsign_constant:	UINT
					| REAL
					| STRING /* ' chars ' */;

/*
Left out:
- structured_type
- ordinal type
- ordinal constant
- unsigned integer
- unsigned real number
- identifier
- letter
- digit
*/

%%


namespace yy {

	void mlaskal_parser::error(const location_type& l, const std::string& m)
	{
		message(DUERR_SYNTAX, l, m);
	}

}
