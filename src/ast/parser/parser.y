%{
#include "std.hpp"
#include "ast_types.hpp"

extern int yylex();
extern int yyparse();
extern FILE *yyin;
extern int lineNum;

void yyerror(ast::Module& mod, const char* s);
%}

%code requires {
#include "std.hpp"
#include "ast_types.hpp"
}



%parse-param {ast::Module& mod}

%union {
    char*  sval;
    int    ival;
    double fval;
    char   cval;
}

%token<sval> ID
%token<sval> STRING
%token<ival> INT
%token<fval> FLOAT

%token LET

%type<sval> id

%start program

%% 

program
    : id { mod.i = 10; }

id
    : ID { $$ = $1; }
    
%%

void yyerror(ast::Module& mod, const char* s)
{
    (void) mod;

    std::fprintf(stderr, "parse error: %s \n", s);
    std::fprintf(stderr, "in line: %i\n", lineNum);
    // close(0);
} 