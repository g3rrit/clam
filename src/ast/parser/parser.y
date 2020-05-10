%{
#include "std.hpp"

extern int yylex();
extern int yyparse();
extern FILE *yyin;
extern int lineNum;

void yyerror(const char* s);
%}

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

%type<string> id

%start program

%% 

program
    : id

id
    : ID { $$ = string(move($1)); }
    
%%

void yyerror(const char* s)
{
    std::fprintf(stderr, "parse error: %s \n", s);
    std::fprintf(stderr, "in line: %i\n", lineNum);
    // close(0);
} 