%output  "build/parser/parser.cpp"
%defines "build/parser/parser.hpp"
%define api.pure full
%locations
%lex-param   { yyscan_t scanner }
%parse-param { yyscan_t scanner }
%parse-param { ast::Module& mod }

%code requires {
#include "ast_types.hpp"
#define YYSTYPE ast::Token
#define YY_EXTRA_TYPE int
#ifndef YY_TYPEDEF_YY_SCANNER_T
#define YY_TYPEDEF_YY_SCANNER_T
typedef void *yyscan_t;
#endif
}

%code {
#include "lexer.hpp"
int yyerror(YYLTYPE* yyllocp, yyscan_t scanner, ast::Module& mod, const char *msg) 
{
    (void) scanner;

    throw Error(Error::PARSER, mod.file, Location(yyllocp->first_line, yyllocp->first_column), String(msg));

    return 0;
}

Location to_loc(YYLTYPE* yylocp)
{
    return Location { 
        yylocp->first_line,
        yylocp->first_column
    };
}

#define LOC to_loc(&yyloc)

}

%initial-action { yyset_extra(0, scanner); }


%token ID
%token STRING
%token INT
%token FLOAT

%token LET

%% 

program
    : id { mod.i = 10; }

id
    : ID { $$ = new ast::Id { String($1.string_val), LOC } }
    
%%

#include <cstdio>

int parse_file(const File& file, ast::Module& module) 
{
    yyscan_t sc;
    int res = 0;

    FILE *input = std::fopen(file.c_str(), "rb");
    if (input == nullptr) {
        return 0;
    }

    if (yylex_init(&sc) != 0) {
        fclose(input);
        return 0;
    }
    yyset_in(input, sc);
    res = yyparse(sc, module);
    yylex_destroy(sc);

    fclose(input);

    return res;
}

