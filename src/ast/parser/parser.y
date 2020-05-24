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
        (uint32_t)yylocp->first_line,
        (uint32_t)yylocp->first_column
    };
}

#define LOC to_loc(&yyloc)

#define SET(l, tx, field, value) \
    do { \
        l._type = ast::Token::tx; \
        l.field = value; \
    } while(0)

}

%initial-action { yyset_extra(0, scanner); }


%token ID
%token STRING
%token INT
%token FLOAT

%token LET
%token DATA
%token STRUCT

%token COLON
%token SEMICOLON
%token EQUALS

%% 

program
    : tl_p { mod.add_tl($1.toplevel); }
    ;

tl_p 
    : data_p { 
            SET($$, TOPLEVEL, toplevel, (new ast::Toplevel { $1.data })); 
        }
    ;

data_p
    : record_p { 
            SET($$, DATA, data, (new ast::Data { $1.record })); 
        }
    | variant_p { 
            SET($$, DATA, data, (new ast::Data { $1.variant })); 
        }
    ;

record_p
    : STRUCT id_p { 
            SET($$, RECORD, record, (new ast::Record { $1.id })); 
        }
    ;

variant_p
    : DATA id_p { 
            SET($$, VARIANT, variant, (new ast::Variant { $1.id })); 
        }
    ;


id_p
    : ID { 
            SET($$, ID, id, (new ast::Id { String($1.string_val), LOC })); 
        }
    ;

int_p
    : INT { 
            SET($$, INT_LIT, int_lit, new ast::Int { $1.int_val }); 
        }
    ;
    
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

