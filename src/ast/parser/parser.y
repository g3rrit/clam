%output  "build/parser/parser.cpp"
%defines "build/parser/parser.hpp"
%define api.pure full
%locations
%lex-param   { yyscan_t scanner }
%parse-param { yyscan_t scanner }
%parse-param { Fn<void(ast::Module*)> cb }
%parse-param { const File& file }

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
int yyerror(YYLTYPE* yyllocp, yyscan_t scanner, Fn<void(ast::Module*)>, const File& file, const char *msg) 
{
    (void) scanner;

    throw Error(Error::PARSER, file, Location(yyllocp->first_line, yyllocp->first_column), String(msg));

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

#define FORWARD(l, r) \
    do  { \
        l._type = r._type; \
        l.field = r.field; \
    } while(0)

}

%initial-action { yyset_extra(0, scanner); }


%token MODULE
%token END
%token ID
%token STRING
%token INT
%token FLOAT
%token CHAR

%token LET
%token DATA
%token STRUCT

%token IF
%token THEN
%token ELSE
%token MATCH

%token COMMA
%token ARROW
%token COLON
%token SPIPE
%token PIPE
%token SEMICOLON
%token EQUALS
%token DOLLAR
%token BACKSLASH

%token LPAREN
%token RPAREN
%token LBRACK
%token RBRACK
%token LBRACE
%token RBRACE

%% 

file_p
    : /* empty */
    | file_p mod_p { cb($2.module); }
    ;

mod_p
    : mod_ps END { $$ = $1; }
    ;

mod_ps
    : MODULE id_p { SET($$, MODULE, module, (new ast::Module { file, $2.id })); }
    | mod_ps tl_p { $$ = $1; $$.module->add_tl($2.toplevel); }
    ;

tl_p 
    : data_p { 
            SET($$, TOPLEVEL, toplevel, (new ast::Toplevel { $1.data })); 
        }
    | comb_p {
            SET($$, TOPLEVEL, toplevel, (new ast::Toplevel { $1.comb }));
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
    : STRUCT id_p EQUALS field_p { 
            SET($$, RECORD, record, (new ast::Record { $2.id })); 
            $$.record->add_field($4.field);
        }
    | record_p SEMICOLON field_p {
            $$.record->add_field($3.field);
        }
    ;

variant_p
    : DATA id_p EQUALS variant_field_p { 
            SET($$, VARIANT, variant, (new ast::Variant { $2.id })); 
            $$.variant->add_record($4.record);
        }
    | variant_p PIPE variant_field_p {
            FORWARD($$, $1);
            $$.variant->add_record($3.record);
        }
    ;

variant_field_p
    : variant_field_p SEMICOLON field_p { 
            FORWARD($$, $1);
            $$.record->add_field($3.field);
        }
    | id_p { SET($$, RECORD, record, (new ast::Record { $1.id })); }

comb_p
    : LET id_p args_p COLON type_p EQUALS exp_p {
            SET($$, COMB, comb, (new ast::Comb { $2.id, $5.type, $7.exp }));
            for (void* arg : *$3.list) {
                $$.comb->add_arg(static_cast<ast::Field*>(arg));
            }
        }
    ;

args_p
    : args_empty_p { FORWARD($$, $1); }
    | LPAREN args_noempty_p RPAREN { FORWARD($$, $2); }
    ;

args_empty_p
    : /* empty */ { SET($$, LIST, list, (new Array<void*> {})); }
    ;

args_noempty_p
    : field_p { 
            SET($$, LIST, list, (new Array<void*> {}));
            $$.list->push_back($1.field); 
        }
    | args_noempty_p COMMA field_p { 
            FORWARD($$, $1);
            $$.list->push_back($3.field); 
        }
    ;

field_p
    : id_p COLON type_p { SET($$, FIELD, field, (new ast::Field { $1.id, $3.type })); }
    ;

type_p
    : prim_type_p ARROW type_p { SET($$, TYPE, type, (new ast::Type { ast::Type::Fun { $1.type, $3.type } })); }
    | id_p { SET($$, TYPE, type, (new ast::Type { ast::Type::Prim { $1.id } })); }
    ;

prim_type_p
    : id_p { SET($$, TYPE, type, (new ast::Type { ast::Type::Prim { $1.id } })); }
    | LPAREN type_p RPAREN { FORWARD($$, $2); }

id_p : ID {  $$ = $1; } ;

int_p : INT { $$ = $1; } ;

float_p : FLOAT { $$ = $1; } ;

string_p : STRING { $$ = $1; } ;

char_p : CHAR { $$ = $1; } ;

exp_basic_p
    : int_p { SET($$, EXP, exp, (new ast::Exp { $1.int_lit })); }
    | float_p { SET($$, EXP, exp, (new ast::Exp { $1.float_lit })); }
    | string_p { SET($$, EXP, exp, (new ast::Exp { $1.string_lit })); }
    | char_p { SET($$, EXP, exp, (new ast::Exp { $1.char_lit })); }
    | id_p { SET($$, EXP, exp, (new ast::Exp { $1.id })); }
    | LPAREN exp_p RPAREN { FORWARD($$, $2); }
    | MATCH exp_p alter_list_p END { SET($$, EXP, exp, (new ast::Exp { ast::Exp::Match { $2.exp , $3.alter_list } })); }
    ;

alter_list_p
    : alter_p { 
            SET($$, ALTER_LIST, alter_list, (new Array<uptr<ast::Exp::Alter>> {}));
            $$.alter_list->emplace_back($1.alter);
        }
    | alter_list_p alter_p { 
            FORWARD($$, $1);
            $$.alter_list->emplace_back($2.alter);
        }
    ;

alter_p 
    : PIPE id_p id_p ARROW exp_p {
            SET($$, ALTER, alter, (new ast::Exp::Alter { $2.id, $3.id, $5.exp }));
        }
    ;

exp_p
    : exp_seq_p { FORWARD($$, $1); }

exp_seq_p
    : exp_seq_p SEMICOLON exp_let_p { SET($$, EXP, exp, (new ast::Exp { ast::Exp::Seq { $1.exp, $3.exp } })); }
    | exp_lam_p { FORWARD($$, $1); }
    ;

exp_let_p
    : id_p COLON EQUALS exp_lam_p { SET($$, EXP, exp, (new ast::Exp { ast::Exp::Let { $1.id, $4.exp } })); }
    | exp_lam_p { FORWARD($$, $1); }
    ;

exp_lam_p
    : BACKSLASH id_p ARROW exp_cond_p { SET($$, EXP, exp, (new ast::Exp { ast::Exp::Lam { $2.id, $4.exp } })); }
    | exp_cond_p { FORWARD($$, $1); }
    ;

exp_cond_p
    : IF exp_p THEN exp_p ELSE exp_spipe_p { SET($$, EXP, exp, (new ast::Exp { ast::Exp::Cond { $2.exp, $4.exp, $6.exp } })); }
    | exp_spipe_p { FORWARD($$, $1); }
    ;

exp_spipe_p
    : exp_spipe_p SPIPE exp_dollar_p { SET($$, EXP, exp, (new ast::Exp { ast::Exp::App { $3.exp, $1.exp } })); }
    | exp_dollar_p { FORWARD($$, $1);}
    ;

exp_dollar_p
    : exp_dollar_p DOLLAR exp_app_p { SET($$, EXP, exp, (new ast::Exp { ast::Exp::App { $1.exp, $3.exp } })); }
    | exp_app_p { FORWARD($$, $1); }
    ;

exp_app_p
    : exp_app_p exp_basic_p { SET($$, EXP, exp, (new ast::Exp { ast::Exp::App { $1.exp, $2.exp } })); }
    | exp_basic_p { FORWARD($$, $1); }
    ;

%%

#include <cstdio>

int parse_file(const File& file, Fn<void(ast::Module*)> cb) 
{
#ifdef YYDEBUG 
    yydebug = 1;
#endif


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
    res = yyparse(sc, cb, file);
    yylex_destroy(sc);

    fclose(input);

    return res;
}

