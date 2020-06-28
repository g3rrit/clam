%option outfile="build/parser/lexer.cpp" 
%option header-file="build/parser/lexer.hpp"
%option yylineno
%option reentrant bison-bridge bison-locations
%option noyywrap nounput noinput

%{
#include "parser.hpp"
#define RET(TOKEN) return yyextra = TOKEN

#include "std.hpp"
#include "ast_types.hpp"
#include <cstdlib>
#include <cstdio>
#include <cstring>

#define YY_USER_ACTION                                             \
    yylloc->first_line = yylloc->last_line;                          \
    yylloc->first_column = yylloc->last_column;                      \
    if (yylloc->last_line == yylineno)                               \
        yylloc->last_column += yyleng;                                 \
    else {                                                           \
        yylloc->last_line = yylineno;                                  \
        yylloc->last_column = yytext + yyleng - std::strrchr(yytext, '\n'); \
    }

char* copy_str(char *str) 
{
    char *res = (char*) malloc(strlen(str) + 1);
    std::strcpy(res, str);
    return res;
}

Location to_loc(YYLTYPE* yylocp);

#define LOC to_loc(yylloc)

#define SET(tx, field, value) \
    do { \
        yylval->_type = ast::Token::tx; \
        yylval->field = value; \
    } while(0)


%}
%%

[ \t]                   ; // ignore all whitespaces
[\n]                    ; // ignore all newlines

module                  { RET(MODULE); }
end                     { RET(END); }

let                     { RET(LET); }
struct                  { RET(STRUCT); }
data                    { RET(DATA); }

if                      { RET(IF); }
then                    { RET(THEN); }
else                    { RET(ELSE); }
match                   { RET(MATCH); }

","                     { RET(COMMA); }
"->"                    { RET(ARROW); }
":"                     { RET(COLON); }
";"                     { RET(SEMICOLON); }
"|>"                    { RET(SPIPE); }
"|"                     { RET(PIPE); }
"="                     { RET(EQUALS); }
"$"                     { RET(DOLLAR); }
"\\"                    { RET(BACKSLASH); }

"\("                    { RET(LPAREN); }
"\)"                    { RET(RPAREN); }
"\["                    { RET(LBRACK); }
"\]"                    { RET(RBRACK); }
"\{"                    { RET(LBRACE); }
"\}"                    { RET(RBRACE); }


[0-9]+		            {
                            SET(INT_LIT, int_lit, (new ast::Int_Lit { atoi(yytext), LOC }));
                            RET(INT);
                        }

[0-9]+\.[0-9]+          {   
                            SET(FLOAT_LIT, float_lit, (new ast::Float_Lit { atof(yytext), LOC }));
                            RET(FLOAT);
                        }

[a-zA-Z_][a-zA-Z0-9_]*  {   
                            SET(ID, id, (new ast::Id { String(copy_str(yytext)), LOC }));
                            RET(ID);
                        }

\'[a-zA-Z]\'            {
                            SET(CHAR_LIT, char_lit, (new ast::Char_Lit { yytext[0], LOC }));
                            RET(CHAR);
                        }

\"(\\.|[^"])*\"         {
                            SET(STRING_LIT, string_lit, (new ast::String_Lit { String(copy_str(yytext)), LOC }));
                            RET(STRING);
                        }   

"/*"((\*+[^/*])|([^*]))*\**"*/"     { printf("multiline comment \n");}
"//".*                              { printf("line comment \n");}


%%