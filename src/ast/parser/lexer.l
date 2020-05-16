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

auto copy_str(char *str) 
{
    char *res = (char*) malloc(strlen(str) + 1);
    std::strcpy(res, str);
    return res;
}
%}

%%

[ \t]                   ; //ignore all whitespaces
[\n]                    { std::printf("\n");} 

let                     { printf("VOID"); RET(LET);}


[0-9]+		            {
                            *yylval = ast::Token { ast::Token::STRING_VAL, std::atoi(yytext) }; 
                            std::printf("NUMBER ");
                            RET(INT);
                        }

[0-9]+\.[0-9]+          {   
                            //yylval.fval = atof(yytext); 
                            std::printf("FLOAT");
                            RET(FLOAT);
                        }

[a-zA-Z_][a-zA-Z0-9_]*  {   
                            //yylval.sval = copy_str(yytext); 
                            printf("ID ");
                            RET(ID);
                        }

\"(\\.|[^"])*\"         {
                            //printf("COMMENT: %s", yytext);
                            RET(STRING);
                        }   

"/*"((\*+[^/*])|([^*]))*\**"*/"     { printf("multiline comment \n");}
"//".*                              { printf("line comment \n");}


%%