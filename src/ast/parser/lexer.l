%option outfile="build/parser/lexer.cpp" 
%option header-file="build/parser/lexer.hpp"
%option reentrant bison-bridge
%option noyywrap nounput noinput

%{
#include "parser.hpp"
#define RET(TOKEN) return yyextra = TOKEN

#include "std.hpp"
#include <cstdlib>
#include <cstdio>

#include <cstring>

auto copy_str(char *str) {
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
                            *yylval = { Token::STRING_VAL, std::atoi(yytext) }; 
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