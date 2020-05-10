%{
#include "std.hpp"
#include <cstdlib>
#include <cstdio>

#define YY_DECL int yylex()

#include"parser.hpp"

int lineNum = 0;

#include <cstring>

auto copy_str(char *str) {
    char *res = (char*) malloc(strlen(str) + 1);
    std::strcpy(res, str);
    return res;
}
%}

%option noyywrap

%%

[ \t]                   ; //ignore all whitespaces
[\n]                    { std::printf("\n"); lineNum++;} 

let                     { printf("VOID"); return LET;}


[0-9]+		            {
                            yylval.ival = std::atoi(yytext); 
                            std::printf("NUMBER ");
                            return INT;
                        }

[0-9]+\.[0-9]+          {   
                            yylval.fval = atof(yytext); 
                            std::printf("FLOAT");
                            return FLOAT;
                        }

[a-zA-Z_][a-zA-Z0-9_]*  {   
                            yylval.sval = copy_str(yytext); 
                            printf("ID ");
                            return ID;
                        }

\"(\\.|[^"])*\"         {
                            printf("COMMENT: %s", yytext);
                            return STRING;
                        }   

"/*"((\*+[^/*])|([^*]))*\**"*/"     { printf("multiline comment \n");}
"//".*                              { printf("line comment \n");}


%%