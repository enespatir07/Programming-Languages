%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gpp.h"

#define OUT_FILE "output.txt"

extern FILE * yyin;     
extern FILE * yyout;   
extern int yyparse();  
extern char * yytext;   
int yylex();


int yyerror(char* str) { 
    fprintf(yyout, "SYNTAX_ERROR  Expression at line is not recognized\n");
    return 0; 
}


%}

%union {
    Valuef valuef;
    char str[99];
}

%start START

%token KW_EXIT KW_DEF

%token OP_PLUS OP_MINUS OP_DIV OP_MULT OP_OP OP_CP OP_COMMA

%token <valuef> VALUEF
%token <str> IDENTIFIER

%type <valuef> EXP
%type <valuef> FUNCTION


%%


START   : START EXP                  { printf("Result: %db%d\n", $2.num, $2.denom); fprintf(yyout, "Result: %db%d\n", $2.num, $2.denom); }
        | START OP_OP KW_EXIT OP_CP  { printf("#Exit\n"); fprintf(yyout, "#Exit\n");  return 0;}
        | START FUNCTION             { printf("#Function\n"); fprintf(yyout, "#Function"); }
        | EXP                        { printf("Result: %db%d\n", $1.num, $1.denom); fprintf(yyout, "Result: %db%d\n", $1.num, $1.denom); }
        | FUNCTION                   { printf("#Function\n"); fprintf(yyout, "#Function"); }
    ;
        


EXP     : OP_OP OP_PLUS EXP EXP OP_CP                   {$$ = addValuef($3, $4);   }
        | OP_OP OP_MINUS EXP EXP OP_CP                  { $$ = subValuef($3, $4);   }     
        | OP_OP OP_MULT EXP EXP OP_CP                   { $$ = multValuef($3, $4); }
        | OP_OP OP_DIV EXP EXP OP_CP                    { $$ = divValuef($3, $4);  }
        | OP_OP IDENTIFIER EXP OP_CP                    {}
        | OP_OP IDENTIFIER EXP EXP OP_CP                {}
        | OP_OP IDENTIFIER EXP EXP EXP OP_CP            {}
        | IDENTIFIER                                    { $$ = createValuef(1,1);}                                   
        | VALUEF                                        { $$ = $1;}
        ;


FUNCTION	: OP_OP  KW_DEF IDENTIFIER EXP OP_CP	  {  }
            | OP_OP  KW_DEF IDENTIFIER IDENTIFIER EXP OP_CP	  {   }
            | OP_OP  KW_DEF IDENTIFIER IDENTIFIER IDENTIFIER EXP OP_CP	  { }
            ;


%%

int main(int argc, char * argv[]) {
    FILE * in_stream = NULL;
    FILE * out_stream = NULL;

    if (argc == 1) {
        /* shell mode */
        in_stream = stdin;
    }
    else {
        in_stream = fopen(argv[1], "r");
        if (!in_stream) {
            printf("File \"%s\" cannot find or open\n", argv[1]);
            return 1;
        }
    }

    out_stream = fopen(OUT_FILE, "w");
    yyin = in_stream;
    yyout = out_stream;
    yyparse();
    fclose(out_stream);
}

