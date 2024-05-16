%{
	#include "gpp.h"
	#include "parser.tab.h"
	
%}

%%

def                     				{ return KW_DEF;         								 }
exit                    				{ return KW_EXIT;        								 }
						 
\+                      				{ return OP_PLUS;        								 }
\-                      				{ return OP_MINUS;       								 }
\/                      				{ return OP_DIV;         								 }
\*                      				{ return OP_MULT;        								 }
\(                      				{ return OP_OP;          								 }
\)                      				{ return OP_CP;          								 }
,                       				{ return OP_COMMA;       								 }
						 
[a-zA-Z_][a-zA-Z0-9_]*     	 			{ strcpy(yylval.str, yytext); return IDENTIFIER; 		 }
[1-9][0-9]*b[1-9][0-9]*    				{ yylval.valuef = convertStrToValuef(yytext); return VALUEF; }

[ \t\r]+                				{                        								 }
\n                      				{                        								 }
.										{printf("LEXICAL ERROR\n"); return 0; 					 }

%%

int yywrap() {
    return 1;
}


