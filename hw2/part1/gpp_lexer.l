%{
#define KW_AND 1
#define KW_OR 2
#define KW_NOT 3
#define KW_EQUAL 4
#define KW_LESS 5
#define KW_NIL 6
#define KW_LIST 7
#define KW_APPEND 8
#define KW_CONCAT 9
#define KW_SET 10
#define KW_DEF 11
#define KW_FOR 12
#define KW_IF 13
#define KW_EXIT 14
#define KW_LOAD 15
#define KW_DISPLAY 16
#define KW_TRUE 17
#define KW_FALSE 18
#define OP_PLUS 19
#define OP_MINUS 20
#define OP_DIV 21
#define OP_MULT 22
#define OP_OP 23
#define OP_CP 24
#define OP_COMMA 25
#define COMMENT 26
#define VALUEF 27
#define IDENTIFIER 28
#define ERROR 29
#define HALT 30

%}

%%

and                     { return KW_AND;         }  
or                      { return KW_OR;          }
not                     { return KW_NOT;         }
equal                   { return KW_EQUAL;       }
less                    { return KW_LESS;        }
nil                     { return KW_NIL;         }
list                    { return KW_LIST;        }
append                  { return KW_APPEND;      }
concat                  { return KW_CONCAT;      }
set                     { return KW_SET;         }
def                     { return KW_DEF;         }
for                     { return KW_FOR;         }
if                      { return KW_IF;          }
exit                    { return KW_EXIT;        }
load                    { return KW_LOAD;        }
display                 { return KW_DISPLAY;     }
true                    { return KW_TRUE;        }
false                   { return KW_FALSE;       }
halt                    { return HALT;           }

\+                      { return OP_PLUS;        }
\-                      { return OP_MINUS;       }
\/                      { return OP_DIV;         }
\*                      { return OP_MULT;        }
\(                      { return OP_OP;          }
\)                      { return OP_CP;          }
,                       { return OP_COMMA;       }

;;.*                    { return COMMENT;        }
[1-9][0-9]*b[1-9][0-9]* { return VALUEF;         }
[a-zA-Z][a-zA-Z0-9]*    { return IDENTIFIER;     }

[ \t\r]+                {                        }
\n                      {                        }
.                       { return ERROR;          }  
%%

int yywrap() {
    return 1;
}

int main() {
    int x;
    while(x = yylex())  {
            
        if (x == KW_AND) {
            printf("KW_AND\n");
        } else if (x == KW_OR) {
            printf("KW_OR\n");
        } else if (x == KW_NOT) {
            printf("KW_NOT\n");
        } else if (x == KW_EQUAL) {
            printf("KW_EQUAL\n");
        } else if (x == KW_LESS) {
            printf("KW_LESS\n");
        } else if (x == KW_NIL) {
            printf("KW_NIL\n");
        } else if (x == KW_LIST) {
            printf("KW_LIST\n");
        } else if (x == KW_APPEND) {
            printf("KW_APPEND\n");
        } else if (x == KW_CONCAT) {
            printf("KW_CONCAT\n");
        } else if (x == KW_SET) {
            printf("KW_SET\n");
        } else if (x == KW_DEF) {
            printf("KW_DEF\n");
        } else if (x == KW_FOR) {
            printf("KW_FOR\n");
        } else if (x == KW_IF) {
            printf("KW_IF\n");
        } else if (x == KW_EXIT) {
            printf("KW_EXIT\n");
        } else if (x == KW_LOAD) {
            printf("KW_LOAD\n");
        } else if (x == KW_DISPLAY) {
            printf("KW_DISPLAY\n");
        } else if (x == KW_TRUE) {
            printf("KW_TRUE\n");
        } else if (x == KW_FALSE) {
            printf("KW_FALSE\n");
        } else if (x == OP_PLUS) {
            printf("OP_PLUS\n");
        } else if (x == OP_MINUS) {
            printf("OP_MINUS\n");
        } else if (x == OP_DIV) {
            printf("OP_DIV\n");
        } else if (x == OP_MULT) {
            printf("OP_MULT\n");
        } else if (x == OP_OP) {
            printf("OP_OP\n");
        } else if (x == OP_CP) {
            printf("OP_CP\n");
        } else if (x == OP_COMMA) {
            printf("OP_COMMA\n");
        } else if (x == COMMENT) {
            printf("COMMENT\n");
        } else if (x == VALUEF) {
            printf("VALUEF\n");
        } else if (x == IDENTIFIER) {
            printf("IDENTIFIER\n");
        } else if (x == ERROR) {
            printf("ERROR\n");
        }
        else if(x == HALT){
            return 0;
        }

    
   }
    return 0;
}

