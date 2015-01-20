%{
#include <stdio.h>
#include <stdlib.h>
#include "P1.h"
%}

%union {
  struct ast *a;
  int i;
  double d;
  char operator[4];
  struct symbol *s;
}

%token <i> INTEGER
%token <d> DOUBLE
%token <s> IDT
%token EOL

%nonassoc CMP
%right '='
%left '+' '-'
%left '*' '/'

%type <a> stmt exp lvalue

%start javafile

%%

stmt: exp
;

exp:exp CMP exp {}
| exp '+' exp {$$ = newAst('+', $1, $3);}
| exp '-' exp {$$ = newAst('-', $1, $3);}
| exp '*' exp {$$ = newAst('*', $1, $3);}
| exp '/' exp {$$ = newAst('/', $1, $3);}
| '('exp')' {$$ = $2;}
| INTEGER {$$ = newInt($1);}
| DOUBLE  {$$ = newFlt($1);}
| lvalue
| lvalue '=' exp {$$ = newAssign($1, $3); dumpAst($$, 0);}
;

lvalue:IDT {$$ = newRef($1);}
;

javafile:
| javafile stmt EOL
;
%%
