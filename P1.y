%{
#include <stdio.h>
#include <stdlib.h>
#include "P1.h"
extern int yylex();
%}

%union {
  struct ast *a;
  long i;
  double d;
  char operator[4];
  struct symbol *s;
}

%token <i> INTEGER
%token <d> DOUBLE
%token <s> IDT

%token ABSTRACT ASSERT BOOLEAN BREAK BYTE CASE CATCH CHAR CLASS CONST CONTINUE DEFAULT DO DOUBLE FLOAT IF INT ELSE END PACKAGE IMPORT STATIC CHARACTER LONG SHORTWHILE RETURN FOR TRY SWITCH PRIVATE PROTECTED PUBLIC

%nonassoc CMP
%right '='
%left '+' '-'
%left '*' '/'

%type <a> list stmt exp ifstmt lvalue impstmt pkgstmt path dostmt whilestmt breakstmt continuestmt returnstmt switchstmt trystmt forstmt vardeclaration modifier type

%start javafile

%%
javafile: list END {exit(0);}
;

list:             {$$ = NULL;}
| stmt list   {if ($2 == NULL)
                     $$ = $1;
                   else
                     $$ = newAst('L', $1, $2);}
| error list  {yyerrok;}
;

stmt: exp ';'
| pkgstmt
| impstmt
| ifstmt
| dostmt
| whilestmt
| forstmt
| switchstmt
| trystmt
| continuestmt;
| breakstmt;
| returnstmt;
;

pkgstmt: PACKAGE IDT ';'  {}
;

impstmt: IMPORT path ';'  {}
| IMPORT STATIC path ';'  {}
;

path: IDT '.' path   {}
| IDT '.' '*'        {}
| IDT                {}
;

modifier: PUBLIC
| PRIVATE
| PROTECTED
;

type: BYTE
| CHAR
| FLOAT
| DOUBLE
| INT
| LONG
| SHORT
;

initializer: exp
| '{' initializer '}'
;

declarator: IDT
| IDT ',' declarator
| IDT '[' exp ']'
| IDT '[' exp ']' ',' declarator
| IDT '=' initializer
| IDT '=' initializer ',' declarator
| IDT'[' exp ']' '=' initializer
| IDT'[' exp ']' '=' initializer ',' declarator
;

vardeclaration: modifier type declarator ';'
;

block: '{' list '}'
;

ifstmt: IF '(' exp ')' block       {}
| IF '(' exp ')' block ELSE block  {}
;

dostmt: DO block WHILE '(' exp ')' ';' {}
;

whilestmt: WHILE '(' exp ')' block {}
;

forstmt: FOR '(' ')' block
;

trystmt:
;

switchstme:
;

returnstmt: RETURN ';' {}
| RETURN exp ';'       {}
;

breakstmt: BREAK ';'   {}
| BREAK IDT ';'        {}
;

continuestmt: CONTINUE ';'    {}
| CONTINUE IDT ';'            {}
;

exp:exp CMP exp {}
| exp '+' exp {$$ = newAst('+', $1, $3);}
| exp '-' exp {$$ = newAst('-', $1, $3);}
| exp '*' exp {$$ = newAst('*', $1, $3);}
| exp '/' exp {$$ = newAst('/', $1, $3);}
| '('exp')' {$$ = $2;}
| INTEGER {$$ = newInt($1);}
| DOUBLE  {$$ = newFlt($1);}
| CHARACTER {}
| lvalue  {$$ = $1;}
| lvalue '=' exp {$$ = newAssign($1, $3);}
;

lvalue:IDT {$$ = newRef($1);}
;
%%
