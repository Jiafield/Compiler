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

%token ABSTRACT ASSERT BOOLEAN BREAK BYTE CASE CATCH CHAR CLASS CONST CONTINUE DEFAULT DO DOUBLETYPE FLOAT IF INT ELSE END PACKAGE IMPORT STATIC CHARACTER LONG SHORT WHILE RETURN FOR TRY SWITCH PRIVATE PROTECTED PUBLIC SUPER EXTENDS FINAL NATIVE SYNCHRONIZED TRANSIENT VOLATILE STRICTFP

%nonassoc CMP
%right '='
%left '+' '-'
%left '*' '/'

%type <a> list stmt exp ifstmt lvalue importdcl pkgdcl typedcl classdcl normalclassdcl enumdcl interfacedcl normalinterfacedcl annotationtypedcl importpath dostmt whilestmt breakstmt continuestmt returnstmt switchstmt trystmt forstmt vardeclaration modifiers modifier javatype basictype referencetype typeargs typearglist typearg annotation annotations qualifiedidt qualifiedidtlist
 
%start javafile

%%
javafile: pkgdcl imports types END      {return 0;}
;

pkgdcl:                                 {}
| PACKAGE qualifiedidt ';'              {}
| annotations PACKAGE qualifiedidt ';'  {}
;

imports:
| importdcl imports
;

importdcl: IMPORT importpath ';'  {}
| IMPORT STATIC importpath ';'    {}
;

importpath: qualifiedidt
| qualifiedidt '.' '*'     {}
;

types:
| typedcl types
;

typedcl: modifiers classdcl
| modifiers interfacedcl
;

classdcl: normalclassdcl
| enumdcl
;

normalclassdcl:IDT {}
;

enumdcl: IDT {}
;

interfacedcl: normalinterfacedcl
| annotationtypedcl
;

normalinterfacedcl: IDT   {}
;

annotationtypedcl: IDT {}
;

qualifiedidt: IDT            {}
| IDT '.' qualifiedidt       {}
;

qualifiedidtlist: qualifiedidt        {}
| qualifiedidt ',' qualifiedidtlist   {}
;

modifiers:                  {}
| modifier modifiers
;

modifier: PUBLIC {}
| PRIVATE        {}
| PROTECTED      {}
| STATIC         {}
| ABSTRACT       {}
| FINAL          {}
| NATIVE         {}
| SYNCHRONIZED    {}
| TRANSIENT      {}
| VOLATILE       {}
| STRICTFP       {}
| annotation
;

annotations: annotation
| annotation annotations
;

annotation: '@' qualifiedidt  {}
;

javatype: basictype
| basictype '[' ']'
| referencetype
| referencetype '[' ']'
;

basictype: BYTE       {}
| CHAR           {}
| FLOAT          {}
| DOUBLETYPE     {}
| INT            {}
| LONG           {}
| SHORT          {}
| BOOLEAN        {}
;

referencetype: IDT      {}
| IDT typeargs          {}
| IDT '.' referencetype {}
| IDT typeargs '.' referencetype     {}
;

typeargs: '<' typearglist '>'   {}
;

typearglist: typearg
| typearg ',' typearglist
;

typearg: referencetype       {}
| '?'                        {}
| '?' EXTENDS referencetype  {}
| '?' SUPER referencetype    {}
;

/*
list:             {$$ = NULL;}
| stmt list   {if ($2 == NULL)
                     $$ = $1;
                   else
                     $$ = newAst('L', $1, $2);}
| error list  {yyerrok;}
;

stmt: exp ';'
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

declarator: IDT
| IDT ',' declarator
| IDT '[' exp ']'
| IDT '[' exp ']' ',' declarator
| IDT '=' initializer
| IDT '=' initializer ',' declarator
| IDT'[' exp ']' '=' initializer
| IDT'[' exp ']' '=' initializer ',' declarator
;

vardeclaration: modifier javatype declarator ';'
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

forstmt: FOR '(' vardeclaration ';' exp ';' exp ')' block {}
;

trystmt: TRY block   {}
;

switchstmt: SWITCH '(' exp ')' block    {}
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
*/
%%
