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

%token ABSTRACT ASSERT BOOLEAN BREAK BYTE CASE CATCH CHAR CLASS CONST CONTINUE DEFAULT DO DOUBLETYPE FLOAT IF INT ELSE END PACKAGE IMPORT STATIC CHARACTER LONG SHORT WHILE RETURN FOR TRY SWITCH PRIVATE PROTECTED PUBLIC SUPER EXTENDS FINAL FINALLY NATIVE SYNCHRONIZED TRANSIENT VOLATILE STRICTFP IMPLEMENTS ENUM INTERFACE THROW THROWS VOID

%nonassoc CMP
%right '='
%left '+' '-'
%left '*' '/'

%type <a> importdcl pkgdcl typedcl classOrInterfaceDcl classDcl normalclassDcl enumdcl interfaceDcl normalinterfaceDcl annotationtypedcl importpath modifiers modifier javatype basictype reference referencetype typeargs typearglist typearg annotation annotations annotationElement elementValuePairs elementValuePair elementValue elementValueArrayInitializer elementValues qualifiedidt qualifiedidtlist typeParameters typeParametersList parameterlist typeparameter bound typelist extendslist implementslist extendstypelist interfacebody annotationtypebody nonWildcardTypeArgs typeargsordiamond nonWildcardTypeArgsOrDiamond classbody classBodyDcls classBodyDcl memberDcl block methodOrFieldDcl methodOrFieldRest fieldDclsRest methodDclRest voidMethodDclRest constructorDclRest genericMethodOrConstructorRest genericMethodOrConstructorDcl throwlist interfaceBodyDcl interfaceMemberDcl interfaceMethodOrFieldDcl interfaceMethodOrFieldRest constantDclsRest constantDclRest constantDcl constantDcls interfaceMethodDclRest voidInterfaceMethodDclRest interfaceGenericMethodDcl sqBrackets formalParameterDcls varModifiers varModifier formalParameterDclsRest varDclId varDcls varDcl varDclRest varInitializer varInitializers arrayInitializer exp blockStmt blockStmts localVarDclStmt stmt parExp switchBlockStmtGroups forControl catches finally
 
%start javafile

%%
javafile: pkgdcl imports types END      {return 0;}
;

pkgdcl:                                 {}
| PACKAGE qualifiedidt ';'              {}
| annotations PACKAGE qualifiedidt ';'  {}
;

imports:
| imports importdcl
;

importdcl: IMPORT STATIC importpath ';'    {}
| IMPORT importpath ';'  {}
;

importpath: qualifiedidt '.' '*'     {}
| qualifiedidt
;

types:
| types typedcl
;

typedcl: classOrInterfaceDcl
;

classOrInterfaceDcl: modifiers classDcl
| modifiers interfaceDcl
;

classDcl: normalclassDcl
| enumdcl
;

normalclassDcl: CLASS IDT typeParametersList extendslist implementslist classbody {}
;

enumdcl: ENUM IDT implementslist enumbody  {}
;

normalinterfaceDcl: INTERFACE IDT typeParametersList extendstypelist interfacebody {}
;

annotationtypedcl: '@' INTERFACE IDT annotationtypebody {}
;

typeParametersList:   {}
| typeParameters      {}

extendslist:          {}
| EXTENDS javatype    {}
;

extendstypelist:     {}
| EXTENDS typelist   {}
;

implementslist:         {}
| IMPLEMENTS typelist   {}
;

typeParameters: '<' parameterlist '>'    {}
;

parameterlist: typeparameter
| parameterlist ',' typeparameter
;

typeparameter: IDT    {}
| IDT EXTENDS bound   {}
;

typelist: referencetype
| typelist ',' referencetype
;

bound: referencetype
| referencetype '&' bound
;

interfaceDcl: normalinterfaceDcl
| annotationtypedcl
;

qualifiedidt: IDT            {}
| qualifiedidt '.' IDT       {}
;

qualifiedidtlist: qualifiedidt        {}
| qualifiedidtlist ',' qualifiedidt   {}
;

modifiers:                  {}
| modifiers modifier
;

modifier: PUBLIC {}
| PRIVATE        {}
| PROTECTED      {}
| STATIC         {}
| ABSTRACT       {}
| FINAL          {}
| NATIVE         {}
| SYNCHRONIZED   {}
| TRANSIENT      {}
| VOLATILE       {}
| STRICTFP       {}
| annotation
;

annotations: annotation
| annotations annotation
;

annotation: '@' qualifiedidt  {}
| '@' qualifiedidt '(' annotationElement ')'    {}
;

annotationElement:      {}
| elementValuePairs
| elementValue
;

elementValuePairs: elementValuePair
| elementValuePairs ',' elementValuePair  {}
;

elementValuePair: IDT '=' elementValue  {}
;

elementValue: annotation
| expression1
| elementValueArrayInitializer
;

elementValueArrayInitializer: '{' '}'   {}
| '{' elementValues '}'             {}
| '{' elementValues ',' '}'             {}
;

elementValues: elementValue
| elementValues ',' elementValue   {}
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

reference: IDT       {}
| IDT typeargs       {}
;

referencetype: reference
| referencetype '.' reference
;

typeargs: '<' typearglist '>'   {}
;

typearglist: typearg
| typearglist ',' typearg
;

typearg: referencetype       {}
| '?'                        {}
| '?' EXTENDS referencetype  {}
| '?' SUPER referencetype    {}
;

nonWildcardTypeArgs: '<' typelist '>'  {}
;

typeargsordiamond: '<' '>'   {}
| typeargs
;

nonWildcardTypeArgsOrDiamond: '<' '>'   {}
| nonWildcardTypeArgs
;

classbody: '{' classBodyDcls '}'   {} 
;

classBodyDcls:                     {}
| classBodyDcls classBodyDcl
;

classBodyDcl: ';'         {}
| modifiers memberDcl
| block
| STATIC block            {}
;

memberDcl: methodOrFieldDcl
| VOID IDT voidMethodDclRest   {}
| IDT constructorDclRest       {}
| genericMethodOrConstructorDcl
| classDcl
| interfaceDcl
;

methodOrFieldDcl: javatype IDT methodOrFieldRest
;

methodOrFieldRest: fieldDclsRest ';'   {}
| methodDclRest
;

fieldDclsRest: varDclRest
| varDclRest ',' varDcls   {}
;

methodDclRest: formalParameters throwlist block
| formalParameters throwlist ';'                 {}
;

voidMethodDclRest: formalParameters throwlist block
| formalParameters throwlist ';'                            {}
;

constructorDclRest: formalParameters throwlist block
;

genericMethodOrConstructorDcl: typeParameters genericMethodOrConstructorRest
;

genericMethodOrConstructorRest: javatype IDT methodDclRest  {}
| VOID IDT methodDclRest {}
| IDT constructorDclRest {} 
;

throwlist:                   {}
| THROWS qualifiedidtlist    {}
;

enumbody: '{' '}' {}
;

interfacebody: '{' interfaceBodyDcl '}' {}
;

interfaceBodyDcl: ';'           {}
| modifiers interfaceMemberDcl
;

interfaceMemberDcl: interfaceMethodOrFieldDcl
| VOID IDT voidInterfaceMethodDclRest              {}
| interfaceGenericMethodDcl
| classDcl
| interfaceDcl
;

interfaceMethodOrFieldDcl: javatype IDT interfaceMethodOrFieldRest  {}
;

interfaceMethodOrFieldRest: constantDclsRest ';'
| interfaceMethodDclRest
;

constantDclsRest: constantDclRest
| constantDclRest ',' constantDcls
;

constantDclRest: sqBrackets '=' varInitializer    {}
;

constantDcl: IDT constantDclRest {}
;

constantDcls:                  {}
| constantDcls ',' constantDcl {}
;

interfaceMethodDclRest: formalParameters sqBrackets throwlist ';'  {}
;

voidInterfaceMethodDclRest: formalParameters throwlist ';'    {}
;

interfaceGenericMethodDcl: typeParameters javatype IDT interfaceMethodDclRest   {}
| typeParameters VOID IDT interfaceMethodDclRest    {}
;

sqBrackets:           {}
| sqBrackets '[' ']'  {}
;

formalParameters: '(' ')'          {}
| '(' formalParameterDcls ')'      {}
;

formalParameterDcls: varModifiers javatype formalParameterDclsRest
;

varModifier: FINAL      {}
| annotation
;

varModifiers:                  {}
| varModifiers varModifier
; 

formalParameterDclsRest: varDclId
| varDclId ',' formalParameterDcls  {}
| '.' '.' '.' varDclId              {}
;

varDclId: IDT sqBrackets   {}
;

varDcls: varDcl
| varDcls varDcl
;

varDcl: IDT varDclRest   {}
;

varDclRest: sqBrackets
| sqBrackets '=' varInitializer   {}
;

varInitializer: arrayInitializer
| exp
;

varInitializers: varInitializer
| varInitializers ',' varInitializer   {}
;

arrayInitializer: '{' '}'      {}
| '{' varInitializers '}'      {}
| '{' varInitializers ',' '}'  {}
;

block: '{' blockStmts '}'   {}
;

blockStmts:                {}
| blockStmts blockStmt     {}
;

blockStmt: localVarDclStmt
| classOrInterfaceDcl
| stmt
| IDT ':' stmt                 {}
;

localVarDclStmt: varModifiers javatype varDcls
;

stmt: block
| ';'
| IDT ':' stmt
| exp ';'
| IF parExp stmt
| IF parExp stmt ELSE stmt
| ASSERT exp ';'
| ASSERT exp ':' exp ';'
| SWITCH parExp '{' switchBlockStmtGroups '}'
| WHILE parExp stmt
| DO stmt WHILE parExp ';'
| FOR '(' forControl ')' stmt
| BREAK ';'
| BREAK IDT ';'
| CONTINUE ';'
| CONTINUE IDT ';'
| RETURN ';'
| RETURN IDT ';'
| THROW exp ';'
| SYNCHRONIZED parExp block
| TRY block catches
| TRY block finally
| TRY block catches finally
| TRY resourceSpec block
| TRY resourceSpec block catches
| TRY resourceSpec block finally
| TRY resourceSpec block catches finally
;

annotationtypebody: '{' '}'   {}
;

switchBlockStmtGroups: '{' '}'  {}
;

forControl: ';'  {}
;

catches: CATCH   {}
;

finally: FINALLY {}
;

exp: expression1 '='   {}
;

parExp: '(' exp ')'  {}
;

expression1: '+'  {}
;

resourceSpec:
;
%%
