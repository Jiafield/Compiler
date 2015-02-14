%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "P1.h"
#include "P1Symbol.h"

#define YYDEBUG 1
extern int yylex();
%}

%union {
  Node *a;
  long i;
  double d;
  char operator[4];
  struct symbol *s;
}

%token <i> INTEGER
%token <d> DOUBLE
%token <s> IDT

%token ABSTRACT ASSERT BOOLEAN BREAK BYTE CASE CATCH CHAR CLASS CONST CONTINUE DEFAULT DO DOUBLETYPE FLOAT IF INT ELSE END PACKAGE IMPORT STATIC CHARACTER LONG SHORT WHILE RETURN FOR TRY SWITCH PRIVATE PROTECTED PUBLIC SUPER EXTENDS FINAL FINALLY NATIVE SYNCHRONIZED TRANSIENT VOLATILE STRICTFP IMPLEMENTS ENUM INTERFACE THROW THROWS VOID  THIS NEW STRING TRUE FALSE NULLSYM

%type<a> javafile pkgdcl imports importdcl types typedcl qualifiedidt importpath classDcl interfaceDcl modifiers normalclassDcl enumdcl typeParametersList extendslist implementslist classbody typeParameters javatype typelist

%right '=' ASSIGN
%right '?' ':'
%left LOGICOR
%left LOGICAND
%left '|'
%left '^'
%left '&'
%left EQUALITY
%left CMP '>' '<' INSTANCEOF
%left SHIFTOP
%left '+' '-'
%left '*' '/' '%'
%right NEW
%right '!' '~'
%right PREPOSTFIX 
%left  '.'

%start javafile

%%
javafile: pkgdcl imports types END   {$$ = newRoot($1, $2, $3); dumpTree($$); return 0;}
;

pkgdcl:                                 {$$ = NULL;}
| PACKAGE qualifiedidt ';'              {Node *t = newLeaf("package");
                                         $$ = newNode(PACKAGE1, 2, t, $2);}
| annotations PACKAGE qualifiedidt ';'  {}
;

imports:             {$$ = newNode(IMPORTS, 0);}
| imports importdcl  {$$ = $1; addChild($$, $2);}
;

importdcl: IMPORT STATIC importpath ';'   {Node *t1 = newLeaf("import");
                                           Node *t2 = newLeaf("static");
                                           $$ = newNode(IMPORTDCL1,  3, t1, t2, $3);} 
| IMPORT importpath ';'                   {Node *t1 = newLeaf("import");
                                           $$ = newNode(IMPORTDCL2,  2, t1, $2);}
;

importpath: qualifiedidt '.' '*'     {Node *t = newLeaf("*"); 
   $$ = newNode(IMPORTPATH1, 2, $1, t);}
| qualifiedidt                       {$$ = NULL;}
;

types:          {$$ = newNode(TYPES, 0);}
| types typedcl {$$ = $1; addChild($$, $2);}
;

typedcl: classDcl         {$$ = newNode(TYPEDCL1, 1, $1);}
| interfaceDcl            {$$ = newNode(TYPEDCL2, 1, $1);}
| modifiers classDcl      {$$ = newNode(TYPEDCL3, 2, $1, $2);}
| modifiers interfaceDcl  {$$ = newNode(TYPEDCL4, 2, $1, $2);}
;

classDcl: normalclassDcl  {$$ = newNode(CLASSDCL1, 1, $1);}
| enumdcl                 {$$ = newNode(CLASSDCL2, 1, $1);}
;

normalclassDcl: CLASS IDT typeParametersList extendslist implementslist classbody 
{ Node *t1 = newLeaf("class");
  Node *t2 = newLeaf($2->name);
  $$ = newNode(NORMALCLASSDCL, 6, t1, t2, $3, $4, $5, $6); 
}
;

enumdcl: ENUM IDT implementslist enumBody  
;

normalinterfaceDcl: INTERFACE IDT typeParametersList extendstypelist interfacebody 
;

annotationtypedcl: '@' INTERFACE IDT annotationtypebody 
;

typeParametersList:   {$$ = newNode(NoParam, 0);}
| typeParameters      {$$ = newNode(TYPEPARAMETERS, 0);}
;

extendslist:          {$$ = newNode(NoExtend, 0);}
| EXTENDS javatype    {Node *t = newLeaf("extends");
   $$ = newNode(EXTENDSLIST, 1, t, $2);}
;

extendstypelist:     
| EXTENDS typelist   
;

implementslist:         {$$ = newNode(NoImplements, 0);}
| IMPLEMENTS typelist   {Node *t = newLeaf("implements");
   $$ = newNode(IMPLEMENTSLIST, 1, t, $2);}
;

typeParameters: '<' parameterlist '>'
;

parameterlist: typeparameter
| parameterlist ',' typeparameter
;

typeparameter: IDT    
| IDT EXTENDS bound   
;

typelist: idts
| typelist ',' idts
;

bound: idts
| idts '&' bound
;

interfaceDcl: normalinterfaceDcl
| annotationtypedcl
;

qualifiedidt: IDT        { Node *t = newLeaf($1->name);
   $$ = newNode(QUALIFIEDIDT, 1, t);}
| qualifiedidt '.' IDT   { Node *t = newLeaf($3->name);
   $$ = $1; addChild($$, t);}  
;

qualifiedidtlist: qualifiedidt        
| qualifiedidtlist ',' qualifiedidt   
;

modifiers: modifier    {}
| modifiers modifier   {}
;

modifier: PUBLIC 
| PRIVATE        
| PROTECTED      
| STATIC         
| ABSTRACT       
| FINAL          
| NATIVE         
| SYNCHRONIZED   
| TRANSIENT      
| VOLATILE       
| STRICTFP       
| annotation
;

annotations: annotation
| annotations annotation
;

annotation: '@' qualifiedidt  
| '@' qualifiedidt '(' annotationElement ')'    
;

annotationElement:      
| elementValuePairs
| elementValue
;

elementValuePairs: elementValuePair
| elementValuePairs ',' elementValuePair  
;

elementValuePair: IDT '=' elementValue  
;

elementValue: annotation
| exp1
| elementValueArrayInitializer
;

elementValueArrayInitializer: '{' '}'   
| '{' elementValues '}'             
| '{' elementValues ',' '}'             
;

elementValues: elementValue
| elementValues ',' elementValue   
;

javatype: basictype
| idts
| basictype dimExps
| idts dimExps
;

basictype: BYTE       
| CHAR           
| FLOAT          
| DOUBLETYPE     
| INT            
| LONG           
| SHORT          
| BOOLEAN        
;

classbody: '{' classBodyDcls '}'    {}
;

classBodyDcls:                     
| classBodyDcls classBodyDcl
;

classBodyDcl: ';' 
| memberDcl        
| modifiers memberDcl
| block
| STATIC block            
;

memberDcl: methodOrFieldDcl
| VOID IDT voidMethodDclRest   
| IDT constructorDclRest       
| genericMethodOrConstructorDcl
| classDcl
| interfaceDcl
;

methodOrFieldDcl: javatype IDT methodOrFieldRest
;

methodOrFieldRest: fieldDclsRest ';'   
| methodDclRest
;

fieldDclsRest: varDclRest
| varDclRest ',' varDcls   
;

methodDclRest: formalParameters sqBrackets throwlist block
| formalParameters sqBrackets throwlist ';'                 
;

voidMethodDclRest: formalParameters throwlist block
| formalParameters throwlist ';'                            
;

constructorDclRest: formalParameters throwlist block
;

genericMethodOrConstructorDcl: typeParameters genericMethodOrConstructorRest
;

genericMethodOrConstructorRest: javatype IDT methodDclRest  
| VOID IDT methodDclRest 
| IDT constructorDclRest  
;

throwlist:                   
| THROWS qualifiedidtlist    
;

interfacebody: '{' interfaceBodyDcls '}'
;

interfaceBodyDcls: interfaceBodyDcl
| interfaceBodyDcls interfaceBodyDcl
;

interfaceBodyDcl: ';'
| interfaceMemberDcl       
| modifiers interfaceMemberDcl
;

interfaceMemberDcl: interfaceMethodOrFieldDcl
| VOID IDT voidInterfaceMethodDclRest              
| interfaceGenericMethodDcl
| classDcl
| interfaceDcl
;

interfaceMethodOrFieldDcl: javatype IDT interfaceMethodOrFieldRest  
;

interfaceMethodOrFieldRest: constantDclsRest ';'
| interfaceMethodDclRest
;

constantDclsRest: constantDclRest
| constantDclRest ',' constantDcls
;

constantDclRest: sqBrackets '=' varInitializer    
;

constantDcl: IDT constantDclRest 
;

constantDcls:                  
| constantDcls ',' constantDcl 
;

interfaceMethodDclRest: formalParameters sqBrackets throwlist ';'  
;

voidInterfaceMethodDclRest: formalParameters throwlist ';'    
;

interfaceGenericMethodDcl: typeParameters javatype IDT interfaceMethodDclRest   
| typeParameters VOID IDT interfaceMethodDclRest    
;

sqBrackets:           
| sqBrackets '[' ']'  
;

formalParameters: '(' ')'  
| '(' formalParameterDcls ')'      
;

formalParameterDcls: javatype formalParameterDclsRest
| modifiers javatype formalParameterDclsRest
;

formalParameterDclsRest: varDclId
| varDclId ',' formalParameterDcls  
| '.' '.' '.' varDclId              
;

varDclId: IDT sqBrackets   
;

varDcls: varDcl
| varDcls varDcl
;

varDcl: IDT varDclRest   
;

varDclRest: sqBrackets
| sqBrackets '=' varInitializer   
;

varInitializer: arrayInitializer
| exp
;

varInitializers: varInitializer
| varInitializers ',' varInitializer   
;

arrayInitializer: '{' '}'      
| '{' varInitializers '}'      
| '{' varInitializers ',' '}'  
;

block: '{' blockStmts '}'   
;

blockStmts:                
| blockStmts blockStmt     
;

blockStmt: localVarDclStmt
| typedcl
| stmt
;

localVarDclStmt: localVarDcl ';'
;

localVarDcl: javatype varDcls
| modifiers javatype varDcls
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
| RETURN exp ';'
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

catches: catchClause
| catches catchClause

catchClause: CATCH '(' modifiers catchType IDT ')' block 
| CATCH '(' catchType IDT ')' block 
;

catchType: qualifiedidt
| catchType '|' qualifiedidt
;

finally: FINALLY block 
;

resourceSpec: '(' resources ')'  
| '(' resources ';' ')'
;

resources: resource
| resources ';' resource
;

resource:  idts varDclId '=' exp
| modifiers idts varDclId '=' exp
;

switchBlockStmtGroups:     
| switchBlockStmtGroups switchBlockStmtGroup
;

switchBlockStmtGroup: switchLabels blockStmts
;

switchLabels: switchLabel       
| switchLabels switchLabel
;

switchLabel: CASE exp ':'    
| CASE IDT ':'  
| DEFAULT ':'                
;

forControl: ';' ';'
| forInit ';' ';'           
| forInit ';' exp ';'            
| forInit ';' exp ';' forUpdate  
| forInit ';' ';' forUpdate      
| localVarDcl ':' exp
;

forInit: localVarDcl
| exp
| forInit ',' exp
| forInit ',' localVarDcl   
;

forUpdate: exp
| forUpdate ',' exp    
;

exp: exp1
| exp1 assignOp exp
;

assignOp: '='  
| ASSIGN       
;

exp1: exp2
| exp2 exp1Rest
;

exp1Rest: '?' exp ':' exp1  
;

exp2: exp3
| exp3 exp2Rest
;

exp2Rest: INSTANCEOF javatype
| infixCat
;

infixCat: infixOp exp3
| infixCat infixOp exp3
;

infixOp: LOGICOR
| LOGICAND
| '|'
| '^'
| '&'
| CMP
| EQUALITY
| '<'
| '>'
| SHIFTOP
| '+'
| '-'
| '*'
| '/'
| '%'
;

prefixOp: PREPOSTFIX
| '!'
| '~'
| '+'
| '-'
;

postfixOp: PREPOSTFIX
;

postfixOps:
| postfixOps postfixOp
;

exp3: prefixOp exp3
| '(' javatype ')' exp3
| '(' exp ')' exp3
| primary selectors postfixOps
;

primary: literal
| parExp
| THIS
| THIS args
| SUPER superSuffix
| NEW creator
| javatype
| idts idtSuffix
| basictype '.' CLASS
| VOID '.' CLASS
;

literal: INTEGER
| DOUBLE
| CHARACTER
| STRING
| TRUE
| FALSE
| NULLSYM
;

parExp: '(' exp ')'  
;

args: '('  ')'
| '(' exps ')'
;

exps: exp
| exps ',' exp
;

idts: IDT
| idts '.' IDT
;

superSuffix: args
| '.' IDT args  
;

creator: idts classCreatorRest
| javatype arrayCreatorRest
;

classCreatorRest: args
| args classbody
;

arrayCreatorRest: arrayInitializer
|
;

dimExps: '[' ']'
| '[' exp ']'
| dimExps '[' exp ']'
| dimExps '[' ']'
;

idtSuffix: args
| '.' CLASS
| '.' THIS
| '.' SUPER args
| '.' NEW innerCreator
;

innerCreator: IDT classCreatorRest
;

selector: '.' IDT
| '.' IDT args
| '.' THIS
| '.' SUPER superSuffix
| '.' NEW innerCreator
| '[' exp ']'
;

selectors:
| selectors selector
;

enumBody: '{' idts '}'
| '{' idts ';' '}'
|  '{' idts ';' classBodyDcls '}'
;

annotationtypebody: '{' annotationTypeElementDcls '}'   
;

annotationTypeElementDcls:
| annotationTypeElementDcls annotationTypeElementDcl
;

annotationTypeElementDcl: annotationTypeElementRest
| modifiers annotationTypeElementRest
;

annotationTypeElementRest: javatype IDT annotationMethodOrConstantRest ';'
| classDcl
| interfaceDcl
;

annotationMethodOrConstantRest: annotationMethodRest
| constantDclsRest
;

annotationMethodRest: '(' ')'
| '(' ')' '[' ']'
| '(' ')' DEFAULT elementValue
| '(' ')' '[' ']' DEFAULT elementValue
;
%%
