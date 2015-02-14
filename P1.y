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

%type<a> javafile pkgdcl imports importdcl types typedcl qualifiedidt importpath classDcl interfaceDcl modifiers normalclassDcl enumdcl typeParametersList extendslist implementslist classbody classBodyDcls classBodyDcl typeParameters javatype typelist modifier annotation memberDcl block methodOrFieldDcl voidMethodDclRest constructorDclRest genericMethodOrConstructorDcl methodOrFieldRest basictype idts dimExps exp exp1 assignOp exp2 exp1Rest exp2Rest infixCat exp3

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
javafile: pkgdcl imports types END   {$$ = newRoot($1, $2, $3); return $$;}
;

pkgdcl:                                 {$$ = newNode(PACKAGE0, 0);}
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
| qualifiedidt                       {$$ = newNode(IMPORTPATH2, 1, $1);}
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

modifiers: modifier    {$$ = newNode(MODIFIERS, 1, $1);}
| modifiers modifier   {$$ = $1; addChild($$, $2); }
;

modifier: PUBLIC   {$$ = newLeaf("public");}
| PRIVATE          {$$ = newLeaf("private");}
| PROTECTED        {$$ = newLeaf("protected");}
| STATIC           {$$ = newLeaf("static");}
| ABSTRACT         {$$ = newLeaf("abstract");}
| FINAL            {$$ = newLeaf("final");}
| NATIVE           {$$ = newLeaf("native");}
| SYNCHRONIZED     {$$ = newLeaf("synchronized");}
| TRANSIENT        {$$ = newLeaf("transient");}
| VOLATILE         {$$ = newLeaf("volatile");}
| STRICTFP         {$$ = newLeaf("strictfp");}
| annotation       {$$ = newNode(MODIFIERANNO, 1, $1);}
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

javatype: basictype    {$$ = newNode(JAVATYPE1, 1, $1);}
| idts                 {$$ = newNode(JAVATYPE2, 1, $1);}
| basictype dimExps    {$$ = newNode(JAVATYPE3, 2, $1, $2);}
| idts dimExps         {$$ = newNode(JAVATYPE4, 2, $1, $2);}
;

basictype: BYTE       {$$ = newLeaf("byte");}
| CHAR                {$$ = newLeaf("char");}
| FLOAT               {$$ = newLeaf("float");}
| DOUBLETYPE          {$$ = newLeaf("double");}
| INT                 {$$ = newLeaf("int");}
| LONG                {$$ = newLeaf("long");}
| SHORT               {$$ = newLeaf("short");}
| BOOLEAN             {$$ = newLeaf("boolean");}
;

classbody: '{' classBodyDcls '}'    { $$ = $2; }
;

classBodyDcls:                     { $$ = newNode(CLASSBODYDCLS, 0);}
| classBodyDcls classBodyDcl       { $$ = $1; addChild($$, $2); }
;

classBodyDcl: ';'   { $$ = newNode(CLASSBODY0, 0); }
| memberDcl         { $$ = newNode(CLASSBODY1, 1, $1); }
| modifiers memberDcl  { $$ = newNode(CLASSBODY2, 2, $1, $2); }
| block             { $$ = newNode(CLASSBODY3, 1, $1); }
| STATIC block      { Node *t = newLeaf("static");
   $$ = newNode(CLASSBODY4, 2, t, $2); }
;

memberDcl: methodOrFieldDcl       { $$ = newNode(MEMBERDCL1, 1, $1);}
| VOID IDT voidMethodDclRest      { Node *t1 = newLeaf("void");
   Node *t2 = newLeaf($2->name);
   $$ = newNode(MEMBERDCL2, 3, t1, t2, $3);}
| IDT constructorDclRest          { Node *t = newLeaf($1->name); 
   $$ = newNode(MEMBERDCL3, 2, t, $2);}
| genericMethodOrConstructorDcl   { $$ = newNode(MEMBERDCL4, 1, $1);}
| classDcl                        { $$ = newNode(MEMBERDCL5, 1, $1);}
| interfaceDcl                    { $$ = newNode(MEMBERDCL6, 1, $1);}
;

methodOrFieldDcl: javatype IDT methodOrFieldRest 
{ Node *t = newLeaf($2->name);
  $$ = newNode(METHODORFIELD, 3, $1, t, $3);}
;

methodOrFieldRest: fieldDclsRest ';'   {$$ = newNode(TEMP, 0);}
| methodDclRest                        {$$ = newNode(TEMP, 0);}
;

fieldDclsRest: varDclRest
| varDclRest ',' varDcls   
;

methodDclRest: formalParameters sqBrackets throwlist block
| formalParameters sqBrackets throwlist ';'                 
;

voidMethodDclRest: formalParameters throwlist block  {$$ = newNode(TEMP, 0);}
| formalParameters throwlist ';'                     {$$ = newNode(TEMP, 0);}       
;

constructorDclRest: formalParameters throwlist block  {$$ = newNode(TEMP, 0);} 
;

genericMethodOrConstructorDcl: typeParameters genericMethodOrConstructorRest    {$$ = newNode(TEMP, 0);}
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

block: '{' blockStmts '}'   {$$ = newNode(TEMP, 0);}
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

exp: exp1              { $$ = $1; }
| exp1 assignOp exp    { $$ = newNode(EXP, 3, $1, $2, $3); }
;

assignOp: '='  { $$ = newLeaf("=");}
| ASSIGN       { $$ = newLeaf("assign");}
;

exp1: exp2         { $$ =$1; }
| exp2 exp1Rest    { $$ = newNode(EXP1, 2, $1, $2); }
;

exp1Rest: '?' exp ':' exp1  { Node *t1 = newLeaf("?"); 
                              Node *t2 = newLeaf(":"); 
                              $$ = newNode(EXP1REST, 4, t1, $2, t2, $4);}
;

exp2: exp3         { $$ = $1; }
| exp3 exp2Rest    { $$ = newNode(EXP2, 2, $1, $2); }
;

exp2Rest: INSTANCEOF javatype  { Node *t = newLeaf("instanceof");
                                 $$ = newNode(EXP2REST, 2, t, $2);}
| infixCat               { $$ = newNode(EXP2REST, 1, $1); }
;

infixCat: infixOp exp3    {}
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

idts: IDT         {Node *t = newLeaf($1->name);
  $$ = newNode(IDTS, 1, t);}
| idts '.' IDT    {$$ = $1; Node *t = newLeaf($3->name); addChild($$, t);}
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

dimExps: '[' ']'        { Node *t1 = newLeaf("[");
                          Node *t2 = newLeaf("]");
                          $$ = newNode(DIM1, 2, t1, t2);}
| '[' exp ']'           { Node *t1 = newLeaf("[");
                          Node *t2 = newLeaf("]");
                          $$ = newNode(DIM2, 3, t1, $2, t2);}
| dimExps '[' exp ']'   { $$ = $1; 
                          Node *t1 = newLeaf("[");
                          Node *t2 = newLeaf("]");
                          addChild($$, t1); addChild($$, $3); addChild($$, t2);}
| dimExps '[' ']'       { $$ = $1; 
                          Node *t1 = newLeaf("[");
                          Node *t2 = newLeaf("]");
                          addChild($$, t1); addChild($$, t2);}
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
