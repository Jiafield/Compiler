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

%token ABSTRACT ASSERT BOOLEAN BREAK BYTE CASE CATCH CHAR CLASS CONST CONTINUE DEFAULT DO DOUBLETYPE FLOAT IF INT ELSE END PACKAGE IMPORT STATIC CHARACTER LONG SHORT WHILE RETURN FOR TRY SWITCH PRIVATE PROTECTED PUBLIC SUPER EXTENDS FINAL FINALLY NATIVE SYNCHRONIZED TRANSIENT VOLATILE STRICTFP IMPLEMENTS ENUM INTERFACE THROW THROWS VOID  THIS NEW STRING TRUE FALSE NULLSYM EQUALITY

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
%left  '.' '(' ')' '[' ']'

%start javafile

%%
javafile: pkgdcl imports types END      {return 0;}
;

pkgdcl:                                 
| PACKAGE qualifiedidt ';'              
| annotations PACKAGE qualifiedidt ';'  
;

imports:
| imports importdcl
;

importdcl: IMPORT STATIC importpath ';'    
| IMPORT importpath ';'  
;

importpath: qualifiedidt '.' '*'     
| qualifiedidt
;

types:
| types typedcl
;

typedcl: classOrInterfaceDcl
;

classOrInterfaceDcl: classDcl
| interfaceDcl
| modifiers classDcl
| modifiers interfaceDcl
;

classDcl: normalclassDcl
| enumdcl
;

normalclassDcl: CLASS IDT typeParametersList extendslist implementslist classbody 
;

enumdcl: ENUM IDT implementslist enumBody  
;

normalinterfaceDcl: INTERFACE IDT typeParametersList extendstypelist interfacebody 
;

annotationtypedcl: '@' INTERFACE IDT annotationtypebody 
;

typeParametersList:   
| typeParameters      

extendslist:          
| EXTENDS javatype    
;

extendstypelist:     
| EXTENDS typelist   
;

implementslist:         
| IMPLEMENTS typelist   
;

typeParameters: '<' parameterlist '>'    
;

parameterlist: typeparameter
| parameterlist ',' typeparameter
;

typeparameter: IDT    
| IDT EXTENDS bound   
;

typelist: referenceType
| typelist ',' referenceType
;

bound: referenceType
| referenceType '&' bound
;

interfaceDcl: normalinterfaceDcl
| annotationtypedcl
;

qualifiedidt: IDT            
| qualifiedidt '.' IDT       
;

qualifiedidtlist: qualifiedidt        
| qualifiedidtlist ',' qualifiedidt   
;

modifiers: modifier    
| modifiers modifier
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

javatype: basictype sqBrackets
| referenceType sqBrackets
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

referenceType: idts
| referenceType '.' idts
;

typeargs: '<' typearglist '>'   
;

typearglist: typearg
| typearglist ',' typearg
;

typearg: referenceType       
| '?'                        
| '?' EXTENDS referenceType  
| '?' SUPER referenceType    
;

/*
nonWildcardTypeArgs: '<' typelist '>'  
;

typeargsordiamond: '<' '>'   
| typeargs
;

nonWildcardTypeArgsOrDiamond: '<' '>'   
| nonWildcardTypeArgs
;
*/
classbody: '{' classBodyDcls '}'    
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

methodDclRest: formalParameters throwlist block
| formalParameters throwlist ';'                 
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

interfacebody: '{' interfaceBodyDcl '}' 
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

formalParameterDcls: formalParameterDclsRest
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
| classOrInterfaceDcl
| stmt
;

localVarDclStmt: FINAL javatype varDcls ';'
| annotation javatype varDcls ';'
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

resource: FINAL referenceType varDclId '=' exp
| annotation referenceType varDclId '=' exp
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
| CASE enumConstantName ':'  
| DEFAULT ':'                
;

enumConstantName: IDT 
;

forControl: forVarControl
| forInit ';' ';'           
| forInit ';' exp ';'            
| forInit ';' exp ';' forUpdate  
| forInit ';' ';' forUpdate      
;

forVarControl: javatype varDclId forVarControlRest
| modifiers javatype varDclId forVarControlRest
;

forVarControlRest: ':' exp  
| forVarDclsRest ';' ';'
| forVarDclsRest ';' exp ';'
| forVarDclsRest ';' exp ';' forUpdate
| forVarDclsRest ';' ';' forUpdate
;

forVarDclsRest:     
| '=' varInitializer  
| ',' varDcls         
| '=' varInitializer ',' varDcls  
;

forInit: exp
| forInit ',' exp    
;

forUpdate: exp
| forUpdate ',' exp    
;

exp: exp1
| exp1 assignOp exp1
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
| primary selectors postfixOps
;

primary: literal
| parExp
| THIS
| THIS args
| SUPER superSuffix
| NEW creator
	 /*| nonWildcardTypeArgs explicitGenericInvocationSuffix
	   | nonWildcardTypeArgs THIS args */
| idts
| idts idtSuffix
| basictype sqBrackets '.' CLASS
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
| IDT typeargs
;

superSuffix: args
| '.' IDT args  
;

/*explicitGenericInvocationSuffix: SUPER superSuffix 
| IDT args 
;
*/

creator: createName classCreatorRest
/*nonWildcardTypeArgs createName classCreatorRest*/
| createName arrayCreatorRest
;

createName: IDT
| createName '.' IDT
	    /*| createName '.' IDT typeargsordiamond
	    IDT typeargsordiamond*/
;

classCreatorRest: args
| args classbody
;

arrayCreatorRest: '.'    
;

idtSuffix: args
| '[' sqBrackets '.' CLASS
| '[' exp ']'
| '.' CLASS
	   //| '.' explicitGenericInvocation
| '.' THIS
| '.' SUPER args
	   //| '.' NEW nonWildcardTypeArgs innerCreatora
| '.' NEW innerCreator
;

//explicitGenericInvocation: nonWildcardTypeArgs explicitGenericInvocationSuffix
//;

innerCreator: IDT classCreatorRest
	      //| IDT nonWildcardTypeArgsOrDiamond classCreatorRest
;

selector: '.' IDT
| '.' IDT args
	  //| '.' explicitGenericInvocation
| '.' THIS
| '.' SUPER superSuffix
| '.' NEW innerCreator
	  //| '.' NEW nonWildcardTypeArgs innerCreator
| '[' exp ']'
;

selectors:
| selectors selector
;

enumBody: '{' enumConstants enumBodyDcl '}'
;

enumConstants:
| enumConstants ',' enumConstant
;

enumConstant: IDT
;

enumBodyDcl: 
| ';'
| ';' classBodyDcls
;

annotationtypebody: '{' annotationTypeElementDcls '}'   
;

annotationTypeElementDcls:
| annotationTypeElementDcls annotationTypeElementDcl
;

annotationTypeElementDcl: annotationTypeElementRest
modifiers annotationTypeElementRest
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
