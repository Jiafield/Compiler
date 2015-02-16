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

%type<a> javafile pkgdcl imports importdcl types typedcl qualifiedidt importpath classDcl interfaceDcl modifiers normalclassDcl enumdcl typeParametersList extendslist implementslist classbody classBodyDcls classBodyDcl typeParameters javatype typelist modifier annotation memberDcl block methodOrFieldDcl voidMethodDclRest constructorDclRest genericMethodOrConstructorDcl methodOrFieldRest basictype idts dimExps exp exp1 assignOp exp2 exp1Rest exp2Rest infixCat exp3 fieldDclsRest methodDclRest varDcls varDcl varDclRest varInitializer arrayInitializer sqBrackets formalParameters formalParameterDcls formalParameterDclsRest varDclId throwlist qualifiedidtlist varInitializers blockStmts blockStmt localVarDclStmt localVarDcl stmt parExp switchBlockStmtGroups forControl catches finally resourceSpec resources resource catchType catchClause switchBlockStmtGroup switchLabels switchLabel forInit forUpdate infixOp prefixOp postfixOp postfixOps primary selectors selector literal args superSuffix idtSuffix exps creator arrayCreatorRest classCreatorRest innerCreator normalinterfaceDcl extendstypelist enumBody interfacebody parameterlist typeparameter bound genericMethodOrConstructorRest annotationtypedcl annotationMethodRest annotationMethodOrConstantRest annotationTypeElementRest annotationTypeElementDcl annotationTypeElementDcls annotationtypebody elementValues elementValueArrayInitializer elementValue elementValuePair elementValuePairs annotationElement annotations interfaceBodyDcls interfaceBodyDcl interfaceMemberDcl interfaceMethodOrFieldDcl interfaceMethodOrFieldRest constantDclsRest constantDclRest constantDcl constantDcls interfaceMethodDclRest voidInterfaceMethodDclRest interfaceGenericMethodDcl

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
javafile: pkgdcl imports types END   { globalRoot = newRoot($1, $2, $3); return 0;}
;

pkgdcl:                                 { $$ = newNode(PACKAGE0, 0);}
| PACKAGE qualifiedidt ';'              { Node *t = newLeaf("package");
                                          $$ = newNode(PACKAGE1, 2, t, $2);}
| annotations PACKAGE qualifiedidt ';'  { $$ = newNode(TEMP, 0);}
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

importpath: qualifiedidt '.' '*'     { Node *t = newLeaf("*"); 
                                       $$ = newNode(IMPORTPATH1, 2, $1, t);}
| qualifiedidt                       { $$ = newNode(IMPORTPATH2, 1, $1);}
;

types:          { $$ = newNode(TYPES, 0);}
| types typedcl { $$ = $1; addChild($$, $2);}
;

typedcl: classDcl         { $$ = newNode(TYPEDCL1, 1, $1);}
| interfaceDcl            { $$ = newNode(TYPEDCL2, 1, $1);}
| modifiers classDcl      { $$ = newNode(TYPEDCL3, 2, $1, $2);}
| modifiers interfaceDcl  { $$ = newNode(TYPEDCL4, 2, $1, $2);}
;

classDcl: normalclassDcl  { $$ = newNode(CLASSDCL1, 1, $1);}
| enumdcl                 { $$ = newNode(CLASSDCL2, 1, $1);}
;

normalclassDcl: CLASS IDT typeParametersList extendslist implementslist classbody 
{ Node *t1 = newLeaf("class");
  Node *t2 = newLeaf($2->name);
  $$ = newNode(NORMALCLASSDCL, 6, t1, t2, $3, $4, $5, $6); }
;

enumdcl: ENUM IDT implementslist enumBody  
{ Node *t1 = newLeaf("enum"); 
  Node *t2 = newLeaf($2->name);
  $$ = newNode(ENUMDCL, 4, t1, t2, $3, $4); }
;

normalinterfaceDcl: INTERFACE IDT typeParametersList extendstypelist interfacebody 
{ Node *t1 = newLeaf("interface"); 
  Node *t2 = newLeaf($2->name);
  $$ = newNode(NORMALINTERFACEDCL, 5, t1, t2, $3, $4, $5);
}
;

annotationtypedcl: '@' INTERFACE IDT annotationtypebody { $$ = newNode(TEMP, 0);}
;

typeParametersList:   {$$ = newNode(NoParam, 0);}
| typeParameters      {$$ = newNode(TYPEPARAMETERS, 0);}
;

extendslist:          {$$ = newNode(NoExtend, 0);}
| EXTENDS javatype    {Node *t = newLeaf("extends");
                       $$ = newNode(EXTENDSLIST, 1, t, $2);}
;

extendstypelist:     { $$ = newNode(NOEXTENDSTYPELIST, 0);}
| EXTENDS typelist   { Node *t = newLeaf("extends"); $$ = newNode(EXTENDSTYPELIST, 2, t, $2);}
;

implementslist:         { $$ = newNode(NoImplements, 0);}
| IMPLEMENTS typelist   { Node *t = newLeaf("implements");
                          $$ = newNode(IMPLEMENTSLIST, 1, t, $2);}
;

typeParameters: '<' parameterlist '>'  { $$ = $2; }
;

parameterlist: typeparameter       { $$ = newNode(TYPEPARAMETERLIST, 1, $1); }
| parameterlist ',' typeparameter  { $$ = $1; addChild($$, $3);}
;

typeparameter: IDT    { $$ = newLeaf($1->name); }
| IDT EXTENDS bound   { Node *t1 = newLeaf($1->name); Node *t2 = newLeaf("extends"); $$ = newNode(TYPEPARAMETER, 3, t1, t2, $3); }
;

typelist: idts        { $$ = newNode(TYPELIST, 1, $1); }
| typelist ',' idts   { $$ = $1; addChild($$, $3); }
;

bound: idts         { $$ = newNode(BOUND, 1, $1); }
| bound '&' idts    { $$ = $1; addChild($$, $3); }
;

interfaceDcl: normalinterfaceDcl  { $$ = $1; }
| annotationtypedcl               { $$ = $1; }
;

qualifiedidt: IDT        { Node *t = newLeaf($1->name);
                           $$ = newNode(QUALIFIEDIDT, 1, t);}
| qualifiedidt '.' IDT   { Node *t = newLeaf($3->name);
                           $$ = $1; addChild($$, t);}  
;

qualifiedidtlist: qualifiedidt        { $$ = newNode(QUALIFIEDIDTLIST1, 1, $1); }
| qualifiedidtlist ',' qualifiedidt   { $$ = newNode(QUALIFIEDIDTLIST2, 2, $1, $3); }
;

modifiers: modifier    { $$ = newNode(MODIFIERS, 1, $1);}
| modifiers modifier   { $$ = $1; addChild($$, $2); }
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

annotations: annotation   { $$ = newNode(TEMP, 0);}
| annotations annotation  { $$ = newNode(TEMP, 0);}
;

annotation: '@' qualifiedidt                   { $$ = newNode(TEMP, 0);}
| '@' qualifiedidt '(' annotationElement ')'   { $$ = newNode(TEMP, 0);}
;

annotationElement:      { $$ = newNode(TEMP, 0);}
| elementValuePairs     { $$ = newNode(TEMP, 0);}
| elementValue          { $$ = newNode(TEMP, 0);}
;

elementValuePairs: elementValuePair         { $$ = newNode(TEMP, 0);}
| elementValuePairs ',' elementValuePair    { $$ = newNode(TEMP, 0);}
;

elementValuePair: IDT '=' elementValue   { $$ = newNode(TEMP, 0);}
;

elementValue: annotation         { $$ = newNode(TEMP, 0);}
| exp1                           { $$ = newNode(TEMP, 0);}
| elementValueArrayInitializer   { $$ = newNode(TEMP, 0);}
;

elementValueArrayInitializer: '{' '}'   { $$ = newNode(TEMP, 0);}
| '{' elementValues '}'                 { $$ = newNode(TEMP, 0);}
| '{' elementValues ',' '}'             { $$ = newNode(TEMP, 0);}
;

elementValues: elementValue         { $$ = newNode(TEMP, 0);}
| elementValues ',' elementValue    { $$ = newNode(TEMP, 0);}
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

classBodyDcl: ';'      { $$ = newNode(CLASSBODY0, 0); }
| memberDcl            { $$ = newNode(CLASSBODY1, 1, $1); }
| modifiers memberDcl  { $$ = newNode(CLASSBODY2, 2, $1, $2); }
| block                { $$ = newNode(CLASSBODY3, 1, $1); }
| STATIC block         { Node *t = newLeaf("static");
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

methodOrFieldRest: fieldDclsRest ';'   { $$ = $1; }
| methodDclRest                        { $$ = $1; }
;

fieldDclsRest: varDclRest   { $$ = $1; }
| varDclRest ',' varDcls    { $$ = newNode(FIELDDCLSREST, 2, $1, $3); }
;

methodDclRest: formalParameters sqBrackets throwlist block { $$ = newNode(METHODDCLREST1, 4, $1, $2, $3, $4); }
| formalParameters sqBrackets throwlist ';'                { $$ = newNode(METHODDCLREST2, 3, $1, $2, $3); }
;

voidMethodDclRest: formalParameters throwlist block  { $$ = newNode(VOIDMETHODDCLREST1, 3, $1, $2, $3);}
| formalParameters throwlist ';'                     { $$ = newNode(VOIDMETHODDCLREST2, 2, $1, $2);}
;

constructorDclRest: formalParameters throwlist block  { $$ = newNode(CONSTRUCTORDCLREST, 3, $1, $2, $3);} 
;

genericMethodOrConstructorDcl: typeParameters genericMethodOrConstructorRest    { $$ = newNode(GENERICMETHODORCONSTRUCTORDCL, 2, $1, $2);}
;

genericMethodOrConstructorRest: javatype IDT methodDclRest  
{ Node *t = newLeaf($2->name);
  $$ = newNode(GENERICREST1, 3, $1, t, $3);
}
| VOID IDT methodDclRest 
{ Node *t1 = newLeaf("void");
  Node *t2 = newLeaf($2->name);
  $$ = newNode(GENERICREST2, 3, t1, t2, $3);
}
| IDT constructorDclRest  
{ Node *t = newLeaf($1->name);
  $$ = newNode(GENERICREST3, 2, t, $2);
}
;

throwlist:                   { $$ = newNode(NOTHROW, 0); }
| THROWS qualifiedidtlist    { Node *t = newLeaf("throws");
                               $$ = newNode(THROWLIST, 2, t, $2);}
;

interfacebody: '{' interfaceBodyDcls '}'  { $$ = $2; }  
;

interfaceBodyDcls: interfaceBodyDcl   { $$ = newNode(INTERFACEBODYDCLS, 1, $1); }
| interfaceBodyDcls interfaceBodyDcl  { $$ = $1; addChild($$, $2); }
;

interfaceBodyDcl: ';'           { $$ = newLeaf(";"); }
| interfaceMemberDcl            { $$ = $1; }
| modifiers interfaceMemberDcl  { $$ = newNode(INTERFACEBODY, 2, $1, $2); }
;

interfaceMemberDcl: interfaceMethodOrFieldDcl   { $$ =$1; }
| VOID IDT voidInterfaceMethodDclRest           { Node *t1 = newLeaf("void"); Node *t2 = newLeaf($2->name); $$ = newNode(INTERFACEMEMBER, 3, t1, t2, $3); }
| interfaceGenericMethodDcl   { $$ = $1; }
| classDcl                    { $$ = $1; }
| interfaceDcl                { $$ = $1; }
;

interfaceMethodOrFieldDcl: javatype IDT interfaceMethodOrFieldRest 
{ Node *t = newLeaf($2->name);
  $$ = newNode(INTERFACEMETHODORFIELD, 3, $1, t, $3);
} 
;

interfaceMethodOrFieldRest: constantDclsRest ';' { $$ = $1; }
| interfaceMethodDclRest                         { $$ = $1; }
;

constantDclsRest: constantDclRest   { $$ = $1; }
| constantDclRest ',' constantDcls  { $$ = newNode(CONSTANTDCLSREST, 2, $1, $3); }
;

constantDclRest: sqBrackets '=' varInitializer    { $$ = newNode(CONSTANTDCLREST, 2, $1, $3);}
;

constantDcl: IDT constantDclRest   { Node *t = newLeaf($1->name); $$ = newNode(CONSTANTDCL, 2, t, $2); }
;

constantDcls:                   { $$ = newNode(CONSTANTDCLS, 0); } 
| constantDcls ',' constantDcl  { $$ = $1; addChild($$, $3); }
;

interfaceMethodDclRest: formalParameters sqBrackets throwlist ';'  
{ $$ = newNode(INTERFACEMETHODDCLREST, 3, $1, $2, $3); }
;

voidInterfaceMethodDclRest: formalParameters throwlist ';'  
{ $$ = newNode(VOIDINTERFACEMETHODREST, 2, $1, $2); }  
;

interfaceGenericMethodDcl: typeParameters javatype IDT interfaceMethodDclRest   
{ Node *t = newLeaf($3->name); $$ = newNode(INTERFACEGENERICMETHOD1, 4, $1, $2, t, $4); }
| typeParameters VOID IDT interfaceMethodDclRest    
{ Node *t1 = newLeaf("void"); Node *t2 = newLeaf($3->name); $$ = newNode(INTERFACEGENERICMETHOD2, 4, $1, t1, t2, $4); }
;

sqBrackets:           { $$ = newNode(SQBRACKETS, 0); }
| sqBrackets '[' ']'  { $$ = $1; Node *t1 = newLeaf("["); 
                        Node *t2 = newLeaf("]");
                        addChild($$, t1);
                        addChild($$, t2);
  }
;

formalParameters: '(' ')'      { Node *t1 = newLeaf("(");
                                 Node *t2 = newLeaf(")");
                                 $$ = newNode(FORMALPARAMETERS1, 2, t1, t2); }
| '(' formalParameterDcls ')'  { Node *t1 = newLeaf("(");
                                 Node *t2 = newLeaf(")");
                                 $$ = newNode(FORMALPARAMETERS2, 3, t1, $2, t2); }   
;

formalParameterDcls: javatype formalParameterDclsRest { $$ = newNode(FORMALPARAMETERDCLS1, 2, $1, $2);}
| modifiers javatype formalParameterDclsRest          { $$ = newNode(FORMALPARAMETERDCLS2, 3, $1, $2, $3);}
;

formalParameterDclsRest: varDclId   { $$ = $1; }
| varDclId ',' formalParameterDcls  { $$ = newNode(FORMALPARAMETERSDCLREST1, 2, $1, $3); }
| '.' '.' '.' varDclId              { Node *t1 = newLeaf(".");
                                      Node *t2 = newLeaf(".");
                                      Node *t3 = newLeaf(".");
                                      $$ = newNode(FORMALPARAMETERSDCLREST2, 4, t1, t2, t3, $4);}
;

varDclId: IDT sqBrackets   { Node *t = newLeaf($1->name); $$ = newNode(VARDCLID, 2, t, $2);}
;

varDcls: varDcl   { $$ = newNode(VARDCLS, 1, $1); }
| varDcls varDcl  { $$ = $1; addChild($$, $2); }
;

varDcl: IDT varDclRest   { Node *t = newLeaf($1->name); 
                           $$ = newNode(VARDCL, 2, t, $2);}
;

varDclRest: sqBrackets              { $$ = $1; }
| sqBrackets '=' varInitializer     { Node *t = newLeaf("="); 
                                      $$ = newNode(VARDCLREST, 3, $1, t, $3);}
;

varInitializer: arrayInitializer   { $$ = $1; }
| exp                              { $$ = $1; }
;

varInitializers: varInitializer       { $$ = newNode(VARINITIALIZERS1, 1, $1); }
| varInitializers ',' varInitializer  { $$ = newNode(VARINITIALIZERS2, 2, $1, $3);}
;

arrayInitializer: '{' '}'      { $$ = newNode(EMPTYARRAY, 0); }
| '{' varInitializers '}'      { $$ = $2; }
| '{' varInitializers ',' '}'  { Node *t = newLeaf(","); $$ = newNode(VARINITIALIZER2, 2, $2, t); }
;

block: '{' blockStmts '}'   { $$ = $2; }
;

blockStmts:                { $$ = newNode(BLOCKSTMTS, 0); }
| blockStmts blockStmt     { $$ = $1; addChild($$, $2); }
;

blockStmt: localVarDclStmt   { $$ = $1; }
| typedcl                    { $$ = $1; }
| stmt                       { $$ = $1; }
;

localVarDclStmt: localVarDcl ';'   { $$ = $1; }
;

localVarDcl: javatype varDcls    { $$ = newNode(LOCALVARDCL1, 2, $1, $2);}
| modifiers javatype varDcls     { $$ = newNode(LOCALVARDCL2, 3, $1, $2, $3); }
;

stmt: block                     { $$ = $1; }
| ';'                           { $$ = newLeaf(";"); }
| IDT ':' stmt                  { Node *t = newLeaf($1->name); $$ = newNode(STMT1, 3, $1, t, $3);}
| exp ';'                       { $$ = $1; }
| IF parExp stmt                { Node *t = newLeaf("if"); $$ = newNode(STMT2, 3, t, $2, $3); }
| IF parExp stmt ELSE stmt      { Node *t1 = newLeaf("if"); Node *t2 = newLeaf("else"); $$ = newNode(STMT3, 5, t1, $2, $3, t2, $5);}
| ASSERT exp ';'                { Node *t = newLeaf("assert"); $$ = newNode(STMT4, 2, t, $2);}
| ASSERT exp ':' exp ';'        { Node *t1 = newLeaf("assert"); Node *t2 = newLeaf(":"); $$ = newNode(STMT5, 4, t1, $2, t2, $4);}
| SWITCH parExp '{' switchBlockStmtGroups '}'   { Node *t = newLeaf("switch"); $$ = newNode(STMT6, 3, t, $2, $4);}
| WHILE parExp stmt             { Node *t = newLeaf("while"); $$ = newNode(STMT7, 3, t, $2, $3);}
| DO stmt WHILE parExp ';'      { Node *t1 = newLeaf("do"); Node *t2 = newLeaf("while"); $$ = newNode(STMT8, 4, t1, $2, t2, $4); }
| FOR '(' forControl ')' stmt   { Node *t = newLeaf("for"); $$ = newNode(STMT9, 3, t, $3, $5);}
| BREAK ';'                     { $$ = newLeaf("break");}
| BREAK IDT ';'                 { Node *t1 = newLeaf("break"); Node *t2 = newLeaf($2->name); $$ = newNode(STMT10, 2, t1, t2); }
| CONTINUE ';'                  { $$ = newLeaf("continue");}
| CONTINUE IDT ';'              { Node *t1 = newLeaf("continue"); Node *t2 = newLeaf($2->name); $$ = newNode(STMT11, 2, t1, t2); }
| RETURN ';'                    { $$ = newLeaf("return");}
| RETURN exp ';'                { Node *t = newLeaf("return"); $$ = newNode(STMT12, 2, t, $2);}
| THROW exp ';'                 { Node *t = newLeaf("throw"); $$ = newNode(STMT13, 2, t, $2);}
| SYNCHRONIZED parExp block     { Node *t = newLeaf("synchronized"); $$ = newNode(STMT14, 3, t, $2, $3);}
| TRY block catches             { Node *t = newLeaf("try"); $$ = newNode(STMT15, 3, t, $2, $3);}
| TRY block finally             { Node *t = newLeaf("try"); $$ = newNode(STMT16, 3, t, $2, $3);}
| TRY block catches finally     { Node *t = newLeaf("try"); $$ = newNode(STMT17, 4, t, $2, $3, $4);}
| TRY resourceSpec block        { Node *t = newLeaf("try"); $$ = newNode(STMT18, 3, t, $2, $3);}
| TRY resourceSpec block catches  { Node *t = newLeaf("try"); $$ = newNode(STMT19, 4, t, $2, $3, $4);}
| TRY resourceSpec block finally   { Node *t = newLeaf("try"); $$ = newNode(STMT20, 4, t, $2, $3, $4);}
| TRY resourceSpec block catches finally   { Node *t = newLeaf("try"); $$ = newNode(STMT21, 5, t, $2, $3, $4, $5);}
;

catches: catchClause    { $$ = newNode(CATCHES, 1, $1); }
| catches catchClause   { $$ = $1; addChild($$, $2); }

catchClause: CATCH '(' modifiers catchType IDT ')' block 
{ Node *t1 = newLeaf("catch");
  Node *t2 = newLeaf($5->name);
  $$ = newNode(CATCHCLAUSE1, 5, t1, $3, $4, t2, $7);
}
| CATCH '(' catchType IDT ')' block 
{ Node *t1 = newLeaf("catch");
  Node *t2 = newLeaf($4->name);
  $$ = newNode(CATCHCLAUSE2, 4, t1, $3, t2, $6);
}
;

catchType: qualifiedidt       { $$ = newNode(CATCHTYPE, 1, $1); }
| catchType '|' qualifiedidt  { $$ = $1; addChild($$, $3); }
;

finally: FINALLY block    { Node *t = newLeaf("finally"); 
                            $$ = newNode(FINALLYCLAUSE, 2, t, $2);}
;

resourceSpec: '(' resources ')'  { $$ = newNode(RESOURCESPEC1, 1, $2); }
| '(' resources ';' ')'          { Node *t = newLeaf(";"); $$ = newNode(RESOURCESPEC2, 2, $2, t); }
;

resources: resource        { $$ = newNode(RESOURCES, 1, $1); }
| resources ';' resource   { $$ = $1, addChild($$, $3); }
;

resource:  idts varDclId '=' exp   { Node *t = newLeaf("="); $$ = newNode(RESOURCE1, 4, $1, $2, t, $4); }
| modifiers idts varDclId '=' exp  { Node *t = newLeaf("="); $$ = newNode(RESOURCE2, 5, $1, $2, $3, t, $5); }
;

switchBlockStmtGroups:     { $$ = newNode(SWITCHGROUPS, 0); }
| switchBlockStmtGroups switchBlockStmtGroup
{ $$ = $1; addChild($$, $2); }
;

switchBlockStmtGroup: switchLabels blockStmts { $$ = newNode(SWITCHGROUP, 2, $1, $2); }
;

switchLabels: switchLabel       { $$ = newNode(SWITCHLABELS, 1, $1); }
| switchLabels switchLabel      { $$ = $1; addChild($$, $2);}
;

switchLabel: CASE exp ':'    { Node *t = newLeaf("case"); $$ = newNode(SWITCHLABEL1, 2, t, $2); }
| CASE IDT ':'   { Node *t1 = newLeaf("case"); Node *t2 = newLeaf($2->name); $$ = newNode(SWITCHLABEL2, 2, t1, t2); }
| DEFAULT ':'    { Node *t = newLeaf("default"); $$ = newNode(SWITCHLABEL3, 1, t);}
;

forControl: ';' ';'              { $$ = newNode(FORCONTROL1, 0); }
| forInit ';' ';'                { $$ = newNode(FORCONTROL2, 1, $1);}
| forInit ';' exp ';'            { $$ = newNode(FORCONTROL3, 2, $1, $3);}
| forInit ';' exp ';' forUpdate  { $$ = newNode(FORCONTROL4, 3, $1, $3, $5);}
| forInit ';' ';' forUpdate      { $$ = newNode(FORCONTROL5, 2, $1, $4);}
| localVarDcl ':' exp            { $$ = newNode(FORCONTROL6, 2, $1, $3);}
;

forInit: localVarDcl          { $$ = newNode(FORINIT, 1, $1); }
| exp                         { $$ = newNode(FORINIT, 1, $1); }
| forInit ',' exp             { $$ = $1; addChild($$, $3); }
| forInit ',' localVarDcl     { $$ = $1; addChild($$, $3); }
;

forUpdate: exp          { $$ = newNode(FORUPDATE, 1, $1); }
| forUpdate ',' exp     { $$ = $1; addChild($$, $3);}
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

infixCat: infixOp exp3    { $$ = newNode(INFIXCAT1, 2, $1, $2); }
| infixCat infixOp exp3   { $$ = newNode(INFIXCAT2, 3, $1, $2, $3); }
;

infixOp: LOGICOR  { $$ = newLeaf("||"); }
| LOGICAND        { $$ = newLeaf("&&"); }
| '|'             { $$ = newLeaf("|"); }
| '^'             { $$ = newLeaf("^"); }
| '&'             { $$ = newLeaf("&"); }
| CMP             { $$ = newLeaf("compare"); }
| EQUALITY        { $$ = newLeaf("=="); }
| '<'             { $$ = newLeaf("<"); }
| '>'             { $$ = newLeaf(">"); }
| SHIFTOP         { $$ = newLeaf("shift"); }
| '+'             { $$ = newLeaf("+"); }
| '-'             { $$ = newLeaf("-"); }
| '*'             { $$ = newLeaf("*"); }
| '/'             { $$ = newLeaf("/"); }
| '%'             { $$ = newLeaf("%"); }
;

prefixOp: PREPOSTFIX  { $$ = newLeaf("++OR--"); }
| '!'                 { $$ = newLeaf("!"); }
| '~'                 { $$ = newLeaf("~"); }
| '+'                 { $$ = newLeaf("+"); }
| '-'                 { $$ = newLeaf("-"); }
;

postfixOp: PREPOSTFIX  { $$ = newLeaf("++OR--"); }
;

postfixOps:             { $$ = newNode(POSTFIXOPS, 0); }
| postfixOps postfixOp  { $$ = $1; addChild($$, $2); }
;

exp3: prefixOp exp3               {$$ = newNode(EXP3, 2, $1, $2);}
| '(' javatype ')' exp3           {$$ = newNode(EXP4, 2, $2, $4);}
| '(' exp ')' exp3                {$$ = newNode(EXP5, 2, $2, $4);}
| primary selectors postfixOps    {$$ = newNode(EXP6, 3, $1, $2, $3);}
;

primary: literal      { $$ = newNode(PRIMARY1, 1, $1); }
| parExp              { $$ = newNode(PRIMARY2, 1, $1); }
| THIS                { $$ = newLeaf("this"); }
| THIS args           { Node *t = newLeaf("this"); $$ = newNode(PRIMARY3, 2, t, $2);}
| SUPER superSuffix   { Node *t = newLeaf("super"); $$ = newNode(PRIMARY4, 2, t, $2);}
| NEW creator         { Node *t = newLeaf("new"); $$ = newNode(PRIMARY5, 2, t, $2);}
| javatype            { $$ = newNode(PRIMARY6, 1, $1); }
| idts idtSuffix      { $$ = newNode(PRIMARY7, 2, $1, $2); }
| basictype '.' CLASS { Node *t = newLeaf("class"); $$ = newNode(PRIMARY8, 2, $1, t); }
| VOID '.' CLASS      { Node *t1 = newLeaf("void"); Node *t2 = newLeaf("class"); $$ = newNode(PRIMARY9, 2, t1, t2);}
;

literal: INTEGER  { $$ = newLeaf("int"); }
| DOUBLE          { $$ = newLeaf("double"); }
| CHARACTER       { $$ = newLeaf("char"); }
| STRING          { $$ = newLeaf("string"); }
| TRUE            { $$ = newLeaf("true"); }
| FALSE           { $$ = newLeaf("false"); }
| NULLSYM         { $$ = newLeaf("null"); }
;

parExp: '(' exp ')'  { $$ = $2; }
;

args: '('  ')'  { $$ = newNode(NOARGS, 0); }
| '(' exps ')'  { $$ = $2; }
;

exps: exp       { $$ = newNode(EXPS, 1, $1); }
| exps ',' exp  { $$ = $1; addChild($$, $3); }
;

idts: IDT         {Node *t = newLeaf($1->name);
  $$ = newNode(IDTS, 1, t);}
| idts '.' IDT    {$$ = $1; Node *t = newLeaf($3->name); addChild($$, t);}
;

superSuffix: args   { $$ = $1; }
| '.' IDT args      { Node *t = newLeaf($2->name); $$ = newNode(SUPERSUFFIX, 2, t, $3); }
;

creator: idts classCreatorRest  { $$ = newNode(CREATOR1, 2, $1, $2); }
| javatype arrayCreatorRest     { $$ = newNode(CREATOR2, 2, $1, $2); }
;

classCreatorRest: args   { $$ = $1; }
| args classbody         { $$ = newNode(CLASSCREATORREST, 2, $1, $2); }
;

arrayCreatorRest: arrayInitializer { $$ = $1; }
|                                  { $$ = newNode(TEMP, 0); }
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

idtSuffix: args         { $$ = $1; }
| '.' CLASS             { $$ = newLeaf("class"); }
| '.' THIS              { $$ = newLeaf("this"); }
| '.' SUPER args        { Node *t = newLeaf("super"); $$ = newNode(IDTSUFFIX1, 2, t, $3); }
| '.' NEW innerCreator  { Node *t = newLeaf("new"); $$ = newNode(IDTSUFFIX2, 2, t, $3);}
;

innerCreator: IDT classCreatorRest { Node *t = newLeaf($1->name); $$ = newNode(INNERCREATOR, 2, t, $2);}
;

selector: '.' IDT        { $$ = newLeaf($2->name);}
| '.' IDT args           { Node *t = newLeaf($2->name); $$ = newNode(SELECTOR1, 2, t, $3);}
| '.' THIS               { $$ = newLeaf("this");}
| '.' SUPER superSuffix  { Node *t = newLeaf("super"); $$ = newNode(SELECTOR2, 2, t, $3);}
| '.' NEW innerCreator   { Node *t = newLeaf("new"); $$ = newNode(SELECTOR3, 2, t, $3);}
| '[' exp ']'            { $$ = $2; }
;

selectors:             { $$ = newNode(SELECTORS, 0); }
| selectors selector   { $$ = $1; addChild($$, $2); }
;

enumBody: '{' idts '}'            { $$ = $2; }
| '{' idts ';' '}'                { Node *t = newLeaf(";"); $$ = newNode(ENUMBODY1, 2, $2, t); }
|  '{' idts ';' classBodyDcls '}' { Node *t = newLeaf(";"); $$ = newNode(ENUMBODY2, 3, $2, t, $4);}
;

annotationtypebody: '{' annotationTypeElementDcls '}'   { $$ = newNode(TEMP, 0);}
;

annotationTypeElementDcls:                             { $$ = newNode(TEMP, 0);}
| annotationTypeElementDcls annotationTypeElementDcl   { $$ = newNode(TEMP, 0);}
;

annotationTypeElementDcl: annotationTypeElementRest   { $$ = newNode(TEMP, 0);}
| modifiers annotationTypeElementRest                 { $$ = newNode(TEMP, 0);}
;

annotationTypeElementRest: javatype IDT annotationMethodOrConstantRest ';'    { $$ = newNode(TEMP, 0);}
| classDcl                                                                    { $$ = newNode(TEMP, 0);}
| interfaceDcl                                                                { $$ = newNode(TEMP, 0);}
;

annotationMethodOrConstantRest: annotationMethodRest        { $$ = newNode(TEMP, 0);}
| constantDclsRest                                          { $$ = newNode(TEMP, 0);}
;

annotationMethodRest: '(' ')'             { $$ = newNode(TEMP, 0);}
| '(' ')' '[' ']'                         { $$ = newNode(TEMP, 0);}
| '(' ')' DEFAULT elementValue            { $$ = newNode(TEMP, 0);}
| '(' ')' '[' ']' DEFAULT elementValue    { $$ = newNode(TEMP, 0);}
;
%%
