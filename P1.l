%option nodefault noyywrap yylineno nounput noinput

%x COMMENT
%x IFILE

EXP ([Ee][-+][0-9]+)

%{
#include "P1.h"
#include "P1Symbol.h"
#include "P1.tab.h"

struct symbol symtab[TABLE_SIZE];
%}

%%
 /* Handle Comments */
"/*"               { BEGIN(COMMENT);}
<COMMENT>"*/"      { BEGIN(INITIAL);}
<COMMENT>([^*]|\n)+|.
<COMMENT><<EOF>>   { return 0;}
"//".*\n           {}

 /* Match Keywords */
"abstract" { return ABSTRACT;}
"assert" { return ASSERT;}
"boolean" { return BOOLEAN;}
"break" { return BREAK;}
"byte" { return BYTE;}
"case" { return CASE;}
"catch" { return CATCH;}
"char" { return CHAR;}
"class" { return CLASS;}
"const" { return CONST;}
"continue" { return CONTINUE;}
"default" { return DEFAULT;}
"do" { return DO;}
"double" { return DOUBLETYPE;}
"else" { return ELSE;}
"enum" { return ENUM;}
"extends" { return EXTENDS;}
"final" { return FINAL;}
"finally" { return FINALLY;}
"float" { return FLOAT;}
"for" { return FOR;}
"goto" { return GOTO;}
"if" { return IF;}
"implements" { return IMPLEMENTS;}
"import" { return IMPORT;}
"instanceof" { return INSTANCEOF;}
"int" { return INT;}
"interface" { return INTERFACE;}
"long" { return LONG;}
"native" { return NATIVE;}
"new" { return NEW;}
"package" { return PACKAGE;}
"private" { return PRIVATE;}
"protected" { return PROTECTED;}
"public" { return PUBLIC;}
"return" { return RETURN;}
"short" { return SHORT;}
"static" { return STATIC;}
"strictfp" { return STRICTFP;}
"super" { return SUPER;}
"switch" { return SWITCH;}
"synchronized" { return SYNCHRONIZED;}
"this" { return THIS;}
"throw" { return THROW;}
"throws" { return THROWS;}
"transient" { return TRANSIENT;}
"try" { return TRY;}
"void" { return VOID;}
"volatile" { return VOLATILE;}
"while" { return WHILE;}
"true" { return TRUE;}
"false" { return FALSE;}
"null" { return NULLSYM;}

 /* Interger */
0[0-7]* |
[1-9][0-9]* |
0[Xx][0-9a-fA-F]+ { yylval.i = atol(yytext);
                   return INTEGER;}

 /* Float */
[0-9]*\.[0-9]+{EXP}?[f|d]? |
[0-9]{EXP} { yylval.d = atof(yytext);
            return DOUBLE;}

 /* Character */
\'([^"\\]|\\['"?\\abfnrtv]|\\[0-7]{1,3}|\\[Xx][0-9a-fA-F]+])\' { return CHARACTER;}

 /* String */
\"([^"\\]|\\['"?\\abfnrtv]|\\[0-7]{1,3}|\\[Xx][0-9a-fA-F]+])*\" { return STRING;}


 /* Punctuations */
"{"|"}"|"["|"]"|"("|")"|"."|";"|"," { return yytext[0];}
"++"|"--"  { return PREPOSTFIX;}
"+"|"-"|"~"|"!" { return yytext[0];}
"*"|"/"|"%" { return yytext[0];}
"<<"|">>"|">>>"  {  return SHIFTOP;}
"=="|"!=" { return EQUALITY;}
"<="|">=" { return CMP;}
">"|"<" { return yytext[0];}
"^"|"&"|"|" { return yytext[0];}
"&&" { return LOGICAND;}
"||" { return LOGICOR;}
"?"|":"|"@" { return yytext[0];}
"="  { return yytext[0];}
"+="|"-="|"*="|"/+"|"%="|"&="|"^="|"|="|"<<="|">>="|">>>="  { return ASSIGN;}


 /* Identifier */
[a-zA-Z_$][a-zA-Z0-9_$]*    { struct symbol *ptr = (struct symbol *)malloc(sizeof(struct symbol));
                             ptr->name = strdup(yytext);
			     yylval.s = ptr; return IDT;}


 /* Whitespace */
[\n\t ]+

 /*End of file*/
 <<EOF>> { return END;}

 /* Invalid character */
. {yyerror("Myth\t%c", yytext[0]);}
%%
