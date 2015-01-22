%option nodefault noyywrap yylineno nounput noinput

%x COMMENT
%x IFILE

EXP ([Ee][-+][0-9]+)

%{
#include "P1.h"
#include "P1.tab.h"
%}

%%
 /* Handle Comments */
"/*"               {printf("Comment start at line %d\n", yylineno); 
                    BEGIN(COMMENT);}
<COMMENT>"*/"      {printf("Comment end at line %d\n", yylineno);
                    BEGIN(INITIAL);}
<COMMENT>([^*]|\n)+|.
<COMMENT><<EOF>>   {printf("Line %d: Unterminated comment\n", yylineno); return 0;}
"//".*\n           {printf("One line comment at line %d\n", yylineno);}

 /* Match Keywords */
"abstract" {printf("KEY\t%s\t\t%d\n", yytext, yylineno); return ABSTRACT;}
"assert" {printf("KEY\t%s\t\t%d\n", yytext, yylineno); return ASSERT;}
"boolean" {printf("KEY\t%s\t\t%d\n", yytext, yylineno); return BOOLEAN;}
"break" {printf("KEY\t%s\t\t%d\n", yytext, yylineno); return BREAK;}
"byte" {printf("KEY\t%s\t\t%d\n", yytext, yylineno); return BYTE;}
"case" {printf("KEY\t%s\t\t%d\n", yytext, yylineno); return CASE;}
"catch" {printf("KEY\t%s\t\t%d\n", yytext, yylineno); return CATCH;}
"char" {printf("KEY\t%s\t\t%d\n", yytext, yylineno); return CHAR;}
"class" {printf("KEY\t%s\t\t%d\n", yytext, yylineno); return CLASS;}
"const" {printf("KEY\t%s\t\t%d\n", yytext, yylineno); return CONST;}
"continue" {printf("KEY\t%s\t\t%d\n", yytext, yylineno); return CONTINUE;}
"default" {printf("KEY\t%s\t\t%d\n", yytext, yylineno); return DEFAULT;}
"do" {printf("KEY\t%s\t\t%d\n", yytext, yylineno); return DO;}
"double" {printf("KEY\t%s\t\t%d\n", yytext, yylineno); return DOUBLE;}
"else" {printf("KEY\t%s\t\t%d\n", yytext, yylineno); return ELSE;}
"enum" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"extends" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"final" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"finally" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"float" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"for" {printf("KEY\t%s\t\t%d\n", yytext, yylineno); return FOR;}
"goto" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"if" {printf("KEY\t%s\t\t%d\n", yytext, yylineno); return IF;}
"implements" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"import" {printf("KEY\t%s\t\t%d\n", yytext, yylineno); return IMPORT;}
"instanceof" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"int" {printf("KEY\t%s\t\t%d\n", yytext, yylineno); return INT;}
"interface" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"long" {printf("KEY\t%s\t\t%d\n", yytext, yylineno); return LONG;}
"native" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"new" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"package" {printf("KEY\t%s\t\t%d\n", yytext, yylineno); return PACKAGE;}
"private" {printf("KEY\t%s\t\t%d\n", yytext, yylineno); return PRIVATE;}
"protected" {printf("KEY\t%s\t\t%d\n", yytext, yylineno); return PROTECTED;}
"public" {printf("KEY\t%s\t\t%d\n", yytext, yylineno); return PUBLIC;}
"return" {printf("KEY\t%s\t\t%d\n", yytext, yylineno); return RETURN;}
"short" {printf("KEY\t%s\t\t%d\n", yytext, yylineno); return SHORT;}
"static" {printf("KEY\t%s\t\t%d\n", yytext, yylineno); return STATIC;}
"strictfp" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"super" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"switch" {printf("KEY\t%s\t\t%d\n", yytext, yylineno); return SWITCH;}
"synchronized" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"this" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"throw" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"throws" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"transient" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"try" {printf("KEY\t%s\t\t%d\n", yytext, yylineno); return TRY;}
"void" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"volatile" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"while" {printf("KEY\t%s\t\t%d\n", yytext, yylineno); return WHILE;}

 /* Interger */
0[0-7]* |
[1-9][0-9]* |
0[Xx][0-9a-fA-F]+ {printf("INT\t%s\t\t%d\n", yytext, yylineno);
                   yylval.i = atol(yytext);
                   return INTEGER;}

 /* Float */
[0-9]*\.[0-9]+{EXP}?[f|d] |
[0-9]{EXP} {printf("FLT\t%s\t\t%d\n", yytext, yylineno);
            yylval.d = atof(yytext);
            return DOUBLE;}

 /* Character */
\'([^"\\]|\\['"?\\abfnrtv]|\\[0-7]{1,3}|\\[Xx][0-9a-fA-F]+])\' {printf("CHAR\t%s\t\t%d\n", yytext, yylineno); return CHARACTER;}

 /* String */
\"([^"\\]|\\['"?\\abfnrtv]|\\[0-7]{1,3}|\\[Xx][0-9a-fA-F]+])*\" {printf("STR\t%s\t\t%d\n", yytext, yylineno);}


 /* Punctuarions */
"{"|"}"|"["|"]"|"("|")"|"."|";"|"," {printf("PUNC\t%s\t\t%d\n", yytext, yylineno); return yytext[0];}
"++"|"--"  {printf("PUNC\t%s\t\t%d\n", yytext, yylineno);}
"+"|"-"|"~"|"!" {printf("PUNC\t%s\t\t%d\n", yytext, yylineno); return yytext[0];}
"*"|"/"|"%" {printf("PUNC\t%s\t\t%d\n", yytext, yylineno); return yytext[0];}
"<<"|">>"|">>>"  {printf("PUNC\t%s\t\t%d\n", yytext, yylineno);}
"=="|"!="|">"|"<"|"<="|">=" {printf("PUNC\t%s\t\t%d\n", yytext, yylineno); strcpy(yylval.operator, yytext); return CMP;}
"^"|"&"|"|"|"&&"|"||" {printf("PUNC\t%s\t\t%d\n", yytext, yylineno);}
"?"|":" {printf("PUNC\t%s\t\t%d\n", yytext, yylineno); return yytext[0];}
"="  {printf("PUNC\t%s\t\t%d\n", yytext, yylineno); return yytext[0];}
"+="|"-="|"*="|"/+"|"%="|"&="|"^="|"|="|"<<="|">>="|">>>="  {printf("PUNC\t%s\t\t%d\n", yytext, yylineno);}


 /* Identifier */
[a-zA-Z_$][a-zA-Z0-9_$]*    {printf("IDT\t%s\t\t%d\n", yytext, yylineno); 
                             yylval.s = lookup(yytext); return IDT;}


 /* Whitespace */
[\n\t ]+

 /*End of file*/
 <<EOF>> {printf("End of file \n"); return END;}

 /* Invalid character */
. {yyerror("Myth\t%c", yytext[0]);}
%%
