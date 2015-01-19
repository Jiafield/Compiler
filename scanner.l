%option nodefault noyywrap yylineno

%x COMMENT

EXP ([Ee][-+][0-9]+)

%{

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
"abstract" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"assert" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"boolean" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"break" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"byte" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"case" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"catch" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"char" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"class" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"const" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"continue" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"default" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"do" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"double" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"else" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"enum" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"extends" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"final" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"finally" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"float" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"for" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"goto" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"if" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"implements" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"import" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"instanceof" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"int" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"interface" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"long" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"native" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"new" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"package" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"private" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"protected" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"public" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"return" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"short" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"static" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"strictfp" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"super" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"switch" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"synchronized" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"this" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"throw" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"throws" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"transient" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"try" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"void" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"volatile" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}
"while" {printf("KEY\t%s\t\t%d\n", yytext, yylineno);}

 /* Interger */
0[0-7]* |
[1-9][0-9]* |
0[Xx][0-9a-fA-F]+ {printf("INT\t%s\t\t%d\n", yytext, yylineno);
                   yylval.i = atoi(yytext);
                   return INTEGER;}

 /* Float */
[0-9]*\.[0-9]+{EXP}? |
[0-9]{EXP} {printf("FLT\t%s\t\t%d\n", yytext, yylineno);
            yylval.d = atof(yytext);
            return DOUBLE;}

 /* Character */
\'([^"\\]|\\['"?\\abfnrtv]|\\[0-7]{1,3}|\\[Xx][0-9a-fA-F]+])\' {printf("CHAR\t%s\t\t%d\n", yytext, yylineno);}

 /* String */
\"([^"\\]|\\['"?\\abfnrtv]|\\[0-7]{1,3}|\\[Xx][0-9a-fA-F]+])*\" {printf("STR\t%s\t\t%d\n", yytext, yylineno);}


 /* Punctuarions */
"{"|"}"|"["|"]"|"("|")"|"."|";"|"," {printf("PUNC\t%s\t\t%d\n", yytext, yylineno);}
"++"|"--"|"+"|"-"|"~"|"!" {printf("PUNC\t%s\t\t%d\n", yytext, yylineno);}
"*"|"/"|"%" {printf("PUNC\t%s\t\t%d\n", yytext, yylineno);}
"<<"|">>"|">>>"  {printf("PUNC\t%s\t\t%d\n", yytext, yylineno);}
"=="|"!="|">"|"<"|"<="|">=" {printf("PUNC\t%s\t\t%d\n", yytext, yylineno);}
"^"|"&"|"|"|"&&"|"||" {printf("PUNC\t%s\t\t%d\n", yytext, yylineno);}
"?"|":" {printf("PUNC\t%s\t\t%d\n", yytext, yylineno);}
"="|"+="|"-="|"*="|"/+"|"%="|"&="|"^="|"|="|"<<="|">>="|">>>="  {printf("PUNC\t%s\t\t%d\n", yytext, yylineno);}


 /* Identifier */
[a-zA-Z_$][a-zA-Z0-9_$]*    {printf("IDT\t%s\t\t%d\n", yytext, yylineno);}


 /* Whitespace */
[\t\n ]+


 /* Invalid character */
. {printf("Myth\t%s\t\t%d\n", yytext, yylineno);}
%%


int main(int argc, char **argv) {
  yylex();
  return 0;
}
