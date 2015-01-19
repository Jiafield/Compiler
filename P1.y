%{
#include <stdio.h>
#include <stdlib.h>
#include "P1.h"
%}

%union {
  struct ast *node;
  int iNum;
  double dNum;
}

%token <i> INTEGER
%token <d> DOUBLE
%token EOL

%right '='
%left '+' '-'
%left '*' '/'

