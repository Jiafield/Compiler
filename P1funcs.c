#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "P1.h"
#include "P1Symbol.h"

Node *newRoot(Node *pkg, Node *imp, Node *types) {
  return NULL;
}

Node *newNode(RULE_TYPE t, char *s, Node *sib, Node *lastSib, Node *c) {
  return NULL;
}

void addChild(RULE_TYPE t, Node *p, Node *c) {
  return;
}

extern char* yytext;
extern int yylineno;
void yyerror(const char *s, ...) {
  va_list ap;
  va_start(ap, s);

  fprintf(stderr, s, ap);
  fprintf(stderr, "\tLine:%d  %s\n", yylineno, yytext);
}

void dumpTree(Node *a, int level) {
}

extern int yyparse();
extern int yydebug;
int main() {
  yydebug = 0;
  return yyparse();
}
