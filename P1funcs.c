#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "P1.h"
#include "P1Symbol.h"

Node *newRoot(Node *pkg, Node *imp, Node *types) {
  Root *r = (Root *)malloc(sizeof(Root));
  r->type = ROOT;
  r->pkg = pkg;
  r->imp = imp;
  r->types = types;
  return (Node *)r;
}

Node *newNode(RULE_TYPE t, char *s, int cNum, ...) {
  Node *n = (Node *)malloc(sizeof(Node));
  va_list ap;
  int i;

  n->type = t;
  n->symbol = s;
  n->children = NULL;
  // Add children
  va_start(ap, cNum);
  for (i = 0; i < cNum; i++) {
    addChild(n, va_arg(ap, Node *));
  }
  va_end(ap);
  return n;
}

void addChild(Node *p, Node *c) {
  if (!p->children) {
    p->children = c;
  } else {
    p->lastChildren->sibling = c;
  }
  p->lastChildren = c;
}

extern char* yytext;
extern int yylineno;
void yyerror(const char *s, ...) {
  va_list ap;
  va_start(ap, s);

  printf(s, ap);
  printf("\tLine:%d  %s\n", yylineno, yytext);
}

void dumpTree(Node *r, int level) {

}


extern int yyparse();
extern int yydebug;
int main() {
  yydebug = 0;
  return yyparse();
}
