#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "P1.h"
#include "P1Symbol.h"

void addEnums() {
#define PROCESS(x) TypeToName[x] = #x;
#include "P1enums.h"
#undef PROCESS  
}

Node *newRoot(Node *pkg, Node *imp, Node *types) {
  Root *r = (Root *)malloc(sizeof(Root));
  r->type = ROOT;
  r->pkg = pkg;
  r->imp = imp;
  r->types = types;
  return (Node *)r;
}

Node *newNode(RULE_TYPE t, int cNum, ...) {
  Node *n = (Node *)malloc(sizeof(Node));
  va_list ap;
  int i;

  n->type = t;
  n->sibling = NULL;
  n->children = NULL;
  // Add children
  va_start(ap, cNum);
  for (i = 0; i < cNum; i++) {
    addChild(n, va_arg(ap, Node *));
  }
  va_end(ap);
  return n;
}

Node *newLeaf(char *s) {
  Leaf *n = (Leaf *)malloc(sizeof(Leaf));
  n->type = TERMINAL;
  n->sibling = NULL;
  n->symbol = s;
  return (Node *)n;
}

void addChild(Node *p, Node *c) {
  if (!(p->children)) {
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

void dumpTree(Node *r) {
  addEnums();
  if (r->type != ROOT) {
    printf("Not a root!\n");
    return;
  }
  Root *root = (Root *)r;
  printf("%s\n", TypeToName[root->type]);
  dump(root->pkg, 1);
  dump(root->imp, 1);
  dump(root->types, 1);
}

void dump(Node *r, int level) {
  if (!r)
    return;

  printf("%*s", level * 2, "");
  level++;

  if (r->type != TERMINAL) {
    printf("%s\n", TypeToName[r->type]);
    Node *ptr = r->children;
    while (ptr) {
      dump(ptr, level);
      ptr = ptr->sibling;
    }
  } else {
    Leaf *n = (Leaf *)r;
    printf("%s\n", n->symbol);
  }
}

extern int yyparse();
extern int yydebug;
int main() {
  yydebug = 0;
  Node *r1 = yyparse();
  dumpTree(r1);
  return 0;
}

