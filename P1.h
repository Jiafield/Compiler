#ifndef SYNTAXTREE
#define SYNTAXTREE

typedef enum {
#define PROCESS(x) x,
#include "P1enums.h"
#undef PROCESS
  TERMINAL,
  RULESEND,
} RULE_TYPE;

char *TypeToName[RULESEND];
void addEnum(RULE_TYPE x);

typedef struct node {
  RULE_TYPE type;
  struct node *sibling;
  struct node *children;
  struct node *lastChildren;
} Node;

typedef struct leaf {
  RULE_TYPE type;
  struct node *sibling;
  char *symbol;
} Leaf;

typedef struct root {
  RULE_TYPE type;
  Node *pkg;
  Node *imp;
  Node *types;
} Root;

Node *newRoot(Node *pkg, Node *imp, Node *types);

Node *newNode(RULE_TYPE t, int cNum, ...);

Node *newLeaf(char *s);
void addChild(Node *p, Node *c);


void yyerror(const char *s, ...);
void dumpTree(Node *a);
void dump(Node *r, int level);

#endif
