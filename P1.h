#ifndef SYNTAXTREE
#define SYNTAXTREE

#include "P1Symbol.h"

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
  int cNum;
} Node;

typedef struct leaf {
  RULE_TYPE type;
  struct node *sibling;
  char *symbol;
  int line;
} Leaf;

typedef struct root {
  RULE_TYPE type;
  Node *pkg;
  Node *imp;
  Node *types;
} Root;

extern Node *globalRoot;

Node *newRoot(Node *pkg, Node *imp, Node *types);

Node *newNode(RULE_TYPE t, int cNum, ...);

Node *newLeaf(char *s);
void addChild(Node *p, Node *c);

int cmpTree(Node *n1, Node *n2, Pair **table);
int naiveCmp(Node *r1, Node *r2, Pair **table);

void yyerror(const char *s, ...);
void dumpTree(Node *a);
void dump(Node *r, int level);

void freeTree(Node *r);
#endif
