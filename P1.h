#ifndef SYNTAX_TREE
#define SYNTAX_TREE


typedef enum {ROOT, TYPES, TYPEDCL} RULE_TYPE;

typedef struct node {
  RULE_TYPE type;
  char *symbol;
  struct node *sibling;
  struct node *children;
  struct node *lastChildren;
} Node;

typedef struct root {
  RULE_TYPE type;
  Node *pkg;
  Node *imp;
  Node *types;
} Root;

Node *newRoot(Node *pkg, Node *imp, Node *types);

Node *newNode(RULE_TYPE t, char *s, Node *sib, Node *lastSib, Node *c);

void addChild(RULE_TYPE t, Node *p, Node *c);


void yyerror(const char *s, ...);
void dumpTree(Node *a, int level);

#endif
