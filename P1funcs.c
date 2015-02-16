#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "P1.h"
#include "P1Symbol.h"

extern void yyrestart(FILE *fp);
extern int yyparse();
extern int yydebug;
extern int yylineno;
Node *globalRoot;

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
  n->cNum = 0;
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
  n->line = yylineno;
  return (Node *)n;
}

void addChild(Node *p, Node *c) {
  if (!(p->children)) {
    p->children = c;
  } else {
    p->lastChildren->sibling = c;
  }
  p->lastChildren = c;
  (p->cNum)++;
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

  printf("%*s", level, "");
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

int cmpTree(Node *n1, Node *n2, Pair **table) {
  // return 0 means false, 1 means true
  if (!n1 || !n2)
    return 0;
  if (n1->type != ROOT || n2->type != ROOT)
    return 0;
  Root *r1 = (Root *)n1;
  Root *r2 = (Root *)n2;
  if (!naiveCmp(r1->pkg, r2->pkg, table))
    return 0;
  if (!naiveCmp(r1->imp, r2->imp, table))
    return 0;
  if (!naiveCmp(r1->types, r2->types, table))
    return 0;
  return 1;
}

int naiveCmp(Node *r1, Node *r2, Pair **table) {
  // Compare if 2 subtrees are exactly the same
  // return 0 means false, 1 means true
  if (r1->type != r2->type)
    return 0;

  if (r1->type == TERMINAL) {
    // If nodes are terminals
    Leaf *l1 = (Leaf *)r1;
    Leaf *l2 = (Leaf *)r2;
    if (strcmp(l1->symbol, l2->symbol) == 0)
      return 1;
    int cmpS = lookupPair(*table, l1->symbol, l2->symbol);
    if (cmpS == 1) {
      return 1;
    } else if (cmpS == 2) {
      return 0;
    } else {
      addPair(table, l1->symbol, l2->symbol);
      return 1;
    }
  } else {
    // Non terminal nodes
    if (r1->cNum != r2->cNum)
      return 0;
    int num = r1->cNum, i, j;
    // Use status table to store the matching status.
    int *status = (int *)calloc(num, sizeof(int));
    if (r1->type == TYPES || r1->type == CLASSBODYDCLS) {
      // Handle reordering of classes and methods
      Node *cur1 = r1->children;
      for (i = 0; i < num; i++) {
	Node *cur2 = r2->children;
	int found = 0;
	for (j = 0; j < num; j++) {
	  // Use pair table to store the temporary symbol matching result
	  Pair *tempTable = NULL;
	  if (status[j]) {
	    cur2 = cur2->sibling;
	    continue;
	  }
	  if (naiveCmp(cur1, cur2, &tempTable)) {
	    catPairs(table, tempTable);
	    found = 1;
	    status[j] = 1;
	    if (r1->type == TYPES)
	      printf("Class %d match %d\n", i, j);
	    else
	      printf("Method or subroutine %d match %d\n", i, j);
	    break;
	  }
	  cur2 = cur2->sibling;
	  freePair(tempTable);
	}	
	if (!found) {
	  return 0;
	}
	cur1 = cur1->sibling;
      }
    } else {
      Node *p1 = r1->children, *p2 = r2->children;
      for (i = 0; i < num; i++) {
	if (!naiveCmp(p1, p2, table))
	  return 0;
	p1 = p1->sibling;
	p2 = p2->sibling;
      }
    }
  }
  return 1;
}

void freeTree(Node *r) {
  if (!r)
    return;
  Root *root = (Root *)r;
  freeSubtree(root->pkg);
  freeSubtree(root->imp);
  freeSubtree(root->types);
}

void freeSubtree(Node *r) {
  if (!r)
    return;
  if (r->type == TERMINAL) {
    Leaf *l = (Leaf *)r;
    free(l);
  } else {
    int i, num = r->cNum;
    Node *ptr = r->children;
    for (i = 0; i < num; i++) {
      freeSubtree(ptr);
      ptr = ptr->sibling;
    }
    free(r);
  }    
}

int main() {
  yydebug = 0;
  char file1[200];
  char file2[200];
  scanf("%s %s", file1, file2);
 
  FILE *fp1 = fopen(file1, "r");
  if (!fp1) {
    printf("Open file error: %s\n", file1);
    exit(1);
  }
  yyrestart(fp1);
  yyparse();
  Node *r1 = globalRoot;
  dumpTree(r1);
  printf("\n");

  FILE *fp2 = fopen(file2, "r");
  if (!fp2) {
    printf("Open file error: %s\n", file2);
    exit(1);
  }
  yylineno = 1;
  yyrestart(fp2);
  yyparse();
  Node *r2 = globalRoot;
  dumpTree(r2);
  printf("\n");

  Pair *symbolPair = NULL;
  if (cmpTree(r1, r2, &symbolPair)) {
    printf("\n****** Matching Symbols ******\n");
    printPair(symbolPair);
    printf("******************************\n");
    printf("Two files are equivalent.\n\n");
  } else {
    printf("Two files are not equivalent.\n\n");
  }

  freeTree(r1);
  freeTree(r2);
  freePair(symbolPair);
  fclose(fp1);
  fclose(fp2);
  return 0;
}
