#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "P1.h"

/* Algorithm : Jenkins hash
   Reference: http://en.wikipedia.org/wiki/Jenkins_hash_function */
static unsigned int hashStr(char *s) {
  unsigned int hash = 0;
  char c;
  while ((c = *s++)) {
    hash += c;
    hash += (hash << 10);
    hash ^= (hash >> 6);
  }
  hash += (hash << 3);
  hash ^= (hash >> 11);
  hash += (hash << 15);
  return hash;
}

struct symbol *lookup(char *s) {
  struct symbol *ptr = &symtab[hashStr(s) % TABLE_SIZE];
  int counter = TABLE_SIZE;

  while (--counter >= 0) {
    // Already exist
    if (ptr->name && strcmp(ptr->name, s) != 0)
      return ptr;
    
    // Not exist, create new entry
    if (!ptr->name) {
      ptr->name = strdup(s);
      return ptr;
    }
    
    // If collision happens, search next entry
    if (++ptr >= symtab + TABLE_SIZE)
      ptr = symtab;
  }
  yyerror("Symbol table out of space");;
  abort();
}

struct ast *newAst(int t, struct ast *l, struct ast *r) {
  struct ast *a = malloc(sizeof(struct ast));
  if (!a) {
    yyerror("Memory allocation error");
    exit(1);
  }
  a->type = t;
  a->l = l;
  a->r = r;
  return a;
}

struct ast *newInt(int num) {
  struct intval *a = malloc(sizeof(struct ast));
  if (!a) {
    yyerror("Memory allocation error");
    exit(1);
  }
  a->type = 'I';
  a->val = num;
  return (struct ast *)a;
}

struct ast *newFlt(double num) {
  struct fltval *a = malloc(sizeof(struct ast));
  if (!a) {
    yyerror("Memory allocation error");
    exit(1);
  }
  a->type = 'F';
  a->val = num;
  return (struct ast *)a;
}

struct ast *newRef(struct symbol *s) {
  struct symref *a = malloc(sizeof(struct ast));
  if (!a) {
    yyerror("Memory allocation error");
    exit(1);
  }
  a->type = 'S';
  a->s = s;
  return (struct ast *)a;
}

struct ast *newAssign(struct ast *l, struct ast *r) {
  struct assign *a = malloc(sizeof(struct assign));
  if (!a) {
    yyerror("Memory allocation error");
    exit(1);
  }
  a->type = '=';
  a->lval = l;
  a->rval = r;
  return (struct ast *)a;
}

extern char* yytext;
extern int yylineno;
void yyerror(const char *s, ...) {
  va_list ap;
  va_start(ap, s);

  fprintf(stderr, s, ap);
  fprintf(stderr, "\tLine:%d  %s\n", yylineno, yytext);
}

void dumpAst(struct ast *a, int level) {
  printf("%*s", 2 * level, "");
  level++;
  
  if (!a) {
    printf("NULL\n");
    return;
  }

  switch(a->type) {
  case 'I':
    printf("Int %d\n", ((struct intval *)a)->val);
    break;
  case 'F':
    printf("Flt %f\n", ((struct fltval *)a)->val);
    break;
  case 'S':
    printf("Ref %s\n", ((struct symref *)a)->s->name);
    break;
  case '=':
    printf("=\n");
    dumpAst(((struct assign *)a)->lval, level);
    dumpAst(((struct assign *)a)->rval, level);
    break;
  case '+':
  case '-':
  case '*':
  case '/':
    printf("%c\n", a->type);
    dumpAst(a->l, level);
    dumpAst(a->r, level);
    break;
  case 'L':
    printf("Stmts\n");
    dumpAst(a->l, level);
    dumpAst(a->r, level);
    break;
  default:
    printf("Bad node\n");
    return;
  }
}

extern int yyparse();
int main() {
  return yyparse();
}
