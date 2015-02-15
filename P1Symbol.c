#include "P1Symbol.h"
#include "P1.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

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

struct symbol *lookup(char *s, struct symbol *symtab) {
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

int lookupPair(Pair *p, char *s1, char*s2) {
  // return 0 means pair not in list
  // return 1 means pair in list
  // return 2 means one of the symbol in list, but the other symbol does not match the pair
  Pair *ptr = p;
  while (ptr) {
    int cmp1, cmp2;
    cmp1 = strcmp(ptr->s1, s1);
    cmp2 = strcmp(ptr->s2, s2);
    if  (cmp1 == 0 && cmp2 == 0) {
      return 1;
    } else if (cmp1 == 0 || cmp2 == 0) {
      return 2;
    }
    ptr = ptr->next;
  }
  return 0;
}

void addPair(Pair **p, char *s1, char *s2) {
  Pair *ptr = (Pair *)malloc(sizeof(Pair));
  ptr->s1 = s1;
  ptr->s2 = s2;

  ptr->next = *p;
  *p = ptr;
}

void printPair(Pair *p) {
  Pair *ptr = p;
  while (ptr) {
    printf("%s %s\n", ptr->s1, ptr->s2);
    ptr = ptr->next;
  }
}
