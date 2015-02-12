#include "P1Symbol.h"
#include "P1.h"
#include <string.h>
#include <stdlib.h>


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
