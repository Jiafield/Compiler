#include "P1Symbol.h"
#include <string.h>
#include <stdlib.h>

/* A Simple Hash Algorithm : Jenkins hash
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

/*
// Enter a scope, create a new table.
struct symTable *enterScope(struct symTable *t);

// Exit a scope, delete the current scope table.
struct symTable *exitScope(struct symTable *t);
*/

// Find s in current symbol table, return 1 for true, 0 for false
struct symbol *findSymbol(char *s) {
  struct symbol *ptr = &table[hashStr(s) % TABLE_SIZE];
  while (ptr) {
    if (ptr->name && strcmp(ptr->name, s) != 0)
      return ptr;   // Found
    ptr = ptr->next;
  }
  return NULL;  // Not found
}

// Add symbol to current scope symbol table.
struct symbol *addSymbol(char *s) {
  struct symbol *ptr = &table[hashStr(s) % TABLE_SIZE];
  if (!(ptr->name)) {
    ptr->name = strdup(s);
    return ptr;
  } else {
    // If collision happens, search next entry
    while (ptr->next) ptr = ptr->next;
    ptr->next = (struct symbol *)malloc(sizeof(struct symbol));
    ptr->next->name = strdup(s);
    ptr->next->next = NULL;
    return ptr->next;
  }
}
