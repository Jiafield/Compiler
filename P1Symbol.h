#ifndef Symbol
#define Symbol

#define TABLE_SIZE 7919

// Use a linked list when hash collisions
struct symbol {
  char *name;
  struct symbol *next;
};

struct symbol table[TABLE_SIZE];

/*
// Enter a scope, create a new table.
struct symTable *enterScope(struct symTable *t);

// Exit a scope, delete the current scope table.
struct symTable *exitScope(struct symTable *t);
*/

// Find s in current symbol table, return 1 for true, 0 for false
struct symbol *findSymbol(char *s);

// Add symbol to current scope symbol table.
struct symbol *addSymbol(char *s);

#endif
