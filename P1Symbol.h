#ifndef Symbol
#define Symbol

#define TABLE_SIZE 7919
struct symbol {
  char *name;
};

struct symbol *lookup(char *, struct symbol *symtab);

#endif
