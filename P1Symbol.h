#ifndef Symbol
#define Symbol

#define TABLE_SIZE 7919
struct symbol {
  char *name;
};

typedef struct pair{
  char *s1;
  char *s2;
  struct pair *next;
} Pair;

struct symbol *lookup(char *, struct symbol *symtab);
int lookupPair(Pair *p, char *s1, char*s2);
void addPair(Pair **p, char *s1, char *s2);
void printPair(Pair *p);

#endif
