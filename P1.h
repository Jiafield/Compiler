struct symbol {
  char *name;
};

#define TABLE_SIZE 7919
struct symbol symtab[TABLE_SIZE];

struct symbol *lookup(char *);

struct ast {
  int type;
  struct ast *l;
  struct ast *r;
};

struct intval {
  int type;  // Type is 'I'
  int val;
};

struct fltval {
  int type;  // Type is 'F'
  double val;
};

struct assign {
  int type;  // Type is '='
  struct ast *lval;
  struct ast *rval;
};

struct symref {
  int type;  // Type is 'S'
  struct symbol *s;
};

struct flow {
  int type;
  struct ast *doList;
  struct ast *elseList;
};

struct ast *newAst(int t, struct ast *l, struct ast *r);
struct ast *newInt(int num);
struct ast *newFlt(double num);
struct ast *newRef(struct symbol *s);
struct ast *newAssign(struct ast *l, struct ast *r);

void yyerror(const char *s, ...);
void dumpAst(struct ast *a, int level);
