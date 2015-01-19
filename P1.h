struct ast {
  int type;
  struct ast *l;
  struct ast *r;
};

struct flow {
  int type;
  struct ast *doList;
  struct ast *elseList;
};

