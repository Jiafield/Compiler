#include "P1Symbol.h"
#include "P1.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

int lookupPair(Pair *p, char *s1, char *s2) {
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

void catPairs(Pair **p, Pair *temp) {
  if (!temp)
    return;
  Pair *head = temp;
  while (head) {
    if (!lookupPair(*p, head->s1, head->s2)) {
      addPair(p, head->s1, head->s2);
    }
    head = head->next;
  }
}

void freePair(Pair *p) {
  if (!p)
    return;
  freePair(p->next);
  free(p);
}
