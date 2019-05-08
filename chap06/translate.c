#include <stdio.h>
#include <stdlib.h>
#include "util.h"
#include "symbol.h"
#include "temp.h"
#include "frame.h"
#include "tree.h"
#include "translate.h"

struct Tr_level_ { Tr_level parent; F_frame frame; };

struct Tr_access_ {
  Tr_level level;
  F_access access;
};
Tr_access Tr_Access(Tr_level level, F_access access);

Tr_level Tr_outermost(void) {
  static Tr_level outermost = NULL;
  if (!outermost) {
    outermost = checked_malloc(sizeof(*outermost));
    outermost->parent = NULL;
    outermost->frame = F_newFrame(Temp_newlabel(), NULL);
  }
  return outermost;
}

Tr_level Tr_newLevel(Tr_level parent, Temp_label name, U_boolList formals) {
  Tr_level lev = checked_malloc(sizeof(*lev));
  lev->parent = parent;
  lev->frame = F_newFrame(name, U_BoolList(TRUE, formals));
  return lev;
}

Tr_access Tr_Access(Tr_level level, F_access access) {
  Tr_access a = checked_malloc(sizeof(*a));
  a->level = level; a->access = access;
  return a;
}

Tr_accessList Tr_formals(Tr_level level) {
  F_accessList formals = F_formals(level->frame);
  Tr_accessList h = Tr_AccessList(NULL,NULL), p = h;
  for (; formals; formals = formals->tail) {
    p->tail = Tr_AccessList(Tr_Access(level, formals->head), NULL);
    p = p->tail;
  }
  p = h->tail;
  free(h);
  return p;
}


Tr_access Tr_allocLocal(Tr_level level, bool escape) {
  Tr_access a = checked_malloc(sizeof(*a));
  return Tr_Access(level, F_allocLocal(level->frame, escape));
}

Tr_accessList Tr_AccessList(Tr_access h, Tr_accessList t) {
  Tr_accessList p = (Tr_accessList)checked_malloc(sizeof(*p));
  p->head = h; p->tail = t;
  return p;
}
