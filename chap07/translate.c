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

typedef struct patchList_ *patchList;
struct patchList_ {Temp_label *head; patchList tail; };
static patchList PatchList(Temp_label *head, patchList tail);

struct Cx { patchList trues; patchList falses; T_stm stm; };

struct Tr_exp_ {
  enum { Tr_ex, Tr_nx, Tr_cx } kind;
  union { T_exp ex; T_stm nx; struct Cx cx; } u;
};

static Tr_exp Tr_Ex(T_exp ex);
static Tr_exp Tr_Nx(T_stm nx);
static Tr_exp Tr_Cx(patchList trues, patchList falses, T_stm stm);
static    T_exp   unEx(Tr_exp e);
static    T_stm   unNx(Tr_exp e);
static struct Cx  unCx(Tr_exp e);

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
  lev->frame = F_newFrame(name, formals);
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

static patchList PatchList(Temp_label *h, patchList t) {
  patchList p = (patchList)checked_malloc(sizeof(*p));
  p->head = h; p->tail = t;
  return p;
}

void doPatch(patchList tList, Temp_label label) {
  for (; tList; tList = tList->tail)
    *(tList->head) = label;
}

patchList joinPatch(patchList first, patchList second) {
  if (!first) return second;
  for (; first->tail; first=first->tail);
  first->tail = second;
  return first;
}

static Tr_exp Tr_Ex(T_exp ex) {
  Tr_exp p = (Tr_exp)checked_malloc(sizeof(*p));
  p->kind = Tr_ex; p->u.ex = ex;
  return p;
};
static Tr_exp Tr_Nx(T_stm nx) {
  Tr_exp p = (Tr_exp)checked_malloc(sizeof(*p));
  p->kind = Tr_nx; p->u.nx = nx;
  return p;
}
static Tr_exp Tr_Cx(patchList trues, patchList falses, T_stm stm) {
  Tr_exp p = (Tr_exp)checked_malloc(sizeof(*p));
  p->kind = Tr_cx;
  p->u.cx.trues = trues;
  p->u.cx.falses = falses;
  p->u.cx.stm = stm;
  return p;
}
static T_exp unEx(Tr_exp e) {
  switch (e->kind) {
    case Tr_ex:
      return e->u.ex;
    case Tr_nx:
      return T_Eseq(e->u.nx, T_Const(0));
    case Tr_cx: // flag := (a > b)
      Temp_temp r = Temp_newtemp();
      Temp_label t = Temp_newlabel(), f = Temp_newlabel();
      doPatch(e->u.cx.trues, t);
      doPatch(e->u.cx.falses, f);
      return  T_Eseq(T_Move(T_Temp(r), T_Const(1)),
              T_Eseq(e->u.cx.stm,
              T_Eseq(T_Label(f),
              T_Eseq(T_Move(T_Temp(r), T_Const(0)),
              T_Eseq(T_Label(t), 
              T_Temp(r))))));
  }
  assert(0);
}
static T_stm unNx(Tr_exp e) {
  switch (e->kind) {
    case Tr_ex:
      return T_Exp(e->u.ex);
    case Tr_nx:
      return e->u.nx;
    case Tr_cx:
      return T_Exp(unEx(e));
      Temp_temp r = Temp_newtemp();
      Temp_label t = Temp_newlabel(), f = Temp_newlabel();
      doPatch(e->u.cx.trues, t);
      doPatch(e->u.cx.falses, f);
      return  T_Seq(T_Move(T_Temp(r), T_Const(1)),
              T_Seq(e->u.cx.stm,
              T_Seq(T_Label(f),
              T_Seq(T_Move(T_Temp(r), T_Const(0)),
              T_Seq(T_Label(t),
              T_Exp(T_Temp(r)))))));
  }
    assert(0);
}
static struct Cx unCx(Tr_exp e) {
  switch (e->kind) {
    case Tr_ex: // if-then-else exp
      T_stm stm = T_Cjump(T_ne, unEx(e), T_Const(0), NULL, NULL);
      patchList trues  = PatchList(&stm->u.CJUMP.true,  NULL);
      patchList falses = PatchList(&stm->u.CJUMP.false, NULL);
      struct Cx cx = { trues, falses, stm };
      return cx;
    case Tr_nx:
      assert(0);
      break;
    case Tr_cx:
      return e->u.cx;
  }
  assert(0);
}
