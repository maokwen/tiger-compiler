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
    case Tr_ex: // if(x)
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

Tr_exp Tr_nilExp() {
  Tr_Ex(T_Const(0));
}
Tr_exp Tr_intExp(int i) {
  Tr_Ex(T_Const(i));
}
Tr_exp Tr_stringExp(string s) {
  // todo
}

Tr_exp Tr_simpleVar(Tr_access acc, Tr_level lev) {
  /*
   * MEM(+(CONST kn, MEM(+(CONST kn-1, ...
   *                   MEM(+(CONST k1, TEMP FP)) ... ))))
   */
  if (acc->level == lev) return Tr_Ex(F_Exp(acc->access, T_Temp(F_FP())));

  Tr_access static_link_offset = Tr_formals(lev)->head;
  return Tr_Ex(F_Exp(static_link_offset, unEx(Tr_simpleVar(acc, lev->parent))));
}
Tr_exp Tr_fieldVar(Tr_exp var, int index, Tr_level lev) {
  /*
   * MEM(+(var, *(index, CONST wordsize)))
   */
  // todo: check access to nil
  return Tr_Ex(T_Mem(
                T_Binop(T_plus, unEx(var),
                                T_Binop(T_mul, T_Const(index),
                                               T_Const(F_wordSize)))));
}
Tr_exp Tr_subscriptVar(Tr_exp var, Tr_exp sub, Tr_level lev) {
  /*
   * MEM(+(var, *(sub, CONST wordsize)))
   */
  // todo: check index out of bounds
  return Tr_Ex(T_Mem(
                T_Binop(T_plus, unEx(var),
                                T_Binop(T_mul, unEx(sub), 
                                               T_Const(F_wordSize)))));
}

Tr_exp Tr_arithmeticOpExp(Tr_exp l, Tr_exp r, T_binOp op) {
  return Tr_Ex(T_Binop(op,
                       unEx(l),
                       unEx(r)));
}
Tr_exp Tr_logicalOpExp(Tr_exp l, Tr_exp r, T_binOp op) {
  T_stm stm = T_Cjump(op, UnEx(l), UnEx(r), NULL, NULL);
  patchList trues = PatchList(&stm->u.SEQ.left->u.CJUMP.true, NULL);
  patchList falses = PatchList(&stm->u.SEQ.right->u.CJUMP.true, NULL);
  return Tr_Cx(trues, falses, stm);
}
Tr_exp Tr_addOpExp(Tr_exp l, Tr_exp r) {
  return Tr_arithmeticOpExp(l, r, T_plus);
}
Tr_exp Tr_minusOpExp(Tr_exp l, Tr_exp r) {
  return Tr_arithmeticOpExp(l, r, T_minus);
}
Tr_exp Tr_timesOpExp(Tr_exp l, Tr_exp r) {
  return Tr_arithmeticOpExp(l, r, T_mul);
}
Tr_exp Tr_divideOpExp(Tr_exp l, Tr_exp r) {
  return Tr_arithmeticOpExp(l, r, T_div);
}
Tr_exp Tr_ltOpExp(Tr_exp l, Tr_exp r) {
  Tr_logicalOpExp(l, r, T_lt);
}
Tr_exp Tr_leOpExp(Tr_exp l, Tr_exp r) {
  Tr_logicalOpExp(l, r, T_le);
}
Tr_exp Tr_gtOpExp(Tr_exp l, Tr_exp r) {
  Tr_logicalOpExp(l, r, T_gt);
}
Tr_exp Tr_geOpExp(Tr_exp l, Tr_exp r) {
  Tr_logicalOpExp(l, r, T_ge);
}
Tr_exp Tr_eqExp(Tr_exp l, Tr_exp r) {
  Tr_logicalOpExp(l, r, T_eq);
}
Tr_exp Tr_neqExp(Tr_exp l, Tr_exp r) {
  Tr_logicalOpExp(l, r, T_ne);
}

Tr_exp Tr_ifExp(Tr_exp e1, Tr_exp e2, Tr_exp e3) {
  Temp_label t = Temp_newlabel(), f = Temp_newlabel(), z = Temp_newlabel();
  Temp_label r = Temp_newlabel();
  doPatch(e1->u.cx.trues, t);
  doPatch(e1->u.cx.trues, f);
  return Tr_Ex(T_Eseq(UnEx(e1),
               T_Eseq(T_Label(t),
               T_Eseq(T_Move(r, unEx(e2)),
               T_Eseq(T_Jump(z, Temp_LabelList(z, NULL)),
               T_Eseq(T_Label(f),
               T_Eseq(T_Move(r, unEx(e3)),
               T_Eseq(T_Label(z),
                      T_Temp(r)))))))));
}
Tr_exp Tr_ifExp_noValue(Tr_exp e1, Tr_exp e2, Tr_exp e3) {
  Temp_label t = Temp_newlabel(), f = Temp_newlabel();
  doPatch(e1->u.cx.trues, t);
  doPatch(e1->u.cx.trues, f);
  if (!e3) {
    return Tr_Ex(T_Seq(UnEx(e1),
                 T_Seq(T_Label(t),
                 T_Seq(unNx(e2),
                 T_Label(f)))));
  }
  Temp_label z = Temp_newlabel();
  return Tr_Ex(T_Seq(UnEx(e1),
               T_Seq(T_Label(t),
               T_Seq(unNx(e2),
               T_Seq(T_Jump(z, Temp_LabelList(z, NULL)),
               T_Seq(T_Label(f),
               T_Seq(unUx(e3),
                     T_Label(z))))))));
}
