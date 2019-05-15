#include <stdio.h>
#include <stdlib.h>
#include "util.h"
#include "symbol.h"
#include "temp.h"
#include "tree.h"
#include "frame.h"
#include "printtree.h"
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

static F_fragList fragList = NULL;

F_fragList Tr_getResult() {
  return fragList;
}

Tr_level Tr_outermost(void) {
  static Tr_level outermost;
  if (!outermost)
    outermost = Tr_newLevel(NULL, Temp_newlabel(), NULL);
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

Tr_expList Tr_ExpList(Tr_exp h, Tr_expList t) {
  Tr_expList p = (Tr_expList)checked_malloc(sizeof(*p));
  p->head = h; p->tail = t;
  return p;
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
    case Tr_cx: {// flag := (a > b)
      Temp_temp r = Temp_newtemp();
      Temp_label t = Temp_newlabel(), f = Temp_newlabel();
      doPatch(e->u.cx.trues, t);
      doPatch(e->u.cx.falses, f);
      return T_Eseq(T_Move(T_Temp(r), T_Const(1)),
              T_Eseq(e->u.cx.stm,
              T_Eseq(T_Label(f),
              T_Eseq(T_Move(T_Temp(r), T_Const(0)),
              T_Eseq(T_Label(t), 
              T_Temp(r))))));
    }
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
    case Tr_ex: { // if(x)
      T_stm stm = T_Cjump(T_ne, unEx(e), T_Const(0), NULL, NULL);
      patchList trues  = PatchList(&stm->u.CJUMP.true,  NULL);
      patchList falses = PatchList(&stm->u.CJUMP.false, NULL);
      struct Cx cx = { trues, falses, stm };
      return cx;
    }
    case Tr_nx: {
      assert(0);
      break;
    }
    case Tr_cx:
      return e->u.cx;
  }
  assert(0);
}


Tr_exp Tr_noExp() {
  return Tr_Ex(T_Const(0));
}

Tr_exp Tr_nilExp() {
  return Tr_Ex(T_Const(0));
}
Tr_exp Tr_intExp(int i) {
  return Tr_Ex(T_Const(i));
}
Tr_exp Tr_stringExp(string s) {
  Temp_label lab = Temp_newlabel();
  F_frag frag = F_StringFrag(lab, s);
  fragList = F_FragList(frag, fragList);
  return Tr_Ex(T_Name(lab));
}

Tr_exp Tr_simpleVar(Tr_access acc, Tr_level lev) {
  /*
   * MEM(+(CONST kn, MEM(+(CONST kn-1, ...
   *                   MEM(+(CONST k1, TEMP FP)) ... ))))
   */
  if (acc->level == lev) return Tr_Ex(F_Exp(acc->access, T_Temp(F_FP())));

  Tr_access static_link_offset = Tr_formals(lev)->head;
  return Tr_Ex(F_Exp(static_link_offset->access, unEx(Tr_simpleVar(acc, lev->parent))));
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
Tr_exp Tr_logicalOpExp(Tr_exp l, Tr_exp r, T_relOp op) {
  T_stm stm = T_Cjump(op, unEx(l), unEx(r), NULL, NULL);
  patchList trues = PatchList(&stm->u.CJUMP.true, NULL);
  patchList falses = PatchList(&stm->u.CJUMP.false, NULL);
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
  return Tr_logicalOpExp(l, r, T_lt);
}
Tr_exp Tr_leOpExp(Tr_exp l, Tr_exp r) {
  return Tr_logicalOpExp(l, r, T_le);
}
Tr_exp Tr_gtOpExp(Tr_exp l, Tr_exp r) {
  return Tr_logicalOpExp(l, r, T_gt);
}
Tr_exp Tr_geOpExp(Tr_exp l, Tr_exp r) {
  return Tr_logicalOpExp(l, r, T_ge);
}
Tr_exp Tr_eqExp(Tr_exp l, Tr_exp r) {
  return Tr_logicalOpExp(l, r, T_eq);
}
Tr_exp Tr_neqExp(Tr_exp l, Tr_exp r) {
  return Tr_logicalOpExp(l, r, T_ne);
}
Tr_exp Tr_stringEqExp(Tr_exp l, Tr_exp r) {
  return Tr_Ex(T_Call(T_Name(Temp_namedlabel("stringEqual")),
               T_ExpList(unEx(l), T_ExpList(unEx(r), NULL))));
}
Tr_exp Tr_stringNeExp(Tr_exp l, Tr_exp r) {
  return Tr_stringEqExp(r, l);
}

Tr_exp Tr_ifExp(Tr_exp e1, Tr_exp e2, Tr_exp e3) {
  Temp_label t = Temp_newlabel(), f = Temp_newlabel(), z = Temp_newlabel();
  Temp_temp r = Temp_newtemp();
  return Tr_Ex(T_Eseq(unNx(e1),
               T_Eseq(T_Label(t),
               T_Eseq(T_Move(T_Temp(r), unEx(e2)),
               T_Eseq(T_Jump(T_Name(z), Temp_LabelList(z, NULL)),
               T_Eseq(T_Label(f),
               T_Eseq(T_Move(T_Temp(r), unEx(e3)),
               T_Eseq(T_Label(z),
                      T_Temp(r)))))))));
}
Tr_exp Tr_ifExp_noValue(Tr_exp e1, Tr_exp e2, Tr_exp e3) {
  Temp_label t = Temp_newlabel(), f = Temp_newlabel();
  if (!e3) {
    return Tr_Nx(T_Seq(unNx(e1),
                 T_Seq(T_Label(t),
                 T_Seq(unNx(e2),
                 T_Label(f)))));
  }
  Temp_label z = Temp_newlabel();
  return Tr_Nx(T_Seq(unNx(e1),
               T_Seq(T_Label(t),
               T_Seq(unNx(e2),
               T_Seq(T_Jump(T_Name(z), Temp_LabelList(z, NULL)),
               T_Seq(T_Label(f),
               T_Seq(unNx(e3),
                     T_Label(z))))))));
}

Tr_exp Tr_recordExp(Tr_expList fields, int size) {
  Temp_temp r = Temp_newtemp();
  T_stm acc =
      T_Seq(T_Move(T_Temp(r),
                   F_externalCall("malloc", //todo:malloc
                                  T_ExpList(T_Const(size * F_wordSize), NULL))),
            NULL);

  T_stm p = acc;
  for (int offset = 0; fields; fields = fields->tail, offset += 1) {
    T_stm s =
        T_Move(T_Mem(T_Binop(T_plus, T_Temp(r), T_Const(offset * F_wordSize))),
               unEx(fields->head));
    if (!(fields->tail)) p->u.SEQ.right = s;
    else p->u.SEQ.right = T_Seq(s, NULL);
    p = p->u.SEQ.right;
  }

  return Tr_Ex(T_Eseq(acc, T_Temp(r)));
}
Tr_exp Tr_arrayExp(Tr_exp size, Tr_exp init) {
  return Tr_Ex(T_Call(T_Name(Temp_namedlabel("initArray")),
                T_ExpList(unEx(size), T_ExpList(unEx(init), NULL))));
}

Tr_exp Tr_whileExp(Tr_exp cond, Tr_exp body, Temp_label done) {
  Temp_label test = Temp_newlabel();
  Temp_label loop = Temp_newlabel();

  return Tr_Nx(T_Seq(T_Label(test),
               T_Seq(T_Cjump(T_eq, unEx(cond), T_Const(0), loop, done),
               T_Seq(T_Label(loop),
               T_Seq(unNx(body),
               T_Seq(T_Jump(T_Name(test), Temp_LabelList(test, NULL)),
                     T_Label(done)))))));
}

Tr_exp Tr_breakExp(Temp_label done) {
  return Tr_Nx(T_Jump(T_Name(done), Temp_LabelList(done, NULL)));
}

Tr_exp Tr_callExp(Temp_label name, Tr_expList args, Tr_level cur, Tr_level lev) {
  T_expList h = T_ExpList(NULL, NULL), p = h;
  for (; args; args = args->tail) {
    p->tail = T_ExpList(unEx(args->head), NULL);
    p = p->tail;
  }

  T_exp fp = T_Temp(F_FP());
	while (cur != lev->parent) {
		fp = F_Exp(F_formals(cur->frame)->head, fp);
		cur = cur->parent;
	}
  h->head = fp;

  return Tr_Ex(T_Call(T_Name(name), h));
}

Tr_exp Tr_assignExp(Tr_exp lvar, Tr_exp rvar) {
  return Tr_Nx(T_Move(unEx(lvar), unEx(rvar)));
}

Tr_exp Tr_LetExp(Tr_expList decs, Tr_exp body) {
  Tr_exp d = Tr_seqStm(decs);
  
  return Tr_Nx(T_Seq(unNx(d), unNx(body)));
}

Tr_exp Tr_seqStm(Tr_expList list) {
  if (!list->tail) return list->head;
  return Tr_Nx(T_Seq(unNx(list->head), unNx(Tr_eseqExp(list->tail))));
}

Tr_exp Tr_eseqExp(Tr_expList list) {
  if (!list->tail) return list->head;
  return Tr_Ex(T_Eseq(unNx(list->head), unEx(Tr_eseqExp(list->tail))));
}

void Tr_procEntryExit(Tr_level level, Tr_exp body, Tr_accessList formals) {
	F_frag frag = F_ProcFrag(unNx(body), level->frame);
	fragList = F_FragList(frag, fragList);
}

T_stm F_procEntryExit1(F_frame frame, T_stm stm);

void Tr_printTree(Tr_exp e) {
    T_stmList sl = T_StmList(unNx(e), NULL);
    printStmList(stdout, sl);
}
