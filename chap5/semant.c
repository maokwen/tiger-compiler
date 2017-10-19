#include "semant.h"
#include <stdio.h>  // for NULL
#include "absyn.h"
#include "env.h"
#include "errormsg.h"
#include "symbol.h"
#include "types.h"
#include "util.h"

struct expty transExp(S_table venv, S_table tenv, A_exp a) {
  switch (a->kind) {
    // ...
    case A_opExp: {
      A_oper oper = a->u.op.oper;
      struct expty left = transExp(venv, tenv, a->u.op.left);
      struct expty right = transExp(venv, tenv, a->u.op.left);
      if (oper == A_plusOp) {
        if (left.ty->kind != Ty_int)
          EM_error(a->u.op.left->pos, "integer required");
        if (right.ty->kind != Ty_int)
          EM_error(a->u.op.right->pos, "integer required");
        return expTy(NULL, Ty_Int());
      }
      //...
    }
    //...
    case A_letExp: {
      struct expty exp;
      A_decList d;
      S_beginScope(venv);
      S_beginScope(tenv);
      for (d = a->u.let.decs; d; d = d->tail)
        transDec(venv, tenv, a->u.let.body);
      S_endScope(venv);
      S_endScope(tenv);
      return exp;
    }
  }
  assert(0); /* should have returned from some clause of the switch */
}

struct expty transVar(S_table venv, S_table tenv, A_var v) {
  switch (v->kind) {
    case A_simpleVar: {
      E_enventry x = S_look(venv, v->u.simple);
      if (x && x->kind == E_varEnventry)
        return expTy(NULL, actual_ty(x->u.var.ty));
      else {
        EM_error(v->pos, "undefined variable %s", S_name(v->u.simple));
        return expTy(NULL, Ty_Int());
      }
    }
    case A_fieldVar: {
    }
    case A_subscriptVar: {
    }
  }
}

void transDec(S_table venv, S_table tenv, A_dec d) {
  switch (d->kind) {
    case A_varDec: { //to extend
      struct expty e = transExp(venv, tenv, d->u.var.init);
      S_enter(venv, d->u.var.var, E_VarEnventry(e.ty));
      break;
    }
    case A_functionDec: { // to extend
      A_fundec f = d->u.function->head;
      Ty_ty resultTy = S_look(tenv, f->result);
      Ty_tyList formalTys = makeFormalTyList(tenv, f->params);
      S_enter(venv, f->name, E_FunEntry(formalTys, resultTy));
      S_beginScope(venv);
      {
        A_fieldList l;
        Ty_tyList t;
        for (l = f->params, t = formalTys; l; l = l->tail, t = t->tail)
          S_enter(venv, l->head->name, E_VarEnventry(t->head));
      }
      transExp(venv, tenv, d->u.function->head->body);
      S_endScope(venv);
      break;
    }
    case A_typeDec: { //to extend
      S_enter(tenv, d->u.type->head->name, transTy(d->u.type->head->ty));
      break;
    }
  }
}
Ty_ty transTy(S_table tenv, A_ty a);
