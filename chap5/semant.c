#include "semant.h"
#include <stdio.h>  // for NULL
#include <string.h>
#include "absyn.h"
#include "env.h"
#include "errormsg.h"
#include "symbol.h"
#include "types.h"
#include "util.h"

Ty_ty actual_ty(Ty_ty ty);
int has_same_ty(Ty_ty ty1, Ty_ty ty2);

/**
 * Translate Expression
 */

struct expty transExp_callExp(S_table venv, S_table tenv, A_exp a);
struct expty transExp_opExp(S_table venv, S_table tenv, A_exp a);
struct expty transExp_recordExp(S_table venv, S_table tenv, A_exp a);
struct expty transExp_seqExp(S_table venv, S_table tenv, A_exp a);
struct expty transExp_assignExp(S_table venv, S_table tenv, A_exp a);
struct expty transExp_ifExp(S_table venv, S_table tenv, A_exp a);
struct expty transExp_whileExp(S_table venv, S_table tenv, A_exp a);
struct expty transExp_forExp(S_table venv, S_table tenv, A_exp a);
struct expty transExp_letExp(S_table venv, S_table tenv, A_exp a);
struct expty transExp_arrayExp(S_table venv, S_table tenv, A_exp a);

struct expty transExp(S_table venv, S_table tenv, A_exp a) {
  switch (a->kind) {
    case A_varExp:
      return transVar(venv, tenv, a->u.var);
    case A_nilExp:
      return expTy(NULL, Ty_Nil());
    case A_intExp:
      return expTy(NULL, Ty_Int());
    case A_stringExp:
      return expTy(NULL, Ty_String());
    case A_callExp:
      return transExp_callExp(venv, tenv, a);
    case A_opExp:
      return transExp_opExp(venv, tenv, a);
    case A_recordExp:
      return transExp_recordExp(venv, tenv, a);
    case A_seqExp:
      return transExp_seqExp(venv, tenv, a);
    case A_assignExp:
      return transExp_assignExp(venv, tenv, a);
    case A_ifExp:
      return transExp_ifExp(venv, tenv, a);
    case A_whileExp:
      return transExp_whileExp(venv, tenv, a);
    case A_forExp:
      return transExp_forExp(venv, tenv, a);
    case A_breakExp:
      return expTy(NULL, Ty_Void());
    case A_letExp:
      return transExp_letExp(venv, tenv, a);
    case A_arrayExp:
      return transExp_arrayExp(venv, tenv, a);
  }
  assert(0); /* should have returned from some clause of the switch */
}

struct expty transExp_callExp(S_table venv, S_table tenv, A_exp a) {
  E_enventry x = S_look(venv, a->u.call.func);
  A_pos pos = a->pos;

  // check func type
  if (!x || x->kind != E_funEntry) {
    EM_error(pos, "undefined variable %s", S_name(a->u.call.func));
    return expTy(NULL, Ty_Void());
  }

  // check every arglist type
  A_expList args = a->u.call.args;
  Ty_tyList formals = x->u.fun.formals;
  while (args && formals) {
    struct expty arg = transExp(venv, tenv, args->head);
    pos = args->head->pos;
    if (!has_same_ty(formals->head, arg.ty)) {
      EM_error(args->head->pos, "para type mismatched");
      break;
    }
    args = args->tail;
    formals = args->tail;
  }
  if (args || formals) EM_error(pos, "para type mismatched");

  return expTy(NULL, actual_ty(x->u.fun.result));
}

struct expty transExp_opExp(S_table venv, S_table tenv, A_exp a) {
  A_oper oper = a->u.op.oper;
  struct expty left = transExp(venv, tenv, a->u.op.left);
  struct expty right = transExp(venv, tenv, a->u.op.left);

  if (oper == A_plusOp || oper == A_minusOp || oper == A_timesOp ||
      oper == A_divideOp || oper == A_ltOp || oper == A_leOp ||
      oper == A_gtOp || oper == A_geOp) {
    if (left.ty->kind != Ty_int)
      EM_error(a->u.op.left->pos, "integer required");
    if (right.ty->kind != Ty_int)
      EM_error(a->u.op.right->pos, "integer required");
  }

  if (oper == A_eqOp || oper == A_neqOp) {
    if (left.ty->kind != right.ty->kind ||
        left.ty->kind == Ty_nil && right.ty->kind == Ty_record ||
        left.ty->kind == Ty_record && right.ty->kind == Ty_nil)
      EM_error(a->u.op.right->pos, "same type required");
  }

  return expTy(NULL, Ty_Int());
}

struct expty transExp_recordExp(S_table venv, S_table tenv, A_exp a) {
  Ty_ty typ = actual_ty(S_look(tenv, a->u.record.typ));
  A_pos pos = a->pos;

  // check record type
  if (!typ || typ->kind != Ty_record) {
    EM_error(pos, "record type %s mismatched", S_name(a->u.record.typ));
    return expTy(NULL, Ty_Void());
  }

  // check exp field type
  A_efieldList efields = a->u.record.fields;
  Ty_fieldList fields = typ->u.record;
  while (efields && fields) {
    struct expty efield = transExp(venv, tenv, efields->head->exp);
    pos = efields->head->exp->pos;
    if (has_same_ty(fields->head->ty, efield.ty)) {
      EM_error(pos, "record type mismatched");
      break;
    }
    efields = efields->tail;
    fields = fields->tail;
  }
  if (efields || fields) EM_error(pos, "record type mismatched");

  return expTy(NULL, typ);
}

struct expty transExp_seqExp(S_table venv, S_table tenv, A_exp a) {
  A_expList seq = a->u.seq;
  if (!seq || !seq->head) return expTy(NULL, Ty_Void());
  for (; seq->tail; seq = seq->tail) transExp(venv, tenv, seq->head);

  return transExp(venv, tenv, seq->head);
}

struct expty transExp_assignExp(S_table venv, S_table tenv, A_exp a) {
  struct expty lvar = transVar(venv, tenv, a->u.assign.var);
  struct expty rvar = transVar(venv, tenv, a->u.assign.exp);

  if (!has_same_ty(lvar.ty, rvar.ty))
    EM_error(
        a->u.assign.exp->pos,
        "cannot initialize a variable of type '%s' with an rvalue of type '%s'",
        type_msg(venv, lvar.ty), type_msg(rvar.ty));

  return expTy(NULL, Ty_Void());
}

struct expty transExp_ifExp(S_table venv, S_table tenv, A_exp a) {
  struct expty test = transExp(venv, tenv, a->u.iff.test);
  struct expty then = transExp(venv, tenv, a->u.iff.then);

  if (test.ty->kind != Ty_int)
    EM_error(a->u.iff.test->pos, "integer type required");

  if (a->u.iff.elsee) {
    struct expty elsee = transExp(venv, tenv, a->u.iff.elsee);
    if (!has_same_ty(then.ty, elsee.ty))
      EM_error(a->u.iff.elsee->pos, "incompatible types ('%s' and '%s')",
               S_name(then.ty), S_name(elsee.ty));
    return expTy(NULL, then);
  }

  if (then.ty->kind == Ty_void)
    EM_error(a->u.iff.then, "this exp must produce no value");

  return expTy(NULL, Ty_Void());
}

struct expty transExp_whileExp(S_table venv, S_table tenv, A_exp a) {
  struct expty test = transExp(venv, tenv, a->u.whilee.test);
  struct expty body = transExp(venv, tenv, a->u.whilee.body);

  if (test.ty->kind != Ty_int)
    EM_error(a->u.whilee.test->pos, "expected unqualified-id");
  if (body.ty->kind != Ty_void)
    EM_error(a->u.whilee.test->pos, "this exp must produce no value");

  return expTy(NULL, Ty_Void());
}

struct expty transExp_forExp(S_table venv, S_table tenv, A_exp a) {
  struct expty lo = transExp(venv, tenv, a->u.forr.lo);
  struct expty hi = transExp(venv, tenv, a->u.forr.hi);

  if (lo.ty->kind != Ty_int || hi.ty->kind != Ty_int)
    EM_error(a->u.forr.lo->pos, "integer type required");

  S_beginScope(venv);
  transDec(venv, tenv, A_VarDec(a->pos, a->u.forr.var, S_Symbol("int"),
                                a->u.forr.lo));
  struct expty body = transExp(venv, tenv, a->u.forr.body);
  if (body.ty->kind != Ty_void)
    EM_error(a->u.forr.body->pos, "this exp must produce no value");
  S_endScope(venv);

  return expTy(NULL, Ty_void);
}

struct expty transExp_letExp(S_table venv, S_table tenv, A_exp a) {
  struct expty exp;
  S_beginScope(venv);
  S_beginScope(tenv);
  for (A_decList d = a->u.let.decs; d; d = d->tail)
    transDec(venv, tenv, d->head);
  exp = transExp(venv, tenv, a->u.let.body);
  S_endScope(venv);
  S_endScope(tenv);

  return exp;
}

struct expty transExp_arrayExp(S_table venv, S_table tenv, A_exp a) {
  Ty_ty typ = actual_ty(S_look(tenv, a->u.array.typ));
  struct expty size = transExp(venv, tenv, a->u.array.size);
  struct expty init = transExp(venv, tenv, a->u.array.init);

  if (!typ || typ->kind != Ty_array) {
    EM_error(a->pos, "record type %s mismatched", S_name(a->u.array.typ));
    return expTy(NULL, Ty_Void());
  }

  if (size.ty->kind != Ty_int)
    EM_error(a->u.array.size->pos, "integer type required");
  if (init.ty->kind != typ->kind)
    EM_error(
        a->u.array.init->pos,
        "cannot initialize a variable of type '%s' with an rvalue of type '%s'",
        type_msg(typ), type_msg(init.ty));

  return expTy(NULL, typ);
}

/**
 * Translate Variable
 */

struct expty transVar_simpleVar(S_table venv, S_table tenv, A_var v);
struct expty transVar_fieldVar(S_table venv, S_table tenv, A_var v);
struct expty transVar_subscriptVar(S_table venv, S_table tenv, A_var v);

struct expty transVar(S_table venv, S_table tenv, A_var v) {
  switch (v->kind) {
    case A_simpleVar:
      return transVar_simpleVar(venv, tenv, v);
    case A_fieldVar:
      return transVar_fieldVar(venv, tenv, v);
    case A_subscriptVar:
      return transVar_subscriptVar(venv, tenv, v);
  }
}

// todo
struct expty transVar_simpleVar(S_table venv, S_table tenv, A_var v) {
  E_enventry x = S_look(venv, v->u.simple);
  if (x && x->kind == E_varEnventry)
    return expTy(NULL, actual_ty(x->u.var.ty));
  else {
    EM_error(v->pos, "undefined variable %s", S_name(v->u.simple));
    return expTy(NULL, Ty_Int());
  }
}

// todo
struct expty transVar_fieldVar(S_table venv, S_table tenv, A_var v) {}

// todo
struct expty transVar_subscriptVar(S_table venv, S_table tenv, A_var v) {}

/**
 * Translate Declearion
 */

void transDec_varDec(S_table venv, S_table tenv, A_dec d);
void transDec_functionDec(S_table venv, S_table tenv, A_dec d);
void transDec_typeDec(S_table venv, S_table tenv, A_dec d);

void transDec(S_table venv, S_table tenv, A_dec d) {
  switch (d->kind) {
    case A_varDec:
      return transDec_varDec(venv, tenv, d);
    case A_functionDec:
      transDec_functionDec(venv, tenv, d);
    case A_typeDec:
      transDec_typeDec(venv, tenv, d);
  }
}

// todo: Deal with recursive variables
void transDec_varDec(S_table venv, S_table tenv, A_dec d) {
  struct expty e = transExp(venv, tenv, d->u.var.init);
  S_enter(venv, d->u.var.var, E_VarEnventry(e.ty));
}

// todo: Deal with functions recursive or without return values
void transDec_functionDec(S_table venv, S_table tenv, A_dec d) {
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
}

// todo: Extended to any length type declaration list, and the recursive condition
void transDec_typeDec(S_table venv, S_table tenv, A_dec d) {
  S_enter(tenv, d->u.type->head->name, transTy(tenv, d->u.type->head->ty));
}

Ty_ty transTy(S_table tenv, A_ty a);

Ty_ty actual_ty(Ty_ty ty) {
  while (ty && ty->kind == Ty_name) ty = ty->u.name.ty;
  return ty;
}

int has_same_ty(Ty_ty ty1, Ty_ty ty2) {
  ty1 = actual_ty(ty1);
  ty2 = actual_ty(ty2);
  if (ty1->kind == ty2->kind || ty1->kind == Ty_record && ty2->kind == Ty_nil ||
      ty2->kind == Ty_record && ty1->kind == Ty_nil)
    return 1;
  return 0;
}

char* type_msg(Ty_ty ty) {
  char* str = checked_malloc(200 * sizeof(char));
  switch (ty->kind) {
    case Ty_record:
      strcpy(str, "record{ ");
      Ty_fieldList fields = ty->u.record;
      if (!fields) {
        strcat(str, type_msg(fields->head->ty));
        fields = fields->tail;
      }
      for (; fields; fields = fields->tail) {
        strcat(str, ", ");
        strcat(str, type_msg(fields->head->ty));
      }
      strcat(str, " }");
      break;
    case Ty_nil:
      strcpy(str, "nil");
      break;
    case Ty_int:
      strcpy(str, "int");
      break;
    case Ty_string:
      strcpy(str, "string");
      break;
    case Ty_array:
      strcpy(str, "array of ");
      strcat(str, type_msg(ty->u.array));
      break;
    case Ty_name:
      strcpy(str, type_msg(actual_ty(ty)));
      break;
    case Ty_void:
      strcpy(str, "void");
      break;
  }
  return str;
}
