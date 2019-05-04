#include <stdio.h>  // for NULL
#include <string.h>
#include "util.h"
#include "errormsg.h"
#include "symbol.h"
#include "absyn.h"
#include "types.h"
#include "env.h"
#include "semant.h"

void SEM_transProg(A_exp exp) {
  S_table tenv = E_base_tenv();
  S_table venv = E_base_venv();
  transExp(venv, tenv, exp);
}

Ty_ty actual_ty(Ty_ty ty);
int has_same_ty(Ty_ty ty1, Ty_ty ty2);
Ty_tyList makeFormalTyList(S_table tenv, A_fieldList afields);
string type_msg(Ty_ty ty);

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
    EM_error(pos, "undeclared variable %s", S_name(a->u.call.func));
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
    formals = formals->tail;
  }
  if (args || formals) EM_error(pos, "para type mismatched");

  return expTy(NULL, actual_ty(x->u.fun.result));
}

struct expty transExp_opExp(S_table venv, S_table tenv, A_exp a) {
  A_oper oper = a->u.op.oper;
  struct expty left = transExp(venv, tenv, a->u.op.left);
  struct expty right = transExp(venv, tenv, a->u.op.right);

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
        (left.ty->kind == Ty_nil && right.ty->kind == Ty_record) ||
        (left.ty->kind == Ty_record && right.ty->kind == Ty_nil))
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

  return expTy(NULL, S_look(tenv, a->u.record.typ));
}

struct expty transExp_seqExp(S_table venv, S_table tenv, A_exp a) {
  A_expList seq;
  for (seq = a->u.seq; seq && seq->tail; seq = seq->tail)
    transExp(venv, tenv, seq->head);

  if (!seq || !seq->head) return expTy(NULL, Ty_Void());
  return transExp(venv, tenv, seq->head);
}

struct expty transExp_assignExp(S_table venv, S_table tenv, A_exp a) {
  struct expty lvar = transVar(venv, tenv, a->u.assign.var);
  struct expty rvar = transExp(venv, tenv, a->u.assign.exp);

  if (!has_same_ty(lvar.ty, rvar.ty))
    EM_error(
        a->u.assign.exp->pos,
        "cannot initialize a variable of type '%s' with an rvalue of type '%s'",
        type_msg(lvar.ty), type_msg(rvar.ty));

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
               type_msg(then.ty), type_msg(elsee.ty));
    return expTy(NULL, then.ty);
  }

  if (then.ty->kind != Ty_void)
    EM_error(a->u.iff.then->pos, "this exp must produce no value");

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
  transDec(venv, tenv,
           A_VarDec(a->pos, a->u.forr.var, S_Symbol("int"), a->u.forr.lo));
  struct expty body = transExp(venv, tenv, a->u.forr.body);
  if (body.ty->kind != Ty_void)
    EM_error(a->u.forr.body->pos, "this exp must produce no value");
  S_endScope(venv);

  return expTy(NULL, Ty_Void());
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
    EM_error(a->pos, "array type %s mismatched", S_name(a->u.array.typ));
    return expTy(NULL, Ty_Void());
  }

  if (size.ty->kind != Ty_int)
    EM_error(a->u.array.size->pos, "integer type required");

  if (!has_same_ty(init.ty, typ->u.array))
    EM_error(
        a->u.array.init->pos,
        "cannot initialize a variable of type '%s' with an rvalue of type '%s'",
        type_msg(typ), type_msg(init.ty));

  return expTy(NULL, S_look(tenv, a->u.array.typ));
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

struct expty transVar_simpleVar(S_table venv, S_table tenv, A_var v) {
  E_enventry x = S_look(venv, v->u.simple);

  if (!x || x->kind != E_varEntry) {
    EM_error(v->pos, "undeclared variable %s", S_name(v->u.simple));
    return expTy(NULL, Ty_Int());
  }

  return expTy(NULL, actual_ty(x->u.var.ty));
}

struct expty transVar_fieldVar(S_table venv, S_table tenv, A_var v) {
  struct expty var = transVar(venv, tenv, v->u.field.var);

  if (var.ty->kind != Ty_record)
    EM_error(v->pos, "invalid types '%s' for record field", type_msg(var.ty));

  Ty_fieldList fields;
  for (fields = var.ty->u.record; fields != NULL; fields = fields->tail)
    if (fields->head->name == v->u.field.sym) break;
  if (!fields) {
    EM_error(v->pos, "undefined record field %s", S_name(v->u.field.sym));
    return expTy(NULL, Ty_Int());
  }
  return expTy(NULL, actual_ty(fields->head->ty));
}

struct expty transVar_subscriptVar(S_table venv, S_table tenv, A_var v) {
  struct expty var = transVar(venv, tenv, v->u.subscript.var);
  struct expty exp = transExp(venv, tenv, v->u.subscript.exp);

  if (var.ty->kind != Ty_array) {
    EM_error(v->u.subscript.var->pos, "variable type '%s' is not array",
             type_msg(var.ty));
    return expTy(NULL, Ty_Int());
  }
  if (exp.ty->kind != Ty_int) {
    EM_error(v->u.subscript.exp->pos, "invalid types '%s' for array subscript",
             type_msg(exp.ty));
    return expTy(NULL, Ty_Int());
  }

  return expTy(NULL, actual_ty(var.ty->u.array));
}

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

void transDec_varDec(S_table venv, S_table tenv, A_dec d) {
  struct expty init = transExp(venv, tenv, d->u.var.init);

  if (d->u.var.typ) {
    Ty_ty typ = S_look(tenv, d->u.var.typ);

    if (!has_same_ty(typ, init.ty))
      EM_error(d->u.var.init->pos,
               "cannot initialize a variable of type '%s' with an rvalue of "
               "type '%s'",
               type_msg(typ), type_msg(init.ty));
  } else if (init.ty->kind == Ty_nil)
    EM_error(d->u.var.init->pos,
             "cannot initialize a nil type without specified record type");

  S_enter(venv, d->u.var.var, E_VarEntry(init.ty));
}

void transDec_functionDec(S_table venv, S_table tenv, A_dec d) {
  for (A_fundecList fundecs = d->u.function; fundecs; fundecs = fundecs->tail) {
    S_symbol name = fundecs->head->name;
    Ty_tyList formals = makeFormalTyList(tenv, fundecs->head->params);

    for (A_fundecList f = d->u.function; f != fundecs; f = f->tail)
      if (f->head->name == name)
        EM_error(f->head->pos,
                 "there are two functions with the same name in the same batch "
                 "of mutually recursive functions");

    if (!fundecs->head->result) {
      S_enter(venv, name, E_FunEntry(formals, Ty_Void()));
    } else {
      Ty_ty result = S_look(tenv, fundecs->head->result);
      S_enter(venv, name, E_FunEntry(formals, result));
    }
  }

  for (A_fundecList fundecs = d->u.function; fundecs; fundecs = fundecs->tail) {
    S_symbol name = fundecs->head->name;
    E_enventry x = S_look(venv, name);
    Ty_tyList formals = x->u.fun.formals;
    Ty_ty result = x->u.fun.result;

    S_beginScope(venv);
    {
      A_fieldList l;
      Ty_tyList t;
      for (l = fundecs->head->params, t = formals; l; l = l->tail, t = t->tail)
        S_enter(venv, l->head->name, E_VarEntry(t->head));

      // check return type
      struct expty body = transExp(venv, tenv, fundecs->head->body);
      if (!has_same_ty(result, body.ty))
        EM_error(fundecs->head->pos, "return type mismatched (%s and %s)",
                 type_msg(result), type_msg(body.ty));
    }
    S_endScope(venv);
  }
}

void transDec_typeDec(S_table venv, S_table tenv, A_dec d) {
  for (A_nametyList decs = d->u.type; decs; decs = decs->tail) {
    S_symbol name = decs->head->name;

    for (A_nametyList ds = d->u.type; ds != decs; ds = ds->tail)
      if (ds->head->name == name)
        EM_error(d->pos,
                 "there are two types with the same name in the same "
                 "(consecutive) batch of mutually recursive types");

    S_enter(tenv, name, Ty_Name(name, NULL));
  }
  for (A_nametyList decs = d->u.type; decs; decs = decs->tail) {
    Ty_ty type = S_look(tenv, decs->head->name);
    type->u.name.ty = transTy(tenv, decs->head->ty);
  }
  for (A_nametyList decs = d->u.type; decs; decs = decs->tail) {
    Ty_ty type = S_look(tenv, decs->head->name);
    if (type->u.name.sym == actual_ty(type)->u.name.sym) {
      EM_error(decs->head->ty->pos, "invalid recursive type declaration");
      type->u.name.ty = Ty_Int();
    }
  }
}

Ty_ty transTy(S_table tenv, A_ty a) {
  switch (a->kind) {
    case A_nameTy: {
      return Ty_Name(a->u.name, S_look(tenv, a->u.name));
    }
    case A_recordTy: {
      Ty_fieldList fields, tail = NULL;
      A_fieldList afields = a->u.record;

      for (A_fieldList afields = a->u.record; afields;
           afields = afields->tail) {
        Ty_ty typ = S_look(tenv, afields->head->typ);
        if (!typ) {
          EM_error(afields->head->pos, "type %s mismatched",
                   S_name(afields->head->name));
          typ = Ty_Int();
        }
        fields = Ty_FieldList(Ty_Field(afields->head->name, typ), tail);
        tail = fields;
      }

      return Ty_Record(fields);
    }
    case A_arrayTy: {
      return Ty_Array(S_look(tenv, a->u.array));
    }
  }
}

Ty_ty actual_ty(Ty_ty ty) {
  if (!ty) return NULL;
  if (ty->kind != Ty_name) return ty;

  Ty_ty p = ty->u.name.ty;
  for (; p->kind == Ty_name; p = p->u.name.ty)
    if (p->u.name.sym == ty->u.name.sym) break;
  return p;
}

int has_same_ty(Ty_ty lty, Ty_ty rty) {
  if (lty->kind == Ty_name && rty->kind == Ty_name) {
    Ty_ty l, r;
    for (l = lty; l->u.name.ty->kind == Ty_name; l = l->u.name.ty);
    for (r = rty; r->u.name.ty->kind == Ty_name; r = r->u.name.ty);
    if (l->u.name.sym == r->u.name.sym)
      return 1;
    return 0;
  }
  lty = actual_ty(lty);
  rty = actual_ty(rty);
  if (lty->kind == rty->kind || (lty->kind == Ty_record && rty->kind == Ty_nil))
    return 1;
  return 0;
}

Ty_tyList makeFormalTyList(S_table tenv, A_fieldList afields) {
  Ty_tyList head = Ty_TyList(NULL, NULL), slow = head;
  for (; afields; afields = afields->tail) {
    Ty_tyList fast = Ty_TyList(S_look(tenv, afields->head->typ), NULL);
    slow->tail = fast;
    slow = fast;
  }
  return head->tail;
}

string type_msg(Ty_ty ty) {
  string str = checked_malloc(200 * sizeof(char));
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
      if (ty->u.name.ty)
        strcpy(str, type_msg(actual_ty(ty)));
      else
        strcpy(str, S_name(ty->u.name.sym));
      break;
    case Ty_void:
      strcpy(str, "void");
      break;
  }
  return str;
}
