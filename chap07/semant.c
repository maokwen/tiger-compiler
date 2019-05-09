#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "util.h"
#include "errormsg.h"
#include "symbol.h"
#include "absyn.h"
#include "types.h"
#include "temp.h"
#include "translate.h"
#include "env.h"
#include "semant.h"

void SEM_transProg(A_exp exp) {
  S_table tenv = E_base_tenv();
  S_table venv = E_base_venv();
  Tr_level level = Tr_outermost();
  transExp(level, venv, tenv, exp);
}

Ty_ty actual_ty(Ty_ty ty);
int has_same_ty(Ty_ty ty1, Ty_ty ty2);
Ty_tyList makeFormalTyList(S_table tenv, A_fieldList afields);
string type_msg(Ty_ty ty);

/**
 * Translate Expression
 */

struct expty transExp_callExp(Tr_level level, S_table venv, S_table tenv, A_exp a);
struct expty transExp_opExp(Tr_level level, S_table venv, S_table tenv, A_exp a);
struct expty transExp_recordExp(Tr_level level, S_table venv, S_table tenv, A_exp a);
struct expty transExp_seqExp(Tr_level level, S_table venv, S_table tenv, A_exp a);
struct expty transExp_assignExp(Tr_level level, S_table venv, S_table tenv, A_exp a);
struct expty transExp_ifExp(Tr_level level, S_table venv, S_table tenv, A_exp a);
struct expty transExp_whileExp(Tr_level level, S_table venv, S_table tenv, A_exp a);
struct expty transExp_forExp(Tr_level level, S_table venv, S_table tenv, A_exp a);
struct expty transExp_letExp(Tr_level level, S_table venv, S_table tenv, A_exp a);
struct expty transExp_arrayExp(Tr_level level, S_table venv, S_table tenv, A_exp a);

struct expty transExp(Tr_level level, S_table venv, S_table tenv, A_exp a) {
  switch (a->kind) {
    case A_varExp:
      return transVar(level, venv, tenv, a->u.var);
    case A_nilExp:
      return expTy(Tr_nilExp(), Ty_Nil());
    case A_intExp:
      return expTy(Tr_intExp(a->u.intt), Ty_Int());
    case A_stringExp:
      return expTy(Tr_stringExp(a->u.stringg), Ty_String());
    case A_callExp:
      return transExp_callExp(level, venv, tenv, a);
    case A_opExp:
      return transExp_opExp(level, venv, tenv, a);
    case A_recordExp:
      return transExp_recordExp(level, venv, tenv, a);
    case A_seqExp:
      return transExp_seqExp(level, venv, tenv, a);
    case A_assignExp:
      return transExp_assignExp(level, venv, tenv, a);
    case A_ifExp:
      return transExp_ifExp(level, venv, tenv, a);
    case A_whileExp:
      return transExp_whileExp(level, venv, tenv, a);
    case A_forExp:
      return transExp_forExp(level, venv, tenv, a);
    case A_breakExp:
      return expTy(NULL, Ty_Void());
    case A_letExp:
      return transExp_letExp(level, venv, tenv, a);
    case A_arrayExp:
      return transExp_arrayExp(level, venv, tenv, a);
  }
  assert(0); /* should have returned from some clause of the switch */
}

struct expty transExp_callExp(Tr_level level, S_table venv, S_table tenv, A_exp a) {
  E_enventry x = S_look(venv, a->u.call.func);
  A_pos pos = a->pos;

  // check func type
  if (!x || x->kind != E_funEntry) {
    EM_error(pos, "undeclared variable '%s'", S_name(a->u.call.func));
    return expTy(NULL, Ty_Void());
  }

  // check every arglist type
  A_expList args = a->u.call.args;
  Ty_tyList formals = x->u.fun.formals;
  while (args && formals) {
    struct expty arg = transExp(level, venv, tenv, args->head);
    pos = args->head->pos;
    if (!has_same_ty(formals->head, arg.ty)) {
      EM_error(args->head->pos, "formals and actuals have different types");
      break;
    }
    args = args->tail;
    formals = formals->tail;
  }
  if (args || formals) EM_error(pos, "formals and actuals have different types");

  return expTy(NULL, actual_ty(x->u.fun.result));
}

struct expty transExp_opExp(Tr_level level, S_table venv, S_table tenv, A_exp a) {
  A_oper oper = a->u.op.oper;
  struct expty left = transExp(level, venv, tenv, a->u.op.left);
  struct expty right = transExp(level, venv, tenv, a->u.op.right);

  if (oper == A_plusOp || oper == A_minusOp || oper == A_timesOp ||
      oper == A_divideOp || oper == A_ltOp || oper == A_leOp ||
      oper == A_gtOp || oper == A_geOp) {
    if (left.ty->kind != Ty_int)
      EM_error(a->u.op.left->pos, "operate of incompatible types");
    if (right.ty->kind != Ty_int)
      EM_error(a->u.op.right->pos, "operate of incompatible types");
  }

  if (oper == A_eqOp || oper == A_neqOp) {
    if (left.ty->kind != right.ty->kind &&
        !(left.ty->kind == Ty_nil && right.ty->kind == Ty_record) &&
        !(left.ty->kind == Ty_record && right.ty->kind == Ty_nil))
      EM_error(a->u.op.right->pos, "same type required");
  }

  switch (oper) {
    case A_plusOp:    Tr_addOpExp(   left.exp, right.exp);
    case A_minusOp:   Tr_minusOpExp( left.exp, right.exp);
    case A_timesOp:   Tr_timespExp(  left.exp, right.exp);
    case A_divideOp:  Tr_divideOpExp(left.exp, right.exp);
    case A_ltOp:      Tr_ltOpExp(    left.exp, right.exp);
    case A_leOp:      Tr_leOpExp(    left.exp, right.exp);
    case A_gtOp:      Tr_gtOpExp(    left.exp, right.exp);
    case A_geOp:      Tr_geOpExp(    left.exp, right.exp);
    case A_eqOp:      Tr_eqExp(      left.exp, right.exp);
    case A_neqOp:     Tr_neqExp(     left.exp, right.exp);
  }

  return expTy(NULL, Ty_Int());
}

struct expty transExp_recordExp(Tr_level level, S_table venv, S_table tenv, A_exp a) {
  Ty_ty typ = actual_ty(S_look(tenv, a->u.record.typ));
  A_pos pos = a->pos;

  // check record type
  if (!typ || typ->kind != Ty_record) {
    EM_error(pos, "record type '%s' mismatched", S_name(a->u.record.typ));
    return expTy(NULL, Ty_Void());
  }

  // check exp field type
  A_efieldList efields = a->u.record.fields;
  Ty_fieldList fields = typ->u.record;
  Tr_expList expl_h = Tr_ExpList(NULL, NULL), expl_p = expl_h;
  int size = 0;
  while (efields && fields) {
    struct expty efield = transExp(level, venv, tenv, efields->head->exp);
    pos = efields->head->exp->pos;
    if (!has_same_ty(fields->head->ty, efield.ty)) {
      EM_error(pos, "record field type mismatched: '%s' and '%s'",
               type_msg(fields->head->ty), type_msg(efield.ty));
      break;
    }
    efields = efields->tail;
    fields = fields->tail;
    expl_p->tail = r_ExpList(efield.exp, NULL);
    size += 1;
  }
  if (efields || fields) EM_error(pos, "record type mismatched");

  expl_p = expl_h->tail;
  free(expl_h);
  return expTy(Tr_recordExp(expl_p, size), S_look(tenv, a->u.record.typ));
}

struct expty transExp_seqExp(Tr_level level, S_table venv, S_table tenv, A_exp a) {
  A_expList seq;
  for (seq = a->u.seq; seq && seq->tail; seq = seq->tail)
    transExp(level, venv, tenv, seq->head);

  if (!seq || !seq->head) return expTy(NULL, Ty_Void());
  return transExp(level, venv, tenv, seq->head);
}

struct expty transExp_assignExp(Tr_level level, S_table venv, S_table tenv, A_exp a) {
  struct expty lvar = transVar(level, venv, tenv, a->u.assign.var);
  struct expty rvar = transExp(level, venv, tenv, a->u.assign.exp);

  if (!has_same_ty(lvar.ty, rvar.ty))
    EM_error(
        a->u.assign.exp->pos,
        "cannot initialize a variable of type '%s' with an rvalue of type '%s'",
        type_msg(lvar.ty), type_msg(rvar.ty));

  return expTy(NULL, Ty_Void());
}

struct expty transExp_ifExp(Tr_level level, S_table venv, S_table tenv, A_exp a) {
  struct expty test = transExp(level, venv, tenv, a->u.iff.test);
  struct expty then = transExp(level, venv, tenv, a->u.iff.then);

  if (test.ty->kind != Ty_int)
    EM_error(a->u.iff.test->pos, "integer type required");

  if (a->u.iff.elsee) {
    struct expty elsee = transExp(level, venv, tenv, a->u.iff.elsee);
    if (!has_same_ty(then.ty, elsee.ty))
      EM_error(a->u.iff.elsee->pos, "types of then - else differ ('%s' and '%s')",
               type_msg(then.ty), type_msg(elsee.ty));
    if (then.ty == Ty_Void())
       return expTy(Tr_ifExp_noValue(test.exp, then.exp, elsee.exp), Ty_Void());
    return expTy(Tr_ifExp(test.exp, then.exp, elsee.exp), then.ty);
  }

  if (then.ty->kind != Ty_void)
    EM_error(a->u.iff.then->pos, "if-then returns non unit");

  return expTy(Tr_ifExp_noValue(test.exp, then.exp, NULL), then.ty);
  return expTy(NULL, Ty_Void());
}

struct expty transExp_whileExp(Tr_level level, S_table venv, S_table tenv, A_exp a) {
  struct expty test = transExp(level, venv, tenv, a->u.whilee.test);
  struct expty body = transExp(level, venv, tenv, a->u.whilee.body);

  if (test.ty->kind != Ty_int)
    EM_error(a->u.whilee.test->pos, "expected unqualified-id");
  if (body.ty->kind != Ty_void)
    EM_error(a->u.whilee.test->pos, "body of while not unit");

  return expTy(Tr_whileExp(test.exp, body.exp), Ty_Void());
}

struct expty transExp_forExp(Tr_level level, S_table venv, S_table tenv, A_exp a) {
  struct expty lo = transExp(level, venv, tenv, a->u.forr.lo);
  struct expty hi = transExp(level, venv, tenv, a->u.forr.hi);

  if (lo.ty->kind != Ty_int || hi.ty->kind != Ty_int)
    EM_error(a->u.forr.lo->pos, "lo or hi expr is not int");

  /*
   * LET VAR i := lo
   *     VAR lmt := hi
   * IN
   *    IF lo < hi THEN
   *      WHILE i <= lmt DO
   *        (body;
   *         i := i+1)
   */
  A_pos pos1 = a->pos;
  A_pos pos2 = a->u.forr.body->pos;
  S_symbol var = a->u.forr.var;
  S_symbol lmt = S_Symbol("limit");
  S_symbol vlo = a->u.forr.var;
  S_symbol vhi = a->u.forr.var;
  A_exp ebody = a->u.forr.body;
  A_exp transformed = A_LetExp(pos1,
      A_DecList(A_VarDec(pos1, var, S_Symbol("int"), vlo),
      A_DecList(A_VarDec(pos1, lmt, S_Symbol("int"), vhi), NULL)),
          A_IfExp(pos1,
              A_OpExp(pos1, A_ltOp, vlo, vhi),
              A_WhileExp(pos1,
                  A_OpExp(pos1, A_leOp, var, lmt),
                  A_SeqExp(pos2, A_ExpList(ebody, 
                                 A_ExpList(A_OpExp(pos1, A_plusOp, var, A_IntExp(pos1, 1)), NULL)))),
              NULL)
  );
  return transExp(level, venv, tenv, transformed);
}

struct expty transExp_letExp(Tr_level level, S_table venv, S_table tenv, A_exp a) {
  struct expty exp;
  S_beginScope(venv);
  S_beginScope(tenv);
  for (A_decList d = a->u.let.decs; d; d = d->tail)
    transDec(level, venv, tenv, d->head);
  exp = transExp(level, venv, tenv, a->u.let.body);
  S_endScope(venv);
  S_endScope(tenv);

  return exp;
}

struct expty transExp_arrayExp(Tr_level level, S_table venv, S_table tenv, A_exp a) {
  Ty_ty typ = actual_ty(S_look(tenv, a->u.array.typ));
  struct expty size = transExp(level, venv, tenv, a->u.array.size);
  struct expty init = transExp(level, venv, tenv, a->u.array.init);

  if (!typ || typ->kind != Ty_array) {
    EM_error(a->pos, "array type '%s' mismatched", S_name(a->u.array.typ));
    return expTy(NULL, Ty_Void());
  }

  if (size.ty->kind != Ty_int)
    EM_error(a->u.array.size->pos, "integer type required");

  if (!has_same_ty(init.ty, typ->u.array))
    EM_error(
        a->u.array.init->pos,
        "cannot initialize a variable of type '%s' with an rvalue of type '%s'",
        type_msg(typ), type_msg(init.ty));

  return expTy(Tr_arrayExp(size.exp, init.exp), S_look(tenv, a->u.array.typ));
}

/**
 * Translate Variable
 */

struct expty transVar_simpleVar(Tr_level level, S_table venv, S_table tenv, A_var v);
struct expty transVar_fieldVar(Tr_level level, S_table venv, S_table tenv, A_var v);
struct expty transVar_subscriptVar(Tr_level level, S_table venv, S_table tenv, A_var v);

struct expty transVar(Tr_level level, S_table venv, S_table tenv, A_var v) {
  switch (v->kind) {
    case A_simpleVar:
      return transVar_simpleVar(level, venv, tenv, v);
    case A_fieldVar:
      return transVar_fieldVar(level, venv, tenv, v);
    case A_subscriptVar:
      return transVar_subscriptVar(level, venv, tenv, v);
  }
}

struct expty transVar_simpleVar(Tr_level level, S_table venv, S_table tenv, A_var v) {
  E_enventry x = S_look(venv, v->u.simple);

  if (!x || x->kind != E_varEntry) {
    EM_error(v->pos, "undeclared variable '%s'", S_name(v->u.simple));
    return expTy(NULL, Ty_Int());
  }

  return expTy(Tr_simpleVar(x->u.var.access, level),
               actual_ty(x->u.var.ty));
}

struct expty transVar_fieldVar(Tr_level level, S_table venv, S_table tenv, A_var v) {
  struct expty var = transVar(level, venv, tenv, v->u.field.var);

  if (var.ty->kind != Ty_record)
    EM_error(v->pos, "invalid types '%s' for record field", type_msg(var.ty));

  int index = 0;
  Ty_fieldList fields;
  for (fields = var.ty->u.record; fields != NULL; fields = fields->tail, index += 1)
    if (fields->head->name == v->u.field.sym) break;
  if (!fields) {
    EM_error(v->pos, "field '%s' not defined in record type", S_name(v->u.field.sym));
    return expTy(NULL, Ty_Int());
  }
  return expTy(Tr_fieldVar(var.exp, index, level),
               actual_ty(fields->head->ty));
}

struct expty transVar_subscriptVar(Tr_level level, S_table venv, S_table tenv, A_var v) {
  struct expty var = transVar(level, venv, tenv, v->u.subscript.var);
  struct expty sub = transExp(level, venv, tenv, v->u.subscript.exp);

  if (var.ty->kind != Ty_array) {
    EM_error(v->u.subscript.var->pos, "variable type '%s' is not array",
             type_msg(var.ty));
    return expTy(NULL, Ty_Int());
  }
  if (sub.ty->kind != Ty_int) {
    EM_error(v->u.subscript.exp->pos, "invalid types '%s' for array subscript",
             type_msg(sub.ty));
    return expTy(NULL, Ty_Int());
  }

  return expTy(Tr_subscriptVar(var.exp, sub.exp, level),
               actual_ty(var.ty->u.array));
}

/**
 * Translate Declearion
 */

void transDec_varDec(Tr_level level, S_table venv, S_table tenv, A_dec d);
void transDec_functionDec(Tr_level level, S_table venv, S_table tenv, A_dec d);
void transDec_typeDec(Tr_level level, S_table venv, S_table tenv, A_dec d);

void transDec(Tr_level level, S_table venv, S_table tenv, A_dec d) {
  switch (d->kind) {
    case A_varDec:
      transDec_varDec(level, venv, tenv, d);
      break;
    case A_functionDec:
      transDec_functionDec(level, venv, tenv, d);
      break;
    case A_typeDec:
      transDec_typeDec(level, venv, tenv, d);
      break;
  }
}

void transDec_varDec(Tr_level level, S_table venv, S_table tenv, A_dec d) {
  struct expty init = transExp(level, venv, tenv, d->u.var.init);

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

  S_enter(venv, d->u.var.var, E_VarEntry(Tr_allocLocal(level, TRUE), init.ty)); // todo check escape = false
}

void transDec_functionDec(Tr_level level, S_table venv, S_table tenv, A_dec d) {
  for (A_fundecList fundecs = d->u.function; fundecs; fundecs = fundecs->tail) {
    S_symbol name = fundecs->head->name;
    Ty_tyList formals = makeFormalTyList(tenv, fundecs->head->params);

    for (A_fundecList f = d->u.function; f != fundecs; f = f->tail)
      if (f->head->name == name)
        EM_error(f->head->pos,
                 "there are two functions with the same name in the same batch "
                 "of mutually recursive functions");

    Ty_ty result = (fundecs->head->result)? S_look(tenv, fundecs->head->result) : Ty_Void();
    Temp_label label = Temp_newlabel();
    U_boolList escapeList = NULL;
    for (A_fieldList l = fundecs->head->params; l; l = l->tail)
      // todo: handle no escape
      escapeList = U_BoolList(TRUE, escapeList);

    S_enter(venv, name,
            E_FunEntry(Tr_newLevel(level, label, escapeList), label, formals,
                      result));
  }

  for (A_fundecList fundecs = d->u.function; fundecs; fundecs = fundecs->tail) {
    S_symbol name = fundecs->head->name;
    E_enventry x = S_look(venv, name);
    Ty_tyList formals = x->u.fun.formals;
    Ty_ty result = x->u.fun.result;
    Tr_level lev = x->u.fun.level;

    S_beginScope(venv);
    {
      A_fieldList l;
      Ty_tyList t;
      Tr_accessList a;
      for (l = fundecs->head->params, t = formals, a = Tr_formals(lev);
           l;
           l = l->tail, t = t->tail, a = a->tail)
        S_enter(venv, l->head->name, E_VarEntry(a->head, t->head));

      // check return type
      struct expty body = transExp(level, venv, tenv, fundecs->head->body);
      if (!has_same_ty(result, body.ty)) {
        if (has_same_ty(result, Ty_Void()))
          EM_error(fundecs->head->pos, "procedure returns value '%s'",
                  type_msg(body.ty));
        else
          EM_error(fundecs->head->pos, "return type mismatched '%s' and '%s')",
                  type_msg(result), type_msg(body.ty));
      }
    }
    S_endScope(venv);
  }
}

void transDec_typeDec(Tr_level level, S_table venv, S_table tenv, A_dec d) {
  for (A_nametyList decs = d->u.type; decs; decs = decs->tail) {
    S_symbol name = decs->head->name;

    for (A_nametyList ds = d->u.type; ds != decs; ds = ds->tail)
      if (ds->head->name == name)
        EM_error(d->pos,
                 "there are two types with the same name in the same batch of "
                 "mutually recursive types");

    S_enter(tenv, name, Ty_Name(name, NULL));
  }
  for (A_nametyList decs = d->u.type; decs; decs = decs->tail) {
    Ty_ty type = S_look(tenv, decs->head->name);
    type->u.name.ty = transTy(level, tenv, decs->head->ty);
  }
  for (A_nametyList decs = d->u.type; decs; decs = decs->tail) {
    Ty_ty type = S_look(tenv, decs->head->name);
    if (type->u.name.sym == actual_ty(type)->u.name.sym) {
      EM_error(decs->head->ty->pos, "mutually recursive types declaration");
      type->u.name.ty = Ty_Int();
    }
  }
}

Ty_ty transTy(Tr_level level, S_table tenv, A_ty a) {
  switch (a->kind) {
    case A_nameTy: {
      return Ty_Name(a->u.name, S_look(tenv, a->u.name));
    }
    case A_recordTy: {
      Ty_fieldList h = Ty_FieldList(NULL, NULL), p = h, fields;
      for (A_fieldList efields = a->u.record; efields;
           efields = efields->tail) {
        Ty_ty typ = S_look(tenv, efields->head->typ);
        if (!typ) {
          EM_error(efields->head->pos, "type '%s' undefined",
                   S_name(efields->head->name));
          typ = Ty_Int();
        }
        p->tail = Ty_FieldList(Ty_Field(efields->head->name, typ), NULL);
        p = p->tail;
      }
      fields = h->tail;
      free(h);

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
  for (; p && p->kind == Ty_name; p = p->u.name.ty)
    if (p == ty) break;
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
