#include <stdio.h>  // for NULL
#include "util.h"
#include "errormsg.h"
#include "symbol.h"
#include "absyn.h"
#include "types.h"
#include "temp.h"
#include "frame.h"
#include "translate.h"
#include "env.h"

E_enventry E_VarEntry(Tr_access access, Ty_ty ty) {
  E_enventry env=checked_malloc(sizeof(*env));
  env->kind=E_varEntry; env->u.var.ty=ty;
  env->u.var.access = access;
  return env;
}
E_enventry E_FunEntry(Tr_level level, Temp_label label, Ty_tyList formals,
                      Ty_ty result) {
  E_enventry env = checked_malloc(sizeof(*env));
  env->kind = E_funEntry;
  env->u.fun.level = level;
  env->u.fun.label = label;
  env->u.fun.formals = formals;
  env->u.fun.result = result;
  return env;
}

S_table E_base_tenv(void) {
  S_table tenv=S_empty();
  S_enter(tenv, S_Symbol("nil"), Ty_Nil());
  S_enter(tenv, S_Symbol("int"), Ty_Int());
  S_enter(tenv, S_Symbol("string"), Ty_String());
  S_enter(tenv, S_Symbol("void"), Ty_Void());
  return tenv;
}

S_table E_base_venv(void) {
  S_table venv = S_empty();
  S_enter(venv, S_Symbol("print"),
          E_FunEntry(Tr_outermost(), Temp_namedlabel("print"),
                     Ty_TyList(Ty_String(), NULL), Ty_Void()));
  S_enter(venv, S_Symbol("flush"),
      E_FunEntry(Tr_outermost(), Temp_namedlabel("flush"), NULL, Ty_Void()));
  S_enter(venv, S_Symbol("getchar"),
          E_FunEntry(Tr_outermost(), Temp_namedlabel("getchar"), NULL,
                     Ty_String()));
  S_enter(venv, S_Symbol("ord"),
          E_FunEntry(Tr_outermost(), Temp_namedlabel("ord"),
                     Ty_TyList(Ty_String(), NULL), Ty_Int()));
  S_enter(venv, S_Symbol("chr"),
          E_FunEntry(Tr_outermost(), Temp_namedlabel("chr"),
                     Ty_TyList(Ty_Int(), NULL), Ty_String()));
  S_enter(venv, S_Symbol("size"),
          E_FunEntry(Tr_outermost(), Temp_namedlabel("size"),
                     Ty_TyList(Ty_String(), NULL), Ty_Int()));
  S_enter(venv, S_Symbol("substring"),
          E_FunEntry(Tr_outermost(), Temp_namedlabel("substring"),
                     Ty_TyList(Ty_String(),
                               Ty_TyList(Ty_Int(), Ty_TyList(Ty_Int(), NULL))),
                     Ty_String()));
  S_enter(venv, S_Symbol("concat"),
          E_FunEntry(Tr_outermost(), Temp_namedlabel("concat"),
                     Ty_TyList(Ty_String(), Ty_TyList(Ty_String(), NULL)),
                     Ty_String()));
  S_enter(venv, S_Symbol("not"),
          E_FunEntry(Tr_outermost(), Temp_namedlabel("not"),
                     Ty_TyList(Ty_Int(), NULL), Ty_Int()));
  S_enter(venv, S_Symbol("exit"),
          E_FunEntry(Tr_outermost(), Temp_namedlabel("exit"),
                     Ty_TyList(Ty_Int(), NULL), Ty_Void()));
  return venv;
}
