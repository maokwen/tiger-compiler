/* sement.h */

void SEM_transProg(A_exp exp);

struct expty {Tr_exp exp; Ty_ty ty;};

struct expty expTy(Tr_exp exp, Ty_ty ty) {
  struct expty e; e.exp=exp; e.ty=ty; return e;
}

struct expty transVar(Tr_level level, S_table venv, S_table tenv, A_var v);
struct expty transExp(Tr_level level, S_table venv, S_table tenv, A_exp a);
void         transDec(Tr_level level, S_table venv, S_table tenv, A_dec d);
       Ty_ty transTy (Tr_level level,               S_table tenv,  A_ty a);
