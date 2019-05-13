/* translate.h */

typedef struct Tr_access_ *Tr_access;

typedef struct Tr_level_ *Tr_level;

typedef struct Tr_accessList_ *Tr_accessList;
struct Tr_accessList_ { Tr_access head; Tr_accessList tail; };
Tr_accessList Tr_AccessList(Tr_access head, Tr_accessList tail);

Tr_level Tr_outermost(void);
Tr_level Tr_newLevel(Tr_level parent, Temp_label name, U_boolList formals);
Tr_accessList Tr_formals(Tr_level level);
Tr_access Tr_allocLocal(Tr_level level, bool escape);

typedef struct Tr_exp_ *Tr_exp;
typedef struct Tr_expList_ *Tr_expList;
struct Tr_expList_ { Tr_exp head; Tr_expList tail; };
Tr_expList Tr_ExpList(Tr_exp head, Tr_expList tail);

Tr_exp Tr_noExp();

Tr_exp Tr_nilExp();
Tr_exp Tr_intExp(int);
Tr_exp Tr_stringExp(string);

Tr_exp Tr_simpleVar(Tr_access, Tr_level);
Tr_exp Tr_fieldVar(Tr_exp, int, Tr_level);
Tr_exp Tr_subscriptVar(Tr_exp, Tr_exp, Tr_level);

Tr_exp Tr_addOpExp(Tr_exp, Tr_exp);
Tr_exp Tr_minusOpExp(Tr_exp, Tr_exp);
Tr_exp Tr_timesOpExp(Tr_exp, Tr_exp);
Tr_exp Tr_divideOpExp(Tr_exp, Tr_exp);
Tr_exp Tr_ltOpExp(Tr_exp, Tr_exp);
Tr_exp Tr_leOpExp(Tr_exp, Tr_exp);
Tr_exp Tr_gtOpExp(Tr_exp, Tr_exp);
Tr_exp Tr_geOpExp(Tr_exp, Tr_exp);
Tr_exp Tr_eqExp(Tr_exp, Tr_exp);
Tr_exp Tr_neqExp(Tr_exp, Tr_exp);
Tr_exp Tr_stringEqExp(Tr_exp l, Tr_exp r);
Tr_exp Tr_stringNeExp(Tr_exp l, Tr_exp r);

Tr_exp Tr_ifExp(Tr_exp, Tr_exp, Tr_exp);
Tr_exp Tr_ifExp_noValue(Tr_exp, Tr_exp, Tr_exp);

Tr_exp Tr_recordExp(Tr_expList fields, int size);
Tr_exp Tr_arrayExp(Tr_exp size, Tr_exp init);

Tr_exp Tr_whileExp(Tr_exp cond, Tr_exp body, Temp_label done);
Tr_exp Tr_breakExp(Temp_label done);

Tr_exp Tr_callExp(Temp_label name, Tr_expList args, Tr_level cur, Tr_level lev);

Tr_exp Tr_assignExp(Tr_exp lvar, Tr_exp rvar);

Tr_exp Tr_LetExp(Tr_expList decs, Tr_exp body);

Tr_exp Tr_eseqExp(Tr_expList list);
Tr_exp Tr_seqStm(Tr_expList list);

void Tr_procEntryExit(Tr_level level, Tr_exp body, Tr_accessList formals);
F_fragList Tr_getResult(void);

T_stm F_procEntryExit1(F_frame frame, T_stm stm);

void Tr_printTree(Tr_exp e);
