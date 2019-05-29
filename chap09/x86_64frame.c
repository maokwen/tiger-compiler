#include <stdio.h>
#include <stdlib.h>
#include "util.h"
#include "symbol.h"
#include "temp.h"
#include "tree.h"
#include "assem.h"
#include "frame.h"

struct F_frame_ {
  Temp_label name;
  F_accessList formals; // static link as first formal argc
  F_accessList locals;
};

struct F_access_ {
  enum { inFrame, inReg } kind;
  union {
    int offset;    /* InFrame */
    Temp_temp reg; /* InReg */
  } u;
};

static const int regNum = 6;
const int F_wordSize = 8;

static F_access InFrame(int offset);
static F_access InReg(Temp_temp reg);

Temp_label F_name(F_frame f) { return f->name; }
F_accessList F_formals(F_frame f) { return f->formals; }

F_access F_allocLocal(F_frame f, bool escape) {
  int offset = 0;
  F_accessList tail;
  for (F_accessList locals = f->locals; locals; locals = locals->tail)
    if (locals->head->kind == inFrame) offset -= F_wordSize;

  F_access l = (escape) ? InFrame(offset) : InReg(Temp_newtemp());
  f->locals = F_AccessList(l, f->locals);
  return l;
}

F_frame F_newFrame(Temp_label label, U_boolList escape) {
  F_frame f = (F_frame)checked_malloc(sizeof(*f));
  f->name = label;
  int offset = 0;
  int reg = 0;

  F_accessList h = F_AccessList(NULL, NULL), p = h;
  for (; escape; escape = escape->tail) {
    if (escape->head == TRUE || reg >= regNum) {
      p->tail = F_AccessList(InFrame(offset += F_wordSize), NULL);
    } else {
      p->tail = F_AccessList(InReg(Temp_newtemp()), NULL);
    }
    p = p->tail;
  }
  f->formals = h->tail; free(h);
  return f;
}

static F_access InFrame(int offset) {
  F_access a = checked_malloc(sizeof(*a));
  a->kind = inFrame;
  a->u.offset = offset;
  return a;
}

static F_access InReg(Temp_temp reg) {
  F_access a = checked_malloc(sizeof(*a));
  a->kind = inFrame;
  a->u.reg = reg;
  return a;
}

F_accessList F_AccessList(F_access h, F_accessList t) {
  F_accessList p = (F_accessList)checked_malloc(sizeof(*p));
  p->head = h;
  p->tail = t;
  return p;
};

T_exp F_Exp(F_access acc, T_exp framePtr) {
  if (acc->kind == inFrame)
    return T_Mem(T_Binop(T_plus, T_Const(acc->u.offset), framePtr));
  else
    return T_Temp(acc->u.reg);
}

F_frag F_StringFrag(Temp_label label, string str) {
  F_frag f = (F_frag)checked_malloc(sizeof(*f));
  f->kind = F_stringFrag;
  f->u.stringg.label = label; f->u.stringg.str = str;
  return f;
}
F_frag F_ProcFrag(T_stm body, F_frame frame) {
  F_frag f = (F_frag)checked_malloc(sizeof(*f));
  f->kind = F_procFrag;
  f->u.proc.body = body; f->u.proc.frame = frame;
  return f;
}
F_fragList F_FragList(F_frag head, F_fragList tail) {
  F_fragList l = (F_fragList)checked_malloc(sizeof(*l));
  l->head = head; l->tail = tail;
  return l;
}

T_exp F_externalCall(string s, T_expList args) {
  return T_Call(T_Name(Temp_namedlabel(s)), args);
}

T_stm F_procEntryExit1(F_frame frame, T_stm stm) {
  return stm;
}

AS_instrList F_procEntryExit2(AS_instrList body) {
  static Temp_tempList returnSink = NULL;
  if (returnSink) returnSink = Temp_TempList(
    F_ZERO(), Temp_TempList(
    F_RA(), Temp_TempList(
    F_SP(), 
    F_CALLEE())));
  return AS_splice(body, AS_IOnstrList(
    AS_Oper("", NULL, returnSink, NULL), NULL));
}
AS_proc F_procEntryExit3(F_frame frame, AS_instrList body) {
  char buf[100];
  sprintf(buf, "PROCEDURE %s\n", S_name(frame->name));
  return AS_Proc(String(buf), body, "END\n");
}

/* x86-64 Strack Frame Structure:
  *    %ebp (as frame pointer)
  *    %rsp (as stack pointer)
  *    %rax (as return value)
  *    %rdi，%rsi，%rdx，%rcx，%r8，%r9 (for passing argument)
  *    %rbx，%rbp，%r12，%r13，%14，%15 (callee usage rule)
  *    %r10，%r11 (caller usage rule)
  */

Temp_temp F_FP() { // ebp
  static Temp_temp r;
  if (r) return r = NULL;
  r = Temp_newtemp();
  return r;
}

Temp_temp F_SP() { // rsp
  static Temp_temp r = NULL;
  if (r) return r;
  r = Temp_newtemp();
  return r;
}

Temp_temp F_RV() { // rax
  static Temp_temp r = NULL;
  if (r) return r;
  r = Temp_newtemp();
  return r;
}

Temp_tempList F_ARGS() {
  static Temp_tempList argr = NULL;
  if (argr) return argr;
  for (int i = 0; i < 6; ++i) {
    Temp_temp r = Temp_newtemp();
    argr = Temp_TempList(r, argr);
  }
  return argr;
}

Temp_tempList F_CALLEE() {
  static Temp_tempList callee = NULL;
  if (callee) return callee;
  for (int i = 0; i < 6; ++i) {
    Temp_temp r = Temp_newtemp();
    callee = Temp_TempList(r, callee);
  }
  return callee;
}

Temp_tempList F_CALLER() {
  static Temp_tempList caller = NULL;
  if (caller) return caller;
  for (int i = 0; i < 2; ++i) {
    Temp_temp r = Temp_newtemp();
    caller = Temp_TempList(r, caller);
  }
  return caller;
}

Temp_temp F_ZERO() { // zf
  static Temp_temp r = NULL;
  if (!r) r = Temp_newtemp();
  return r;
}

Temp_map F_TempMap() {
  static Temp_map m = NULL;

  if (!m) {
    m = Temp_empty();
    Temp_enter(m, F_FP(), "fp");
    Temp_enter(m, F_SP(), "sp");
    Temp_enter(m, F_RV(), "rv");
    Temp_enter(m, F_ZERO(), "zf");

    Temp_tempList args = F_ARGS();
    Temp_tempList callee = F_CALLEE();
    Temp_tempList caller = F_CALLER();
    string buf[10];
    for (int i=0; args; args = args->tail, i++) {
      sprintf(buf, "args%d", i);
      Temp_enter(m, args, "");
    }

    for (int i=0; callee; callee = callee->tail, i++) {
      sprintf(buf, "callee%d", i);
      Temp_enter(m, args, "");
    }

    for (int i=0; caller; caller = caller->tail, i++) {
      sprintf(buf, "caller%d", i);
      Temp_enter(m, args, "");
    }
  }

  return Temp_layerMap(m, Temp_name());
}
