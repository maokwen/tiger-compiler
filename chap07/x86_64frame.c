#include <stdio.h>
#include <stdlib.h>
#include "util.h"
#include "symbol.h"
#include "temp.h"
#include "frame.h"

struct F_frame_ {
  Temp_label label;
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
static const int F_wordSize = 8;

static F_access InFrame(int offset);
static F_access InReg(Temp_temp reg);

Temp_label F_name(F_frame f) { return f->label; }
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
  f->label = label;
  int offset = 0;
  int reg = 0;

  F_accessList h = F_AccessList(NULL, NULL), p = h;
  for (; escape; escape = escape->tail) {
    if (escape->head == TRUE || reg >= regNum) {
      p->tail = F_AccessList(InFrame(offset += F_wordSize), p);
    } else {
      p->tail = F_AccessList(InReg(Temp_newtemp()), p);
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
