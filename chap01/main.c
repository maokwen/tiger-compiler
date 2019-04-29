#include "util.h"
#include "slp.h"
#include "prog1.h"
#include <stdio.h>

/**
 * 1. Write a function int maxargs(A_stm) that tells the maximum number of
 * arguments of any print statement within any subexpression of a given
 * statement. For example,maxargs(prog) is 2.
 */

int maxargs(A_stm);
void interp(A_stm);

// Impletement

int maxargs_exp(A_exp e) {
  switch (e->kind) {
    case A_idExp:  return 0;
    case A_numExp: return 0;
    case A_opExp:
      return maxargs_exp(e->u.op.left) + maxargs_exp(e->u.op.right);
    case A_eseqExp:
      return maxargs(e->u.eseq.stm) + maxargs_exp(e->u.eseq.exp);
  }
}

int maxargs_exp_list(A_expList el) {
  if (el->kind == A_lastExpList)
    return maxargs_exp(el->u.last);
  else
    return maxargs_exp(el->u.pair.head) + maxargs_exp_list(el->u.pair.tail);
}

int maxargs(A_stm s) {
  switch (s->kind) {
    case A_compoundStm:
      return maxargs(s->u.compound.stm1) + maxargs(s->u.compound.stm2);
    case A_assignStm:
      return maxargs_exp(s->u.assign.exp);
    case A_printStm:
      return 1 + maxargs_exp_list(s->u.print.exps);
  }
}

/**
 * 2. Write a function void interp(A_stm) that “interprets” a programin this
 * language.To write in a “functionalprogramming”style – in which younever use
 * an assignment statement– initialize eachlocal variable as you declareit.
 */

void interp(A_stm s);

typedef struct table* Table_;
struct table {string id; int value; Table_ tail;};
Table_ Table(string id, int value, struct table* tail) {
  Table_ t = malloc(sizeof(*t));
  t->id = id; t->value = value; t->tail = tail;
  return t;
}

Table_ interpStm(A_stm s, Table_ t);
Table_ update(Table_ t, string id, int value);

struct IntAndTable {int i; Table_ t;};
struct IntAndTable IntAndTable(int i, Table_ t) {
  struct IntAndTable it;
  it.i = i; it.t = t;
  return it;
}
struct IntAndTable interpExp(A_exp e, Table_ t);
int lookup(Table_ t, string key);

// Impletement

void interp(A_stm s) {
  Table_ t = NULL;
  interpStm(s, t);
}

Table_ interpPrintExpList(A_expList el, Table_ t) {
  if (el->kind == A_lastExpList) {
    struct IntAndTable it = interpExp(el->u.last, t);
    printf("%s\n", it.i);
    return it.t;
  } else {
    struct IntAndTable it = interpExp(el->u.pair.head, t);
    printf("%d\n", it.i);
    return interpPrintExpList(el->u.pair.tail, it.t);
  }
}

Table_ interpStm(A_stm s, Table_ t) {
  switch (s->kind) {
    case A_compoundStm:
      return interpStm(s->u.compound.stm1, interpStm(s->u.compound.stm2, t));
    case A_assignStm:
      return update(t, s->u.assign.id, interpExp(s->u.assign.exp, t).i);
    case A_printStm: {
      return interpPrintExpList(s->u.print.exps, t);
    }
  }
}

struct IntAndTable interpExp(A_exp e, Table_ t) {
  switch (e->kind) {
    case A_idExp:
      return IntAndTable(lookup(t, e->u.id), t);
    case A_numExp:
      return IntAndTable(e->u.num, t);
    case A_opExp: {
      struct IntAndTable left = interpExp(e->u.op.left, t);
      struct IntAndTable right = interpExp(e->u.op.right, left.t);
      switch (e->u.op.oper) {
        case A_plus:
          return IntAndTable(left.i + right.i, right.t);
        case A_minus:
          return IntAndTable(left.i - right.i, right.t);
        case A_times:
          return IntAndTable(left.i * right.i, right.t);
        case A_div:
          return IntAndTable(left.i / right.i, right.t);
      }
    }
    case A_eseqExp:
      return interpExp(e->u.eseq.exp, interpStm(e->u.eseq.stm, t));
  }
}

Table_ update(Table_ t, string id, int value) {
  return Table(id, value, t);
}

int lookup(Table_ t, string key) {
  if (t == NULL) exit(-1);
  else if (t->id == key)
    return t->value;
  else
    lookup(t->tail, key);

}

/**
 * Main
 */

int main() {
  interp(prog());
}
