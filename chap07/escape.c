#include "absyn.h"
#include "symbol.h"
#include "escape.h"

void Esc_findEscape(A_exp exp) {
}

static void traverseExp(S_table env, int depth, A_exp e);
static void traverseDec(S_table env, int depth, A_exp e);
static void traverseVar(S_table env, int depth, A_exp e);
