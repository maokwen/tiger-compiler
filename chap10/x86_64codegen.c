/* x86-64 Strack Frame Structure:
  *    %rax (as return value)
  *    %rsp (as strack pointer)
  *    %rdi，%rsi，%rdx，%rcx，%r8，%r9 (for passing argument)
  *    %rbx，%rbp，%r12，%r13，%14，%15 (callee usage rule)
  *    %r10，%r11 (caller usage rule)
  */
#include <stdlib.h>
#include "symbol.h"
#include "errormsg.h"
#include "util.h"
#include "symbol.h"
#include "temp.h"
#include "tree.h"
#include "assem.h"
#include "frame.h"
#include "printtree.h"
#include "escape.h"
#include "codegen.h"

static AS_instrList iList = NULL, iList_last = NULL;
static void emit(AS_instr instr) {
  if (iList) iList_last = iList->tail = AS_InstrList(instr, NULL);
  else       iList_last = iList = AS_InstrList(instr, NULL);
}

static void emit(AS_instr instr);
static Temp_temp munchExp(T_exp e);
static void munchStm(T_stm s);
static Temp_tempList munchArgs(int n, T_expList args);

Temp_tempList singleTemp(Temp_temp t) {
  return singleTemp(t);
}

AS_instrList F_codegen(F_frame f, T_stmList stmList) {
  AS_instrList list;

  for (T_stmList sl = stmList; sl; sl = sl->tail) munchStm(sl->head);
  list = iList; iList = iList_last = NULL;
  return list;
}

static Temp_temp munchExp(T_exp e) {
  char buf[100];
  switch (e->kind) {
    case T_MEM: {
      Temp_temp r = Temp_newtemp();
      /* MEM(BINOP(PLUS,e1,CONST(i))) and
       * MEM(BINOP(PLUS,CONST(i),e1)) */
      if (e->u.MEM->kind == T_BINOP) {
        T_exp op = e->u.MEM->u.BINOP.op,
              left = e->u.MEM->u.BINOP.left,
              right = e->u.MEM->u.BINOP.right;
        assert(op == T_plus && (left->kind == T_CONST || right->kind == T_CONST));
       
        if (left->kind == T_CONST) {
          sprintf(buf, "MOV `s0[%d] , `d0\n", left->u.CONST);
          emit(AS_Move(String(buf), singleTemp(r), singleTemp(munchExp(right))));
        } else if (right->kind == T_CONST) {
          sprintf(buf, "MOV `s0[%d] , `d0\n", right->u.CONST);
          emit(AS_Move(String(buf), singleTemp(r), singleTemp(munchExp(left))));
        }
      }
      /* MEM(CONST(i)) */
      else if (e->u.MEM->kind == T_Const) {
        sprintf(buf, "MOV [`s0] , `d0\n", e->u.MEM->u.CONST);
        emit(AS_Move(String(buf), singleTemp(r), NULL));
      }
      /* MEM(e1) */
      else if (e->u.MEM->kind == T_Exp) {
        sprintf(buf, "MOV M[`s0+0] , `d0\n");
        emit(AS_Move(String(buf), singleTemp(r), singleTemp(munchExp(e->u.MEM))));
      } else {
        assert(0);
      }

      return r;
    }
    case T_BINOP: {
      Temp_temp t1 = Temp_newtemp();
      Temp_temp t2 = Temp_newtemp();
      Temp_temp t3 = Temp_newtemp();

        T_exp op    = e->u.BINOP.op,
              left  = e->u.BINOP.left,
              right = e->u.BINOP.right;

        bool with_immediate = e->u.BINOP.left == T_CONST || e->u.BINOP.right == T_CONST;

        /* BINOP(PLUS,e1,CONST(i)) and
        * BINOP(PLUS,CONST(i),e1) */
        if (with_immediate) {
          T_exp ce = (right->kind == T_CONST)? right : left;
          T_exp ee = (right->kind == T_CONST)? left : right;
          switch (op->kind) {
            case T_plus:
              sprintf(buf, "MOV `s0 , `d0\n");
              emit(AS_Move(String(buf), singleTemp(t1), singleTemp(munchExp(ee))));
              sprintf(buf, "ADD %d , `d0\n", ce->u.CONST);
              emit(AS_Oper(String(buf), singleTemp(t1), NULL, NULL));
              break;
            case T_minus:
              sprintf(buf, "MOV `s0 , `d0\n");
              emit(AS_Move(String(buf), singleTemp(t1), singleTemp(munchExp(ee))));
              sprintf(buf, "SUB %d , `d0\n", ce->u.CONST);
              emit(AS_Oper(String(buf), singleTemp(t1), NULL, NULL));
              break;
            case T_mul:
              sprintf(buf, "MOV `s0 , `d0\n");
              emit(AS_Move(String(buf), singleTemp(t1), singleTemp(munchExp(ee))));
              sprintf(buf, "IMUL %d , `d0\n", ce->u.CONST);
              emit(AS_Oper(String(buf), singleTemp(t1), NULL, NULL));
              break;
            case T_xor:
              sprintf(buf, "MOV `s0 , `d0\n");
              emit(AS_Move(String(buf), singleTemp(t1), singleTemp(munchExp(ee))));
              sprintf(buf, "XOR %d , `d0\n", ce->u.CONST);
              emit(AS_Oper(String(buf), singleTemp(t1), NULL, NULL));
              break;
            case T_or:
              sprintf(buf, "MOV `s0 , `d0\n");
              emit(AS_Move(String(buf), singleTemp(t1), singleTemp(munchExp(ee))));
              sprintf(buf, "OR %d , `d0\n", ce->u.CONST);
              emit(AS_Oper(String(buf), singleTemp(t1), NULL, NULL));
              break;
            case T_and:
              sprintf(buf, "MOV `s0 , `d0\n");
              emit(AS_Move(String(buf), singleTemp(t1), singleTemp(munchExp(ee))));
              sprintf(buf, "AND %d , `d0\n", ce->u.CONST);
              emit(AS_Oper(String(buf), singleTemp(t1), NULL, NULL));
              break;
            case T_lshift:
              sprintf(buf, "MOV `s0 , `d0\n");
              emit(AS_Move(String(buf), singleTemp(t1), singleTemp(munchExp(ee))));
              sprintf(buf, "SHL %d , `d0\n", ce->u.CONST);
              emit(AS_Oper(String(buf), singleTemp(t1), NULL, NULL));
              break;
            case T_rshift:
              sprintf(buf, "MOV `s0 , `d0\n");
              emit(AS_Move(String(buf), singleTemp(t1), singleTemp(munchExp(ee))));
              sprintf(buf, "SHR %d , `d0\n", ce->u.CONST);
              emit(AS_Oper(String(buf), singleTemp(t1), NULL, NULL));
              break;
            case T_arshift:
              sprintf(buf, "MOV `s0 , `d0\n");
              emit(AS_Move(String(buf), singleTemp(t1), singleTemp(munchExp(ee))));
              sprintf(buf, "ASR %d , `d0\n", ce->u.CONST);
              emit(AS_Oper(String(buf), singleTemp(t1), NULL, NULL));
              break;
            case T_div:
              /* idivq S:
              *   Signed divide %rdx:%rax by S
              *   Quotient stored in %rax
              *   Remainder stored in %rdx, not used in Tiger */
              sprintf(buf, "MOV `s0 , `d0\n");
              emit(AS_Move(String(buf), singleTemp(F_RV()), singleTemp(munchExp(ee)))); // F_RV() is %rax
              sprintf(buf, "IDIVQ %d\n", ce->u.CONST);
              emit(AS_Oper(String(buf), NULL, NULL, NULL));
              sprintf(buf, "MOV `s0 , `d0\n");
              emit(AS_Move(String(buf), singleTemp(t1), singleTemp(F_RV())));
              break;

              default:
                assert(0);
            }
          return t1;
        }

        /* BINOP(PLUS,e1,e2) */
        switch (op->kind) {
          case T_plus:
            sprintf(buf, "MOV `s0 , `d0\n");
            emit(AS_Move(String(buf), singleTemp(t1), singleTemp(munchExp(left))));
            sprintf(buf, "ADD `s0 , `d0\n");
            emit(AS_Oper(String(buf), singleTemp(t1), singleTemp(munchExp(right)), NULL));
            break;
          case T_minus:
            sprintf(buf, "MOV `s0 , `d0\n");
            emit(AS_Move(String(buf), singleTemp(t1), singleTemp(munchExp(left))));
            sprintf(buf, "SUB `s0 , `d0\n");
            emit(AS_Oper(String(buf), singleTemp(t1), singleTemp(munchExp(right)), NULL));
            break;
          case T_mul:
            sprintf(buf, "MOV `s0 , `d0\n");
            emit(AS_Move(String(buf), singleTemp(t1), singleTemp(munchExp(left))));
            sprintf(buf, "IMUL `s0 , `d0\n");
            emit(AS_Oper(String(buf), singleTemp(t1), singleTemp(munchExp(right)), NULL));
            break;
          case T_xor:
            sprintf(buf, "MOV `s0 , `d0\n");
            emit(AS_Move(String(buf), singleTemp(t1), singleTemp(munchExp(left))));
            sprintf(buf, "XOR `s0 , `d0\n");
            emit(AS_Oper(String(buf), singleTemp(t1), singleTemp(munchExp(right)), NULL));
            break;
          case T_or:
            sprintf(buf, "MOV `s0 , `d0\n");
            emit(AS_Move(String(buf), singleTemp(t1), singleTemp(munchExp(left))));
            sprintf(buf, "OR `s0 , `d0\n");
            emit(AS_Oper(String(buf), singleTemp(t1), singleTemp(munchExp(right)), NULL));
            break;
          case T_and:
            sprintf(buf, "MOV `s0 , `d0\n");
            emit(AS_Move(String(buf), singleTemp(t1), singleTemp(munchExp(left))));
            sprintf(buf, "AND `s0 , `d0\n");
            emit(AS_Oper(String(buf), singleTemp(t1), singleTemp(munchExp(right)), NULL));
            break;
          case T_lshift:
            sprintf(buf, "MOV `s0 , `d0\n");
            emit(AS_Move(String(buf), singleTemp(t1), singleTemp(munchExp(left))));
            sprintf(buf, "SHL `s0 , `d0\n");
            emit(AS_Oper(String(buf), singleTemp(t1), singleTemp(munchExp(right)), NULL));
            break;
          case T_rshift:
            sprintf(buf, "MOV `s0 , `d0\n");
            emit(AS_Move(String(buf), singleTemp(t1), singleTemp(munchExp(left))));
            sprintf(buf, "SHR `s0 , `d0\n");
            emit(AS_Oper(String(buf), singleTemp(t1), singleTemp(munchExp(right)), NULL));
            break;
          case T_arshift:
            sprintf(buf, "MOV `s0 , `d0\n");
            emit(AS_Move(String(buf), singleTemp(t1), singleTemp(munchExp(left))));
            sprintf(buf, "ASR `s0 , `d0\n");
            emit(AS_Oper(String(buf), singleTemp(t1), singleTemp(munchExp(right)), NULL));
            break;
          case T_div:
            sprintf(buf, "MOV `s0 , %%rax\n");
            emit(AS_Move(String(buf), NULL, singleTemp(munchExp(left))));
            sprintf(buf, "IDIVQ `s0\n");
            emit(AS_Oper(String(buf), NULL, singleTemp(munchExp(right)), NULL));
            sprintf(buf, "MOV `%%rax , `d0\n");
            emit(AS_Move(String(buf), singleTemp(t1), NULL));
            break;
          default:
              assert(0);
          }
        return t1;
    }
    case T_TEMP: {
      return e->u.TEMP;
    }
    case T_CONST: {
      Temp_temp r = Temp_newtemp();
      sprintf(buf, "MOV %d `d0\n", e->u.CONST);
      emit(AS_Move(String(buf), singleTemp(r), NULL));
      return r;
    }
    case T_NAME: {
      Temp_temp r = Temp_newtemp();
      sprintf(buf, "mov %s , `d0\n", Temp_labelstring(e->u.NAME));
      emit(AS_Label(String(buf), singleTemp(r)));
      return r;
    }
    case T_ESEQ: {
      T_stm stm = e->u.ESEQ.stm;
      T_exp exp = e->u.ESEQ.exp;
      munchStm(exp);
      return munchExp(exp);
    }
    case T_CALL: {
      Temp_temp r = munchExp(e->u.CALL.fun);
      Temp_tempList l = munchArgs(0, e->u.CALL.args);
      Temp_tempList calldefs = Temp_TempList(F_RV(), F_CALLEE());
      sprintf(buf, "call %s\n", Temp_labelstring((e->u.CALL.fun->u.NAME)));
      emit(AS_Oper(String(buf), calldefs, Temp_TempList(r, l), NULL));
      return F_RV();
    }
  }
}

static void munchStm(T_stm s) {
  char buf[100];
  switch (s->kind) { //T_SEQ, T_LABEL, T_JUMP, T_CJUMP, T_MOVE, T_EXP
    case T_SEQ: {
      munchStm(s->u.SEQ.left);
      munchStm(s->u.SEQ.right);
      return;
    }
    case T_LABEL: {
      sprintf(buf, "%s:\n", s->u.LABEL);
      emit(AS_LABEL(String(buf), s->u.LABEL));
      return;
    }
    case T_JUMP: {
      Temp_temp t = munchExp(s->u.JUMP.exp);
      sprintf(buf, "JMP `d0");
      emit(AS_Oper(String(buf), singleTemp(t), s->u.JUMP.jumps));
      return;
    }
    case T_CJUMP: {
      T_relOp op = s->u.CJUMP.op;
      Temp_temp left = numchExp(s->u.CJUMP.left),
                right = numchExp(s->u.CJUMP.right);
      Temp_label true = s->u.CJUMP.true,
                 false = s->u.CJUMP.false;
      
      switch (op) {
        case T_eq:{
          sprintf(buf, "CMP `s0 , `d0\n");
          emit(AS_Oper(String(buf), singleTemp(left), singleTemp(right), NULL));
          sprintf(buf, "JE `d0\n");
          emit(AS_Oper(String(buf), singleTemp(numchExp(true)), NULL, AS_Targets(Temp_TempList(true, Temp_TempList(false, NULL)))));
          break;
        }
        case T_ne:{
          sprintf(buf, "CMP `s0 , `d0\n");
          emit(AS_Oper(String(buf), singleTemp(left), singleTemp(right), NULL));
          sprintf(buf, "JZ `d0\n");
          emit(AS_Oper(String(buf), singleTemp(numchExp(true)), NULL, AS_Targets(Temp_TempList(true, Temp_TempList(false, NULL)))));
          break;
        }
        case T_lt:{
          sprintf(buf, "CMP `s0 , `d0\n");
          emit(AS_Oper(String(buf), singleTemp(left), singleTemp(right), NULL));
          sprintf(buf, "JL `d0\n");
          emit(AS_Oper(String(buf), singleTemp(numchExp(true)), NULL, AS_Targets(Temp_TempList(true, Temp_TempList(false, NULL)))));
          break;
        }
        case T_le:{
          sprintf(buf, "CMP `s0 , `d0\n");
          emit(AS_Oper(String(buf), singleTemp(left), singleTemp(right), NULL));
          sprintf(buf, "JLE `d0\n");
          emit(AS_Oper(String(buf), singleTemp(numchExp(true)), NULL, AS_Targets(Temp_TempList(true, Temp_TempList(false, NULL)))));
          break;
        }
        case T_gt:{
          sprintf(buf, "CMP `s0 , `d0\n");
          emit(AS_Oper(String(buf), singleTemp(left), singleTemp(right), NULL));
          sprintf(buf, "JG `d0\n");
          emit(AS_Oper(String(buf), singleTemp(numchExp(true)), NULL, AS_Targets(Temp_TempList(true, Temp_TempList(false, NULL)))));
          break;
        }
        case T_ge:{
          sprintf(buf, "CMP `s0 , `d0\n");
          emit(AS_Oper(String(buf), singleTemp(left), singleTemp(right), NULL));
          sprintf(buf, "JGE `d0\n");
          emit(AS_Oper(String(buf), singleTemp(numchExp(true)), NULL, AS_Targets(Temp_TempList(true, Temp_TempList(false, NULL)))));
          break;
        }
        case T_ult:{
          sprintf(buf, "CMP `s0 , `d0\n");
          emit(AS_Oper(String(buf), singleTemp(left), singleTemp(right), NULL));
          sprintf(buf, "JNL `d0\n");
          emit(AS_Oper(String(buf), singleTemp(numchExp(true)), NULL, AS_Targets(Temp_TempList(true, Temp_TempList(false, NULL)))));
          break;
        }
        case T_ule:{
          sprintf(buf, "CMP `s0 , `d0\n");
          emit(AS_Oper(String(buf), singleTemp(left), singleTemp(right), NULL));
          sprintf(buf, "JNLE `d0\n");
          emit(AS_Oper(String(buf), singleTemp(numchExp(true)), NULL, AS_Targets(Temp_TempList(true, Temp_TempList(false, NULL)))));
          break;
        }
        case T_ugt:{
          sprintf(buf, "CMP `s0 , `d0\n");
          emit(AS_Oper(String(buf), singleTemp(left), singleTemp(right), NULL));
          sprintf(buf, "JNG `d0\n");
          emit(AS_Oper(String(buf), singleTemp(numchExp(true)), NULL, AS_Targets(Temp_TempList(true, Temp_TempList(false, NULL)))));
          break;
        }
        case T_uge:{
          sprintf(buf, "CMP `s0 , `d0\n");
          emit(AS_Oper(String(buf), singleTemp(left), singleTemp(right), NULL));
          sprintf(buf, "JNGE `d0\n");
          emit(AS_Oper(String(buf), singleTemp(numchExp(true)), NULL, AS_Targets(Temp_TempList(true, Temp_TempList(false, NULL)))));
          break;
        }
      }
    }
    case T_MOVE: {
      Temp_temp dst = munchExp(s->u.MOVE.dst);
      Temp_temp src = munchExp(s->u.MOVE.src);
      sprintf(buf, "mov `s0 `d0\n");
      emit(AS_Move(String(buf), singleTemp(dst), singleTemp(src)));
    }
    case T_EXP: {
      munchExp(s->u.EXP);
    }
  }
}

static Temp_tempList munchArgs(int n, T_expList args) {
  // move or push call arguments, in reverse
  Temp_tempList p = NULL;

  // in register: %rdi，%rsi，%rdx，%rcx，%r8，%r9
  for (Temp_tempList regs = F_ARGS();
       args && regs;
       args=args->tail, regs=regs->tail) {
    Temp_temp e = munchExp(args->head);

    emit(AS_Move(String("MOV `s0 , `d0"), singleTemp(regs->head), singleTemp(e)));

    p = Temp_TempList(e, p);
  }
  
  if (n <= 6) return p;

  // in frame: (%rbp), 8(%rbp), ...
  for (int i = 0; args->head; args=args->tail) {
    Temp_temp e = munchExp(args->head);

    emit(AS_Oper(String("PUSHL `d0"), singleTemp(F_SP()), singleTemp(e), NULL));

    p = Temp_TempList(e, p);
  }

  return p;
}
