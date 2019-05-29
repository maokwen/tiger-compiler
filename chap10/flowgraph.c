#include <stdio.h>
#include <stdlib.h> /* for atoi */
#include <string.h> /* for strcpy */
#include "util.h"
#include "table.h"
#include "symbol.h"
#include "absyn.h"
#include "temp.h"
#include "tree.h"
#include "assem.h"
#include "frame.h"
#include "errormsg.h"
#include "assem.h"
#include "graph.h"
#include "temp.h"
#include "flowgraph.h"

G_graph FG_AssemFlowGraph(AS_instrList il) {
  G_graph fg = G_Graph();
  G_node root = G_Node(fg, NULL), prev = root;

  TAB_table labelNode = TAB_empty(); // label-node table

  for (; il; il = il->tail) {
    AS_instr ai = il->head;
    G_node node = G_Node(fg, (void*)ai);

    if (ai->kind == I_LABEL) {
      enter(labelNode, (void*)(ai->u.LABEL.label), (void*)(node));
    } else if (ai->kind = I_OPER && ai->u.OPER.jumps) {
      // handle cjump label list
      for (Temp_labelList l = ai->u.OPER.jumps; l != NULL; l = l->tail) {
        G_node n = (G_node)TAB_look(labelNode, (void*)(l->head));
        G_addEdge(node, n);
      }
    }
    G_addEdge(prev, node);
    prev = node;
  }
}

Temp_tempList FG_def(G_node n) {
  AS_instr ai = (AS_instr)G_nodeInfo(n);
  switch (ai->kind) {
    case I_OPER:
      return ai->u.OPER.dst;
    case I_MOVE:
      return ai->u.MOVE.dst;
    case I_LABEL:
      return NULL;
    default:
      assert(0);
  }
}
G_nodeList FG_succ(G_node n) {
  return G_succ(n);
}
Temp_tempList FG_use(G_node n) {
  AS_instr ai = (AS_instr)G_nodeInfo(n);
  switch (ai->kind)
  {
  case I_OPER:
    return ai->u.OPER.src;
  case I_MOVE:
    return ai->u.MOVE.src;
  case I_LABEL:
    return NULL;
  default:
    assert(0);
  }
}
bool FG_isMove(G_node n) {
  AS_instr ai = (AS_instr)G_nodeINfo(n);
  return (ai->kind == I_MOVE);
}
