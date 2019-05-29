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
#include "liveness.h"

static void enterLiveMap(G_table t, G_node flowNode, Temp_tempList temps);
static Temp_tempList lookupLiveMap(G_table t, G_node flownode);


struct Live_graph Live_liveness(G_graph flow) {
  G_table in = G_empty(), out = G_empty();

  // construct live map: remembers what is live at the exit of each flow-graph node
  {
    G_table in1 = G_empty(), out1 = G_empty(); // in & out 's backup
    bool stable = FALSE;
    do {
      for (G_nodeList nodes = G_nodes(flow); nodes; nodes = nodes->tail) {
        G_node n = nodes->head;
        enterLiveMap(in1, n, lookupLiveMap(in, n));
        enterLiveMap(out1, n, lookupLiveMap(out, n));
        
        { /* in[n] <- Union(use[n], Minus(out[n], def[n])) */
          Temp_tempList l_in = FG_use(n);
          Temp_tempList last = NULL;
          for (Temp_tempList l = l_in; l; l = l->tail)
            last = l;
          
          Temp_tempList a, b, c;
          TAB_table t = TAB_empty();
          for (Temp_tempList l = FG_def(n); l; l=l->tail) {
            TAB_enter(t, l->head, TRUE);
          }
          for (Temp_tempList l = lookupLiveMap(out, n); l; l = l->tail) {
            if (TAB_look(t, l->head)) {
              if (!last) {
                l_in = last = Temp_TempList(l->head, NULL);
                continue;
              }
              last->tail = Temp_TempList(l->head, NULL);
              last = last->tail;
            }
          }
          free(t);
          enterLiveMap(in, n, l_in);
        }

        { /* out[n] <- Union(in[s for s in succ[n]]) */
          Temp_tempList l_out = NULL;
          Temp_tempList last = l_out;
          for (G_nodeList nodes = FG_succ(n); nodes; nodes = nodes->tail) {
            G_node s = nodes->head;
            for (Temp_tempList l = lookupLiveMap(in, s); l; l = l->tail) {
              if (!last) {
                l_out = last =  Temp_TempList(l->head, NULL);
                continue;
              }
              last->tail =  Temp_TempList(l->head, NULL);
              last = last->tail;
            }
          }
          enterLiveMap(out, n, l_out);
        }
      }
      { /* check if is stable */
        TAB_table t1 = TAB_empty(), t2 = TAB_empty();
        for (G_nodeList nodes = G_nodes(flow); nodes; nodes = nodes->tail) {
          G_node n = nodes->head;
          for (Temp_tempList l = lookupLiveMap(in, n); l; l = l->tail)
            TAB_enter(t1, l->head, TRUE);
          for (Temp_tempList l = lookupLiveMap(in1, n); l; l = l->tail)
            if (!TAB_look(t1, l->head)) {
              stable = FALSE;
              break;
            }
          if (stable) break;

          for (Temp_tempList l = lookupLiveMap(out, n); l; l = l->tail)
            TAB_enter(t2, l->head, TRUE);
          for (Temp_tempList l = lookupLiveMap(out1, n); l; l = l->tail)
            if (!TAB_look(t2, l->head)) {
              stable = FALSE;
              break;
            }
          free(t1), free(t2);
        }
      }
    } while(!stable);

    free(in1), free(out1);
  }

  // construct conflict graph
  {}
}

static void enterLiveMap(G_table t, G_node flowNode,
                         Temp_tempList temps) {
  G_enter(t, flowNode, temps);
}

static Temp_tempList lookupLiveMap(G_table t,
                                   G_node flownode) {
  return (Temp_tempList)G_look(t, flownode);
}

Live_moveList Live_MoveList(G_node src, G_node dst,
                            Live_moveList tail) {
  Live_moveList p = (Live_moveList)checked_malloc(sizeof(p));
  p->src = src; p->dst = dst, p->tail = tail;
  return p;
}

Temp_temp Live_gtemp(G_node n) {
  return (Temp_temp)G_nodeInfo(n);
}


