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
  // construct live map: remembers what is live at the exit of each flow-graph node
  G_table in = G_empty(), out = G_empty();
  {
    G_table in1 = G_empty(), out1 = G_empty();
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

  // construct interference graph
  struct Live_graph lg;
  {
    G_graph graph = G_Graph();
    Live_moveList moves = NULL;
    TAB_table table = TAB_empty();
    
    for (G_nodeList nodes = G_nodes(flow); nodes; nodes = nodes->tail) {
      G_node n = nodes->head;
      Temp_tempList outs = lookupLiveMap(out, n);
      if (!FG_isMove(n)){ /* addEdge(d for d in def[n], t for t in out[n]) */
        Temp_tempList defs = FG_def(n);

        for (Temp_tempList ds = defs; ds; ds = ds->tail) {
          Temp_temp d = ds->head;
          for (Temp_tempList ts = outs; ts; ts = ts->tail) {
            Temp_temp t = ts->head;
            // find t in interference graph
            G_node t_node = TAB_look(table, t);
            if (!t_node) {
              t_node = G_Node(graph, t);
              TAB_enter(table, t, t_node);
            }
            // d
            G_node d_node = G_Node(graph, d);
            TAB_enter(table, d, d_node);
            
            G_addEdge(d_node, t_node);
          }
        }
      } else { /* for move a<-c, if b != c, addEdge(a, b for b in out[n]) */
        Temp_temp a = FG_def(n)->head;
        Temp_temp c = FG_use(n)->head;
        assert(FG_def(n)->tail == NULL && FG_use(n)->tail == NULL);

        for (Temp_tempList bs = outs; bs; bs = bs->tail) {
          Temp_temp b = bs->head;
          if (b == c) continue;

          // find b in interference graph
          G_node b_node = TAB_look(table, b);
          if (!b_node) {
            b_node = G_Node(graph, b);
            TAB_enter(table, b, b_node);
          }
          G_node c_node = TAB_look(table, c);
          if (!c_node) {
            c_node = G_Node(graph, c);
            TAB_enter(table, c, c_node);
          }
          // a
          G_node a_node = G_Node(graph, a);
          TAB_enter(table, a, a_node);
          
          G_addEdge(a_node, b_node);
          moves = Live_MoveList(c_node, a_node, moves); // ?
        }
      }
    }

    lg.graph = graph;
    lg.moves = moves;
  }

  return lg;
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


