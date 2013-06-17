/* Copyright (c) 2013 Radek Micek */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/signals.h>
#include <caml/threads.h>

#ifdef BLS_STUBS_LOG
#include <stdio.h>
#endif

#include "graph.hh"

using namespace bliss;

#ifdef BLS_STUBS_LOG
#define log(...) (printf(__VA_ARGS__), fflush(stdout))
#else
#define log(...)
#endif

#define Graph_val(v) (*((Graph **) Data_custom_val(v)))

static void bls_finalize(value gv) {
  Graph * g = Graph_val(gv);

  log("bls_finalize(%p)\n", (void *)g);

  delete g;
}

static struct custom_operations bliss_ops = {
  (char *)"cz.radekm.crossbow.bliss",
  bls_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

extern "C" {

CAMLprim value bls_create_graph(value unit) {
  CAMLparam1 (unit);
  CAMLlocal1 (gv);

  Graph * g = new Graph();
  // Heuristic shs_fs results in assertion failure.
  g->set_splitting_heuristic(Graph::shs_fsm);

  gv = caml_alloc_custom(&bliss_ops, sizeof(Graph *), 0, 1);
  Graph_val(gv) = g;

  log("bls_create_graph() = %p\n", (void *)g);

  CAMLreturn (gv);
}

CAMLprim value bls_add_vertex(value gv, value colv) {
  CAMLparam2 (gv, colv);

  Graph * g = Graph_val(gv);
  unsigned int col = Int_val(colv);

  unsigned int v = g->add_vertex(col);

  log("bls_add_vertex(%p, %d) = %d\n", (void *)g, col, v);

  CAMLreturn (Val_int(v));
}

CAMLprim value bls_add_edge(value gv, value av, value bv) {
  CAMLparam3 (gv, av, bv);

  Graph * g = Graph_val(gv);
  unsigned int a = Int_val(av);
  unsigned int b = Int_val(bv);

  g->add_edge(a, b);

  log("bls_add_edge(%p, %d, %d)\n", (void *)g, a, b);

  CAMLreturn (Val_unit);
}

CAMLprim value bls_canonical_form(value gv, value nv) {
  CAMLparam2 (gv, nv);
  CAMLlocal1 (labv);

  Graph * g = Graph_val(gv);
  int n = Int_val(nv);

  Stats stats;

  caml_release_runtime_system();
  const unsigned int *labeling = g->canonical_form(stats, 0, 0);
  caml_acquire_runtime_system();

  if (n <= 0) {
    labv = Atom(0);
  }
  else {
    labv = caml_alloc_tuple(n);
    for (int i = 0; i < n; i++) {
      Store_field(labv, i, Val_int(labeling[i]));
    }
  }

  log("bls_canonical_form(%p)\n", (void *)g);

  CAMLreturn (labv);
}

} // extern "C" {
