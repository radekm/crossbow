/* Copyright (c) 2013 Radek Micek */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/signals.h>
#include <caml/threads.h>

#ifdef MINISAT_STUBS_LOG
#include <stdio.h>
#endif

#include "minisat/core/SolverTypes.h"
#include "minisat/core/Solver.h"

using namespace Minisat;

#ifdef MINISAT_STUBS_LOG
#define log(...) printf(__VA_ARGS__)
#define log_lits(v) { \
  log("["); \
  for (int i = 0; i < v.size(); i++) { \
    log("%d", toInt(v[i])); \
    if (i < v.size()-1) log(";"); \
  } \
  log("]"); \
}
#else
#define log(...)
#define log_lits(v)
#endif

#define Solver_val(v) (*((Solver **) Data_custom_val(v)))

static void minisat_finalize (value sv) {
  Solver * s = Solver_val(sv);

  log("minisat_finalize(%p)\n", s);

  delete s;
}

static struct custom_operations minisat_ops = {
  (char *)"cz.radekm.crossbow.minisat",
  minisat_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

extern "C" {

CAMLprim value minisat_create(value unit) {
  CAMLparam1 (unit);
  CAMLlocal1 (sv);

  Solver * s = new Solver();

  sv = caml_alloc_custom(&minisat_ops, sizeof(Solver *), 0, 1);
  Solver_val(sv) = s;

  log("minisat_create() = %p\n", s);

  CAMLreturn (sv);
}

CAMLprim value minisat_new_var(value sv) {
  CAMLparam1 (sv);

  Solver * s = Solver_val(sv);
  Var var = s->newVar();

  //log("minisat_new_var(%p) = %d\n", s, var);

  CAMLreturn (Val_int(var));
}

CAMLprim value minisat_add_clause(value sv, value litsv, value lenv) {
  CAMLparam3 (sv, litsv, lenv);

  Solver * s = Solver_val(sv);
  int len = Int_val(lenv);

  // Literals.
  vec<Lit> lits;
  lits.capacity(len);
  for (int i = 0; i < len; i++) {
    lits.push(toLit(Int_val(Field(litsv, i))));
  }

  log("minisat_add_clause(%p, ", s);
  log_lits(lits);
  log(", %d) = ", len);

  bool res = s->addClause_(lits);

  log("%d\n", (int)res);

  CAMLreturn (Val_bool(res));
}

CAMLprim value minisat_solve(value sv, value assumptsv) {
  CAMLparam2 (sv, assumptsv);

  Solver * s = Solver_val(sv);
  int len = Wosize_val(assumptsv);

  // Assumptions.
  vec<Lit> assumpts;
  assumpts.capacity(len);
  for (int i = 0; i < len; i++) {
    assumpts.push(toLit(Int_val(Field(assumptsv, i))));
  }

  caml_release_runtime_system();
  int res = toInt(s->solveLimited(assumpts));
  caml_acquire_runtime_system();

  // Normalize lbool.
  if (res != 0 && res != 1)
    res = 2;

  log("minisat_solve(%p, ", s);
  log_lits(assumpts);
  log(") = %d\n", res);

  CAMLreturn (Val_int(res));
}

CAMLprim value minisat_model_value(value sv, value varv) {
  CAMLparam2 (sv, varv);

  Solver * s = Solver_val(sv);
  Var var = Int_val(varv);
  int res = toInt(s->modelValue(var));

  // Normalize lbool.
  if (res != 0 && res != 1)
    res = 2;

  //log("minisat_model_value(%p, %d) = %d\n", s, var, res);

  CAMLreturn (Val_int(res));
}

CAMLprim value minisat_interrupt(value sv) {
  CAMLparam1 (sv);

  Solver * s = Solver_val(sv);
  s->interrupt();

  log("minisat_interrupt(%p)\n", s);

  CAMLreturn (Val_unit);
}

CAMLprim value minisat_clear_interrupt(value sv) {
  CAMLparam1 (sv);

  Solver * s = Solver_val(sv);
  s->clearInterrupt();

  log("minisat_clear_interrupt(%p)\n", s);

  CAMLreturn (Val_unit);
}

} // extern "C" {
