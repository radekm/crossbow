/* Copyright (c) 2013 Radek Micek */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/signals.h>
#include <caml/threads.h>

#ifdef CMSAT_STUBS_LOG
#include <stdio.h>
#endif

#include "solvertypes.h"
#include "solver.h"

using namespace CMSat;

#ifdef CMSAT_STUBS_LOG
#define log(...) printf(__VA_ARGS__)
#define log_lits(v) { \
  log("["); \
  for (unsigned int i = 0; i < v.size(); i++) { \
    log("%d", v[i].toInt());	       \
    if (i < v.size()-1) log(";"); \
  } \
  log("]"); \
}
#else
#define log(...)
#define log_lits(v)
#endif

#define Solver_val(v) (*((Solver **) Data_custom_val(v)))

static void cmsat_finalize (value sv) {
  Solver * s = Solver_val(sv);

  log("cmsat_finalize(%p)\n", (void *)s);

  delete s;
}

static struct custom_operations cmsat_ops = {
  (char *)"cz.radekm.crossbow.cmsat",
  cmsat_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
};

extern "C" {

CAMLprim value cmsat_create(value unit) {
  CAMLparam1 (unit);
  CAMLlocal1 (sv);

  SolverConf conf;
  conf.doRenumberVars = false;
  conf.doSaveMem = false;
  conf.doBlockClauses = false;
  conf.doVarElim = false;
  Solver * s = new Solver(conf);

  sv = caml_alloc_custom(&cmsat_ops, sizeof(Solver *), 0, 1);
  Solver_val(sv) = s;

  log("cmsat_create() = %p\n", (void *)s);

  CAMLreturn (sv);
}

CAMLprim value cmsat_new_var(value sv) {
  CAMLparam1 (sv);

  Solver * s = Solver_val(sv);
  Var var = s->newVar();

  //log("cmsat_new_var(%p) = %d\n", (void *)s, var);

  CAMLreturn (Val_int(var));
}

CAMLprim value cmsat_add_clause(value sv, value litsv, value lenv) {
  CAMLparam3 (sv, litsv, lenv);

  Solver * s = Solver_val(sv);
  int len = Int_val(lenv);

  // Literals.
  vector<Lit> lits;
  lits.reserve(len);
  for (int i = 0; i < len; i++) {
    lits.push_back(Lit::toLit(Int_val(Field(litsv, i))));
  }

  log("cmsat_add_clause(%p, ", (void *)s);
  log_lits(lits);
  log(", %d) = ", len);

  bool res = s->addClause(lits);

  log("%d\n", (int)res);

  CAMLreturn (Val_bool(res));
}

CAMLprim value cmsat_solve(value sv, value assumptsv) {
  CAMLparam2 (sv, assumptsv);

  Solver * s = Solver_val(sv);
  int len = Wosize_val(assumptsv);

  // Assumptions.
  vector<Lit> assumpts;
  assumpts.reserve(len);
  for (int i = 0; i < len; i++) {
    assumpts.push_back(Lit::toLit(Int_val(Field(assumptsv, i))));
  }

  caml_release_runtime_system();
  lbool lb = s->solve(&assumpts);
  caml_acquire_runtime_system();

  // Convert lbool.
  int res = 2;
  if (lb == l_True) res = 0;
  else if (lb == l_False) res = 1;

  log("cmsat_solve(%p, ", (void *)s);
  log_lits(assumpts);
  log(") = %d\n", res);

  CAMLreturn (Val_int(res));
}

CAMLprim value cmsat_model_value(value sv, value varv) {
  CAMLparam2 (sv, varv);

  Solver * s = Solver_val(sv);
  Var var = Int_val(varv);
  lbool lb = s->modelValue(Lit(var, false));

  // Convert lbool.
  int res = 2;
  if (lb == l_True) res = 0;
  else if (lb == l_False) res = 1;

  //log("cmsat_model_value(%p, %d) = %d\n", (void *)s, var, res);

  CAMLreturn (Val_int(res));
}

CAMLprim value cmsat_interrupt(value sv) {
  CAMLparam1 (sv);

  Solver * s = Solver_val(sv);
  s->setNeedToInterrupt();

  log("cmsat_interrupt(%p)\n", (void *)s);

  CAMLreturn (Val_unit);
}

} // extern "C" {
