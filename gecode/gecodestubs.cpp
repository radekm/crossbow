/* Copyright (c) 2013 Radek Micek */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/signals.h>
#include <caml/threads.h>

#include <vector>

#include <gecode/int.hh>
#include <gecode/search.hh>

using namespace Gecode;

struct bool_var {
  int id;
};

struct int_var {
  int id;
};

struct bool_var_array {
  int id;
};

struct int_var_array {
  int id;
};

class GecodeForCrossbow : public Space {

private:
  BoolVarArgs boolVars;
  IntVarArgs intVars;

  BoolVarArgs tmpBoolVars;
  IntVarArgs tmpIntVars;

  std::vector<BoolVarArgs> boolVarArrays;
  std::vector<IntVarArgs> intVarArrays;

  BoolVar & boolVar(bool_var v) {
    if (v.id >= 0)
      return boolVars[v.id];
    else
      return tmpBoolVars[-v.id - 1];
  }

  IntVar & intVar(int_var v) {
    if (v.id >= 0)
      return intVars[v.id];
    else
      return tmpIntVars[-v.id - 1];
  }

  BoolVarArray boolValues;
  IntVarArray intValues;

public:
  GecodeForCrossbow() {
  }

  GecodeForCrossbow(bool share, GecodeForCrossbow & g) : Space(share, g) {
    boolValues.update(*this, share, g.boolValues);
    intValues.update(*this, share, g.intValues);
  }

  virtual Space * copy(bool share) {
    return new GecodeForCrossbow(share, *this);
  }

  bool_var newBoolVar() {
    bool_var v;
    v.id = boolVars.size();
    boolVars << BoolVar(*this, 0, 1);
    return v;
  }

  int_var newIntVar(int domSize) {
    int_var v;
    v.id = intVars.size();
    intVars << IntVar(*this, 0, domSize-1);
    return v;
  }

  bool_var newTmpBoolVar() {
    tmpBoolVars << BoolVar(*this, 0, 1);
    bool_var v;
    v.id = -tmpBoolVars.size();
    return v;
  }

  int_var newTmpIntVar(int domSize) {
    tmpIntVars << IntVar(*this, 0, domSize-1);
    int_var v;
    v.id = -tmpIntVars.size();
    return v;
  }

  bool_var_array newBoolVarArray(std::vector<bool_var> & vars) {
    BoolVarArgs arr(vars.size());
    for (unsigned int i = 0; i < vars.size(); i++) {
      arr[i] = boolVar(vars[i]);
    }

    bool_var_array a;
    a.id = boolVarArrays.size();

    boolVarArrays.push_back(arr);

    return a;
  }

  int_var_array newIntVarArray(std::vector<int_var> & vars) {
    IntVarArgs arr(vars.size());
    for (unsigned int i = 0; i < vars.size(); i++) {
      arr[i] = intVar(vars[i]);
    }

    int_var_array a;
    a.id = intVarArrays.size();

    intVarArrays.push_back(arr);

    return a;
  }

  void linear(std::vector<int_var> & vars, std::vector<int> & coefs, int c) {
    assert (vars.size() == coefs.size());

    IntArgs a(coefs.size());
    for (unsigned int i = 0; i < coefs.size(); i++) {
      a[i] = coefs[i];
    }

    IntVarArgs x(vars.size());
    for (unsigned int i = 0; i < vars.size(); i++) {
      x[i] = intVar(vars[i]);
    }

    Gecode::linear(*this, a, x, IRT_EQ, c);
  }

  void boolElement(bool_var_array arr, int_var idx, bool_var y) {
    element(*this, boolVarArrays[arr.id], intVar(idx), boolVar(y));
  }

  void intElement(int_var_array arr, int_var idx, int_var y) {
    element(*this, intVarArrays[arr.id], intVar(idx), intVar(y));
  }

  void eqVarVar(int_var x, int_var x2, bool_var y) {
    rel(*this, intVar(x), IRT_EQ, intVar(x2), boolVar(y));
  }

  void eqVarConst(int_var x, int c, bool_var y) {
    rel(*this, intVar(x), IRT_EQ, c, boolVar(y));
  }

  void clause(std::vector<bool_var> & pos, std::vector<bool_var> & neg) {
    BoolVarArgs p(pos.size());
    for (unsigned int i = 0; i < pos.size(); i++) {
      p[i] = boolVar(pos[i]);
    }

    BoolVarArgs n(neg.size());
    for (unsigned int i = 0; i < neg.size(); i++) {
      n[i] = boolVar(neg[i]);
    }

    Gecode::clause(*this, BOT_OR, p, n, 1);
  }

  void allDifferent(std::vector<int_var> & vars) {
    IntVarArgs x(vars.size());
    for (unsigned int i = 0; i < vars.size(); i++) {
      x[i] = intVar(vars[i]);
    }

    distinct(*this, x);
  }

  /* No variables should be created after this call. */
  void endSpec() {
    boolValues = BoolVarArray(*this, boolVars);
    intValues = IntVarArray(*this, intVars);

    branch(*this, intValues, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    branch(*this, boolValues, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
  }

  int getBoolValue(bool_var v) {
    return boolValues[v.id].val();
  }

  int getIntValue(int_var v) {
    return intValues[v.id].val();
  }
};

#ifdef GECODE_STUBS_LOG
#include <cstdio>
#define log(...) printf(__VA_ARGS__)
#define log_vars(vars) { \
  log("["); \
  for (unsigned int i = 0; i < vars.size(); i++) { \
    log("%d", vars[i].id);	       \
    if (i < vars.size()-1) log(";"); \
  } \
  log("]"); \
}
#define log_coefs(coefs) { \
  log("["); \
  for (unsigned int i = 0; i < coefs.size(); i++) { \
    log("%d", coefs[i]);	       \
    if (i < coefs.size()-1) log(";"); \
  } \
  log("]"); \
}
#else
#define log(...)
#define log_vars(vars)
#define log_coefs(coefs)
#endif

struct Interrupt : public Search::Stop {
  bool _stop;

  Interrupt() {
    _stop = false;
  }

  virtual bool stop(const Search::Statistics &, const Search::Options &) {
    return _stop;
  }
};

struct GecodeSolver {
  int nthreads;
  GecodeForCrossbow * g;
  GecodeForCrossbow * lastSolution;
  DFS<GecodeForCrossbow> * dfs;
  Interrupt * stop;

  GecodeSolver(int nthreads) {
    this->nthreads = nthreads;
    this->g = new GecodeForCrossbow();
    this->lastSolution = 0;
    this->dfs = 0;
    this->stop = 0;
  }

  ~GecodeSolver() {
    if (g) {
      delete g;
      g = 0;
    }
    if (lastSolution) {
      delete lastSolution;
      lastSolution = 0;
    }
    if (dfs) {
      delete dfs;
      dfs = 0;
    }
    if (stop) {
      delete stop;
      stop = 0;
    }
  }
};

#define Solver_val(v) (*((GecodeSolver **) Data_custom_val(v)))

static void gecode_finalize (value gv) {
  GecodeSolver * g = Solver_val(gv);

  log("gecode_finalize(%p)\n", (void *)g);

  delete g;
}

static struct custom_operations gecode_ops = {
  (char *)"cz.radekm.crossbow.gecode",
  gecode_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
};

extern "C" {

CAMLprim value gecode_create(value nthreadsv) {
  CAMLparam1 (nthreadsv);
  CAMLlocal1 (gv);

  int nthreads = Int_val(nthreadsv);
  GecodeSolver * g = new GecodeSolver(nthreads);

  gv = caml_alloc_custom(&gecode_ops, sizeof(GecodeSolver *), 0, 1);
  Solver_val(gv) = g;

  log("gecode_create(%d) = %p\n", nthreads, (void *)g);

  CAMLreturn (gv);
}

CAMLprim value gecode_new_bool_var(value gv) {
  CAMLparam1 (gv);

  GecodeSolver * g = Solver_val(gv);
  bool_var var = g->g->newBoolVar();

  CAMLreturn (Val_int(var.id));
}

CAMLprim value gecode_new_int_var(value gv, value dom_sizev) {
  CAMLparam2 (gv, dom_sizev);

  GecodeSolver * g = Solver_val(gv);
  int_var var = g->g->newIntVar(Int_val(dom_sizev));

  CAMLreturn (Val_int(var.id));
}

CAMLprim value gecode_new_tmp_bool_var(value gv) {
  CAMLparam1 (gv);

  GecodeSolver * g = Solver_val(gv);
  bool_var var = g->g->newTmpBoolVar();

  CAMLreturn (Val_int(var.id));
}

CAMLprim value gecode_new_tmp_int_var(value gv, value dom_sizev) {
  CAMLparam2 (gv, dom_sizev);

  GecodeSolver * g = Solver_val(gv);
  int_var var = g->g->newTmpIntVar(Int_val(dom_sizev));

  CAMLreturn (Val_int(var.id));
}

void bool_var_vector_of_value(std::vector<bool_var> & vars, value varsv) {
  for (unsigned int i = 0; i < Wosize_val(varsv); i++) {
    bool_var v;
    v.id = Int_val(Field(varsv, i));
    vars.push_back(v);
  }
}

void int_var_vector_of_value(std::vector<int_var> & vars, value varsv) {
  for (unsigned int i = 0; i < Wosize_val(varsv); i++) {
    int_var v;
    v.id = Int_val(Field(varsv, i));
    vars.push_back(v);
  }
}

CAMLprim value gecode_new_bool_var_array(value gv, value varsv) {
  CAMLparam2 (gv, varsv);

  GecodeSolver * g = Solver_val(gv);

  std::vector<bool_var> vars;
  bool_var_vector_of_value(vars, varsv);

  bool_var_array arr = g->g->newBoolVarArray(vars);

  CAMLreturn (Val_int(arr.id));
}

CAMLprim value gecode_new_int_var_array(value gv, value varsv) {
  CAMLparam2 (gv, varsv);

  GecodeSolver * g = Solver_val(gv);

  std::vector<int_var> vars;
  int_var_vector_of_value(vars, varsv);

  int_var_array arr = g->g->newIntVarArray(vars);

  CAMLreturn (Val_int(arr.id));
}

CAMLprim value gecode_linear(value gv, value varsv, value coefsv, value cv) {
  CAMLparam4 (gv, varsv, coefsv, cv);

  GecodeSolver * g = Solver_val(gv);

  std::vector<int_var> vars;
  int_var_vector_of_value(vars, varsv);

  std::vector<int> coefs;
  for (unsigned int i = 0; i < Wosize_val(coefsv); i++) {
    coefs.push_back(Int_val(Field(coefsv, i)));
  }

  int c = Int_val(cv);

  g->g->linear(vars, coefs, c);

  log("gecode_linear(%p, ", (void *)g);
  log_vars(vars);
  log(", ");
  log_coefs(coefs);
  log(", %d)\n", c);

  CAMLreturn (Val_unit);
}

CAMLprim value gecode_bool_element(value gv, value arrv, value xv, value yv) {
  CAMLparam4 (gv, arrv, xv, yv);

  GecodeSolver * g = Solver_val(gv);

  bool_var_array arr;
  arr.id = Int_val(arrv);

  int_var x;
  x.id = Int_val(xv);

  bool_var y;
  y.id = Int_val(yv);

  g->g->boolElement(arr, x, y);

  log("gecode_bool_element(%p, %d, %d, %d)\n", (void *)g, arr.id, x.id, y.id);

  CAMLreturn (Val_unit);
}

CAMLprim value gecode_int_element(value gv, value arrv, value xv, value yv) {
  CAMLparam4 (gv, arrv, xv, yv);

  GecodeSolver * g = Solver_val(gv);

  int_var_array arr;
  arr.id = Int_val(arrv);

  int_var x;
  x.id = Int_val(xv);

  int_var y;
  y.id = Int_val(yv);

  g->g->intElement(arr, x, y);

  log("gecode_int_element(%p, %d, %d, %d)\n", (void *)g, arr.id, x.id, y.id);

  CAMLreturn (Val_unit);
}

CAMLprim value gecode_eq_var_var(value gv, value xv, value x2v, value yv) {
  CAMLparam4 (gv, xv, x2v, yv);

  GecodeSolver * g = Solver_val(gv);

  int_var x;
  x.id = Int_val(xv);

  int_var x2;
  x2.id = Int_val(x2v);

  bool_var y;
  y.id = Int_val(yv);

  g->g->eqVarVar(x, x2, y);

  log("gecode_eq_var_var(%p, %d, %d, %d)\n", (void *)g, x.id, x2.id, y.id);

  CAMLreturn (Val_unit);
}

CAMLprim value gecode_eq_var_const(value gv, value xv, value cv, value yv) {
  CAMLparam4 (gv, xv, cv, yv);

  GecodeSolver * g = Solver_val(gv);

  int_var x;
  x.id = Int_val(xv);

  int c = Int_val(cv);

  bool_var y;
  y.id = Int_val(yv);

  g->g->eqVarConst(x, c, y);

  log("gecode_eq_var_const(%p, %d, %d, %d)\n", (void *)g, x.id, c, y.id);

  CAMLreturn (Val_unit);
}

CAMLprim value gecode_clause(value gv, value posv, value negv) {
  CAMLparam3 (gv, posv, negv);

  GecodeSolver * g = Solver_val(gv);

  std::vector<bool_var> pos;
  bool_var_vector_of_value(pos, posv);

  std::vector<bool_var> neg;
  bool_var_vector_of_value(neg, negv);

  g->g->clause(pos, neg);

  log("gecode_clause(%p, ", (void *)g);
  log_vars(pos);
  log(", ");
  log_vars(neg);
  log(")\n");

  CAMLreturn (Val_unit);
}

CAMLprim value gecode_all_different(value gv, value varsv) {
  CAMLparam2 (gv, varsv);

  GecodeSolver * g = Solver_val(gv);

  std::vector<int_var> vars;
  int_var_vector_of_value(vars, varsv);

  g->g->allDifferent(vars);

  log("gecode_all_different(%p, ", (void *)g);
  log_vars(vars);
  log(")\n");

  CAMLreturn (Val_unit);
}

CAMLprim value gecode_solve(value gv) {
  CAMLparam1 (gv);

  GecodeSolver * g = Solver_val(gv);

  if (!g->dfs) {
    g->g->endSpec();

    Interrupt * interrupt = new Interrupt();
    g->stop = interrupt;

    Search::Options opts;
    opts.stop = interrupt;

    DFS<GecodeForCrossbow> * dfs = new DFS<GecodeForCrossbow>(g->g, opts);
    g->dfs = dfs;

    delete g->g;
    g->g = 0;
  }

  if (g->lastSolution) {
    delete g->lastSolution;
    g->lastSolution = 0;
  }

  g->stop->_stop = false;

  caml_release_runtime_system();
  g->lastSolution = g->dfs->next();
  caml_acquire_runtime_system();

  int result = 2;
  if (g->lastSolution)
    result = 0;
  else if (!g->dfs->stopped())
    result = 1;

  log("gecode_solve(%p) = %d\n", (void *)g, result);

  CAMLreturn (Val_int(result));
}

CAMLprim value gecode_interrupt(value gv) {
  CAMLparam1 (gv);

  GecodeSolver * g = Solver_val(gv);

  g->stop->_stop = true;

  log("gecode_interrupt(%p)\n", (void *)g);

  CAMLreturn (Val_unit);
}

CAMLprim value gecode_bool_value(value gv, value varv) {
  CAMLparam2 (gv, varv);

  GecodeSolver * g = Solver_val(gv);

  bool_var var;
  var.id = Int_val(varv);

  int result = g->lastSolution->getBoolValue(var);

  log("gecode_bool_value(%p, %d) = %d\n", (void *)g, Int_val(varv), result);

  CAMLreturn (Val_int(result));
}

CAMLprim value gecode_int_value(value gv, value varv) {
  CAMLparam2 (gv, varv);

  GecodeSolver * g = Solver_val(gv);

  int_var var;
  var.id = Int_val(varv);

  int result = g->lastSolution->getIntValue(var);

  log("gecode_int_value(%p, %d) = %d\n", (void *)g, Int_val(varv), result);

  CAMLreturn (Val_int(result));
}

}
