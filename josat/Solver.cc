/***************************************************************************************[Solver.cc]
Copyright (c) 2003-2006, Niklas Een, Niklas Sorensson
Copyright (c) 2007-2010, Niklas Sorensson
Copyright (c) 2013, Radek Micek

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
associated documentation files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute,
sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or
substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
**************************************************************************************************/

#include <math.h>

#include "josat/mtl/Alg.h"
#include "josat/mtl/Sort.h"
#include "josat/utils/System.h"
#include "josat/core/Solver.h"

using namespace Josat;

//=================================================================================================
// Options:


static const char* _cat = "CORE";

static DoubleOption  opt_var_decay         (_cat, "var-decay",   "The variable activity decay factor",            0.95,     DoubleRange(0, false, 1, false));
static DoubleOption  opt_clause_decay      (_cat, "cla-decay",   "The clause activity decay factor",              0.999,    DoubleRange(0, false, 1, false));
static DoubleOption  opt_random_var_freq   (_cat, "rnd-freq",    "The frequency with which the decision heuristic tries to choose a random variable", 0, DoubleRange(0, true, 1, true));
static DoubleOption  opt_random_seed       (_cat, "rnd-seed",    "Used by the random variable selection",         91648253, DoubleRange(0, false, HUGE_VAL, false));
static IntOption     opt_phase_saving      (_cat, "phase-saving", "Controls the level of phase saving (0=none, 1=limited, 2=full)", 2, IntRange(0, 2));
static BoolOption    opt_rnd_init_act      (_cat, "rnd-init",    "Randomize the initial activity", false);
static BoolOption    opt_luby_restart      (_cat, "luby",        "Use the Luby restart sequence", true);
static IntOption     opt_restart_first     (_cat, "rfirst",      "The base restart interval", 100, IntRange(1, INT32_MAX));
static DoubleOption  opt_restart_inc       (_cat, "rinc",        "Restart interval increase factor", 2, DoubleRange(1, false, HUGE_VAL, false));
static DoubleOption  opt_garbage_frac      (_cat, "gc-frac",     "The fraction of wasted memory allowed before a garbage collection is triggered",  0.20, DoubleRange(0, false, HUGE_VAL, false));
static IntOption     opt_min_learnts_lim   (_cat, "min-learnts", "Minimum learnt clause limit",  0, IntRange(0, INT32_MAX));


//=================================================================================================
// Constructor/Destructor:


Solver::Solver() :

    // Parameters (user settable):
    //
    verbosity        (0)
  , var_decay        (opt_var_decay)
  , clause_decay     (opt_clause_decay)
  , random_var_freq  (opt_random_var_freq)
  , random_seed      (opt_random_seed)
  , luby_restart     (opt_luby_restart)
  , phase_saving     (opt_phase_saving)
  , rnd_pol          (false)
  , rnd_init_act     (opt_rnd_init_act)
  , garbage_frac     (opt_garbage_frac)
  , min_learnts_lim  (opt_min_learnts_lim)
  , restart_first    (opt_restart_first)
  , restart_inc      (opt_restart_inc)

    // Parameters (the rest):
    //
  , learntsize_factor((double)1/(double)3), learntsize_inc(1.1)

    // Parameters (experimental):
    //
  , learntsize_adjust_start_confl (100)
  , learntsize_adjust_inc         (1.5)

    // Statistics: (formerly in 'SolverStats')
    //
  , solves(0), starts(0), decisions(0), rnd_decisions(0), propagations(0), conflicts(0)
  , dec_vars(0), num_clauses(0), num_learnts(0), clauses_literals(0), learnts_literals(0), max_literals(0), tot_literals(0)

  , watches            (WatcherDeleted(ca))
  , order_heap         (VarOrderLt(activity))
  , ok                 (true)
  , cla_inc            (1)
  , var_inc            (1)
  , qhead              (0)
  , simpDB_assigns     (-1)
  , simpDB_props       (0)
  , remove_satisfied   (true)
  , next_var           (0)

    // Resource constraints:
    //
  , conflict_budget    (-1)
  , propagation_budget (-1)
  , asynch_interrupt   (false)
{
    vec<Lit> ps;
    ps.push(lit_Undef);
    ps.push(lit_Undef);
    svc_implicit_clause = ca.alloc(ps, false);
}


Solver::~Solver()
{
}


//=================================================================================================
// Minor methods:


// Creates a new SAT variable in the solver. If 'decision' is cleared, variable will not be
// used as a decision variable (NOTE! This has effects on the meaning of a SATISFIABLE result).
//
Var Solver::newVar(lbool upol, bool dvar, bool valueVar)
{
    Var v = next_var++;

    watches  .init(mkLit(v, false));
    watches  .init(mkLit(v, true ));
    assigns  .insert(v, l_Undef);
    vardata  .insert(v, mkVarData(CRef_Undef, 0));
    activity .insert(v, rnd_init_act ? drand(random_seed) * 0.00001 : 0);
    seen     .insert(v, 0);
    polarity .insert(v, true);
    user_pol .insert(v, upol);
    value_var.insert(v, valueVar);
    decision .reserve(v);
    trail    .capacity(v+1);
    setDecisionVar(v, dvar);

    svc_watches.insert(v, CRef_Undef);

    return v;
}

bool Solver::addSingleValueConstraint(const vec<Var>& vs) {
    assert(decisionLevel() == 0);
    if (!ok) return false;

    add_tmp.clear();
    for (int i = 0; i < vs.size(); i++)
        add_tmp.push(mkLit(vs[i], lsign_Pos));

    vec<Lit>& ps = add_tmp;

    // Remove duplicate literals and false literals.
    // Count true literals, value literals and true value literals.
    sort(ps);
    int ntrue_lits = 0;
    int nval_lits = 0;
    int ntrue_val_lits = 0;
    int i, j;
    Lit p;
    for (i = 0, j = 0, p = lit_Undef; i < ps.size(); i++)
        if (value(ps[i]) != l_False && ps[i] != p) {
            ps[j++] = p = ps[i];

            bool val = value_var[var(ps[i])];
            bool sat = value(ps[i]) == l_True;

            ntrue_lits += sat;
            nval_lits += val;
            ntrue_val_lits += sat && val;
        }
    ps.shrink(i - j);

    // Conflict clause.
    if (ntrue_val_lits > 1 || ps.size() == 0)
        return ok = false;

    // Satisfied clause. We don't need the clause
    // since it is satisfied and no two value literals
    // can be true at the same time.
    if (ntrue_lits >= 1 && nval_lits <= 1)
        return true;

    // Unit clause.
    if (ps.size() == 1) {
        uncheckedEnqueue(ps[0]);
        return ok = (propagate() == CRef_Undef);
    }

    // One value literal is true,
    // other value literals must be false.
    if (ntrue_val_lits == 1) {
        for (i = 0; i < ps.size(); i++)
            if (value(ps[i]) == l_Undef && value_var[var(ps[i])])
                uncheckedEnqueue(~ps[i]);

        return ok = (propagate() == CRef_Undef);
    }

    // Now the clause contains at least two literals.
    // All value literals in the clause are unassigned.
    // Non-value literals can be unassigned or can be assigned true.
    // If some non-value literal is assigned true then the clause
    // contains at least two value literals.

    CRef cr = ca.alloc(ps, false, true);
    clauses.push(cr);
    attachSingleValueConstraint(cr);

    return true;
}

void Solver::attachSingleValueConstraint(CRef cr) {
    attachClause(cr);

    const Clause& c = ca[cr];
    assert(c.single_value_constraint());

    // For each variable watch for true assignment.
    for (int i = 0; i < c.size(); i++) {
        Var v = var(c[i]);

        // Variable is not watched.
        assert(svc_watches[v] == CRef_Undef);

        if (value_var[v])
            svc_watches[v] = cr;
    }
}

void Solver::detachSingleValueConstraint(CRef cr, bool strict) {
    detachClause(cr);

    const Clause& c = ca[cr];
    assert(c.single_value_constraint());

    for (int i = 0; i < c.size(); i++)
        svc_watches[var(c[i])] = CRef_Undef;
}

bool Solver::addClause_(vec<Lit>& ps)
{
    assert(decisionLevel() == 0);
    if (!ok) return false;

    // Check if clause is satisfied and remove false/duplicate literals:
    sort(ps);
    Lit p; int i, j;
    for (i = j = 0, p = lit_Undef; i < ps.size(); i++)
        if (value(ps[i]) == l_True || ps[i] == ~p)
            return true;
        else if (value(ps[i]) != l_False && ps[i] != p)
            ps[j++] = p = ps[i];
    ps.shrink(i - j);

    if (ps.size() == 0)
        return ok = false;
    else if (ps.size() == 1){
        uncheckedEnqueue(ps[0]);
        return ok = (propagate() == CRef_Undef);
    }else{
        CRef cr = ca.alloc(ps, false);
        clauses.push(cr);
        attachClause(cr);
    }

    return true;
}


void Solver::attachClause(CRef cr){
    const Clause& c = ca[cr];
    assert(c.size() > 1);
    assert(
        // c[0] is always assigned l_Undef in ordinary clauses.
        value(c[0]) == l_Undef ||
        // Single value constraints may contain true non-value literals.
        (c.single_value_constraint() &&
            value(c[0]) == l_True &&
            !value_var[var(c[0])]));
    assert(
        // c[1] is always assigned l_Undef in ordinary clauses added by user.
        value(c[1]) == l_Undef ||
        // c[1] is always assigned l_False in ordinary learned clauses.
        value(c[1]) == l_False ||
        // Single value constraints may contain true non-value literals.
        (c.single_value_constraint() &&
            value(c[1]) == l_True &&
            !value_var[var(c[1])]));
    watches[~c[0]].push(Watcher(cr, c[1]));
    watches[~c[1]].push(Watcher(cr, c[0]));
    if (c.learnt()) num_learnts++, learnts_literals += c.size();
    else            num_clauses++, clauses_literals += c.size();
}


void Solver::detachClause(CRef cr, bool strict){
    const Clause& c = ca[cr];
    assert(c.size() > 1);
    
    // Strict or lazy detaching:
    if (strict){
        remove(watches[~c[0]], Watcher(cr, c[1]));
        remove(watches[~c[1]], Watcher(cr, c[0]));
    }else{
        watches.smudge(~c[0]);
        watches.smudge(~c[1]);
    }

    if (c.learnt()) num_learnts--, learnts_literals -= c.size();
    else            num_clauses--, clauses_literals -= c.size();
}


void Solver::removeClause(CRef cr) {
    Clause& c = ca[cr];

    // Locked clause may be removed only when removing clauses satisfied
    // on decision level 0.
    assert(!locked(c) || level(var(c[0])) == 0);

    // Don't leave pointers to free'd memory!
    if (locked(c)) vardata[var(c[0])].reason = CRef_Undef;

    if (c.single_value_constraint()) {

        // Find and remove pointers to implicit clauses.
        for (int i = 0; i < c.size(); i++)
            if (value(c[i]) == l_False && reason(var(c[i])) == cr)
                vardata[var(c[i])].reason = CRef_Undef;

        detachSingleValueConstraint(cr);
    }
    else {
        detachClause(cr);
    }

    c.mark(1);
    ca.free(cr);
}


// Ordinary clause is satisfied iff it contains true literal.
//
// Single value constraint is satisfied iff
// it contains true literal and no two its value literals can be true.
bool Solver::satisfied(const Clause& c) const {
    assert(decisionLevel() == 0);

    // Assumes that propagation was done and no conflict was encountered,
    // otherwise it may mark single value constraint as satisfied even
    // if some implicit "at most one" clause is not satisfied.

    if (c.single_value_constraint()) {
        bool true_lit_found = false;
        int nundef_value_lits = 0;
        for (int i = 0; i < c.size(); i++) {
            bool val = value_var[var(c[i])];
            if (value(c[i]) == l_True)
                if (val) return true;
                else true_lit_found = true;
            else if (value(c[i]) == l_Undef && val)
                nundef_value_lits++;
        }
        // Now the constraint doesn't contain any true value literals.
        return (true_lit_found && nundef_value_lits <= 1);
    }
    else {
        for (int i = 0; i < c.size(); i++)
            if (value(c[i]) == l_True)
                return true;
        return false;
    }
}

// Revert to the state at given level (keeping all assignment at 'level' but not beyond).
//
void Solver::cancelUntil(int level) {
    if (decisionLevel() > level){
        for (int c = trail.size()-1; c >= trail_lim[level]; c--){
            Var      x  = var(trail[c]);
            assigns [x] = l_Undef;
            if (phase_saving > 1 || (phase_saving == 1 && c > trail_lim.last()))
                polarity[x] = sign(trail[c]);
            insertVarOrder(x); }
        qhead = trail_lim[level];
        trail.shrink(trail.size() - trail_lim[level]);
        trail_lim.shrink(trail_lim.size() - level);
    } }


//=================================================================================================
// Major methods:


Lit Solver::pickBranchLit()
{
    Var next = var_Undef;

    // Random decision:
    if (drand(random_seed) < random_var_freq && !order_heap.empty()){
        next = order_heap[irand(random_seed,order_heap.size())];
        if (value(next) == l_Undef && decision[next])
            rnd_decisions++; }

    // Activity based decision:
    while (next == var_Undef || value(next) != l_Undef || !decision[next])
        if (order_heap.empty()){
            next = var_Undef;
            break;
        }else
            next = order_heap.removeMin();

    // Choose polarity based on different polarity modes (global or per-variable):
    if (next == var_Undef)
        return lit_Undef;
    else if (user_pol[next] != l_Undef)
        return mkLit(next, user_pol[next] == l_True);
    else if (rnd_pol)
        return mkLit(next, drand(random_seed) < 0.5);
    else
        return mkLit(next, polarity[next]);
}


/*_________________________________________________________________________________________________
|
|  analyze : (confl : Clause*) (out_learnt : vec<Lit>&) (out_btlevel : int&)  ->  [void]
|  
|  Description:
|    Analyze conflict and produce a reason clause.
|  
|    Pre-conditions:
|      * 'out_learnt' is assumed to be cleared.
|      * Current decision level must be greater than root level.
|  
|    Post-conditions:
|      * 'out_learnt[0]' is the asserting literal at level 'out_btlevel'.
|      * If out_learnt.size() > 1 then 'out_learnt[1]' has the greatest decision level of the 
|        rest of literals. There may be others from the same level though.
|  
|________________________________________________________________________________________________@*/
void Solver::analyze(CRef confl, vec<Lit>& out_learnt, int& out_btlevel)
{
    // Number of literals from current decision level in learnt clause.
    int pathC = 0;
    Lit p     = lit_Undef;

    // Generate conflict clause:
    //
    out_learnt.push();      // (leave room for the asserting literal)
    int index   = trail.size() - 1;

    do{
        assert(confl != CRef_Undef); // (otherwise should be UIP)
        Clause& c = ca[confl];

        if (c.learnt())
            claBumpActivity(c);

        for (int j = (p == lit_Undef) ? 0 : 1; j < c.size(); j++){
            Lit q = c[j];

            // False literals from decision level 0 will never become true
            // so we can skip them.
            //
            // Attention: If the conflict clause contains only literals from
            // decision level 0 then these will be skipped and seen will
            // remain empty. This will cause problems when searching
            // for literal p.
            if (!seen[var(q)] && level(var(q)) > 0){
                varBumpActivity(var(q));

                // To avoid duplicate literals.
                seen[var(q)] = 1;

                if (level(var(q)) >= decisionLevel())
                    // q will be resolved out - we don't even add it.
                    pathC++;
                else
                    out_learnt.push(q);
            }
        }

        // Select literal p which will be resolved out in the next iteration.
        while (!seen[var(trail[index--])]);
        p     = trail[index+1];
        confl = getAntecedent(p);

        seen[var(p)] = 0;
        pathC--;

    }while (pathC > 0);
    out_learnt[0] = ~p;

    // Invariant: seen now contains exactly variables from out_learnt except p.

    // Simplify conflict clause:
    //
    int i, j;
    out_learnt.copyTo(analyze_toclear);

    // Deep conflict clause minimization
    //
    for (i = j = 1; i < out_learnt.size(); i++)
        if (reason(var(out_learnt[i])) == CRef_Undef || !litRedundant(out_learnt[i]))
            out_learnt[j++] = out_learnt[i];

    max_literals += out_learnt.size();
    out_learnt.shrink(i - j);
    tot_literals += out_learnt.size();

    // Find correct backtrack level:
    //
    if (out_learnt.size() == 1)
        out_btlevel = 0;
    else{
        int max_i = 1;
        // Find the first literal assigned at the next-highest level:
        for (int i = 2; i < out_learnt.size(); i++)
            if (level(var(out_learnt[i])) > level(var(out_learnt[max_i])))
                max_i = i;
        // Swap-in this literal at index 1:
        Lit p             = out_learnt[max_i];
        out_learnt[max_i] = out_learnt[1];
        out_learnt[1]     = p;
        out_btlevel       = level(var(p));
    }

    for (int j = 0; j < analyze_toclear.size(); j++) seen[var(analyze_toclear[j])] = 0;    // ('seen[]' is now cleared)
}


// Check if 'p' can be removed from a conflict clause.
bool Solver::litRedundant(Lit p)
{
    enum { seen_undef = 0, seen_source = 1, seen_removable = 2, seen_failed = 3 };
    assert(seen[var(p)] == seen_undef || seen[var(p)] == seen_source);
    assert(reason(var(p)) != CRef_Undef);

    Clause*               c     = &ca[getAntecedent(p)];
    vec<ShrinkStackElem>& stack = analyze_stack;
    stack.clear();

    for (uint32_t i = 1; ; i++){
        if (i < (uint32_t)c->size()){
            // Checking 'p'-parents 'l':
            Lit l = (*c)[i];
            
            // Variable at level 0 or previously removable:
            if (level(var(l)) == 0 || seen[var(l)] == seen_source || seen[var(l)] == seen_removable){
                continue; }
            
            // Check variable can not be removed for some local reason:
            if (reason(var(l)) == CRef_Undef || seen[var(l)] == seen_failed){
                stack.push(ShrinkStackElem(0, p));
                for (int i = 0; i < stack.size(); i++)
                    if (seen[var(stack[i].l)] == seen_undef){
                        seen[var(stack[i].l)] = seen_failed;
                        analyze_toclear.push(stack[i].l);
                    }
                    
                return false;
            }

            // Recursively check 'l':
            stack.push(ShrinkStackElem(i, p));
            i  = 0;
            p  = l;
            c  = &ca[getAntecedent(p)];
        }else{
            // Finished with current element 'p' and reason 'c':
            if (seen[var(p)] == seen_undef){
                seen[var(p)] = seen_removable;
                analyze_toclear.push(p);
            }

            // Terminate with success if stack is empty:
            if (stack.size() == 0) break;
            
            // Continue with top element on stack:
            i  = stack.last().i;
            p  = stack.last().l;
            c  = &ca[getAntecedent(p)];

            stack.pop();
        }
    }

    return true;
}


/*_________________________________________________________________________________________________
|
|  analyzeFinal : (p : Lit)  ->  [void]
|  
|  Description:
|    Specialized analysis procedure to express the final conflict in terms of assumptions.
|    Calculates the (possibly empty) set of assumptions that led to the assignment of 'p', and
|    stores the result in 'out_conflict'.
|________________________________________________________________________________________________@*/
void Solver::analyzeFinal(Lit p, LSet& out_conflict)
{
    out_conflict.clear();
    out_conflict.insert(p);

    if (decisionLevel() == 0)
        return;

    seen[var(p)] = 1;

    for (int i = trail.size()-1; i >= trail_lim[0]; i--){
        Var x = var(trail[i]);
        if (seen[x]){
            // No antecedent and level > 0 imply that x is assumption.
            if (reason(x) == CRef_Undef){
                assert(level(x) > 0);
                out_conflict.insert(~trail[i]);
            }else{
                Clause& c = ca[getAntecedent(trail[i])];
                for (int j = 1; j < c.size(); j++)
                    if (level(var(c[j])) > 0)
                        seen[var(c[j])] = 1;
            }
            seen[x] = 0;
        }
    }

    seen[var(p)] = 0;
}


void Solver::uncheckedEnqueue(Lit p, CRef from, Var reason_svc)
{
    assert(value(p) == l_Undef);
    assigns[var(p)] = lbool(!sign(p));
    vardata[var(p)] = mkVarData(from, decisionLevel(), reason_svc);
    trail.push_(p);
}


/*_________________________________________________________________________________________________
|
|  propagate : [void]  ->  [Clause*]
|  
|  Description:
|    Propagates all enqueued facts. If a conflict arises, the conflicting clause is returned,
|    otherwise CRef_Undef.
|  
|    Post-conditions:
|      * the propagation queue is empty, even if there was a conflict.
|________________________________________________________________________________________________@*/
CRef Solver::propagate()
{
    CRef    confl     = CRef_Undef;
    int     num_props = 0;

    while (qhead < trail.size()){
        Lit            p   = trail[qhead++];     // 'p' is enqueued fact to propagate.
        num_props++;

        // Value literal p is true - make other value literals
        // from the same single value constraint false.
        if (sign(p) == lsign_Pos && svc_watches[var(p)] != CRef_Undef) {
            CRef cr = svc_watches[var(p)];
            const Clause& c = ca[cr];

            // All value literals except p must be false.
            for (int i = 0; i < c.size(); i++) {

                Lit q = c[i];

                if (q == p || value(q) == l_False || !value_var[var(q)])
                    continue;
                else if (value(q) == l_Undef)
                    uncheckedEnqueue(~q, cr, var(p));
                else {
                    assert(value(q) == l_True);
                    confl = svc_implicit_clause;
                    // Save conflict clause.
                    ca[confl][0] = ~p;
                    ca[confl][1] = ~q;
                    // Empty propagation queue.
                    qhead = trail.size();
                    goto AfterPropagation;
                }
            }
        }

        vec<Watcher>&  ws  = watches.lookup(p);
        Watcher        *i, *j, *end;

        for (i = j = (Watcher*)ws, end = i + ws.size();  i != end;){
            // Try to avoid inspecting the clause:
            Lit blocker = i->blocker;
            if (value(blocker) == l_True){
                *j++ = *i++; continue; }

            // Make sure the false literal is data[1]:
            CRef     cr        = i->cref;
            Clause&  c         = ca[cr];
            Lit      false_lit = ~p;
            if (c[0] == false_lit)
                c[0] = c[1], c[1] = false_lit;
            assert(c[1] == false_lit);
            i++;

            // If 0th watch is true, then clause is already satisfied.
            Lit     first = c[0];
            Watcher w     = Watcher(cr, first);
            if (first != blocker && value(first) == l_True){
                *j++ = w; continue; }

            // Look for new watch:
            for (int k = 2; k < c.size(); k++)
                if (value(c[k]) != l_False){
                    c[1] = c[k]; c[k] = false_lit;
                    watches[~c[1]].push(w);
                    goto NextClause; }

            // Did not find watch -- clause is unit under assignment:
            *j++ = w;
            if (value(first) == l_False){
                confl = cr;
                qhead = trail.size();
                // Copy the remaining watches:
                while (i < end)
                    *j++ = *i++;
            }else
                uncheckedEnqueue(first, cr);

        NextClause:;
        }
        ws.shrink(i - j);
    }

AfterPropagation:;

    propagations += num_props;
    simpDB_props -= num_props;

    return confl;
}


/*_________________________________________________________________________________________________
|
|  reduceDB : ()  ->  [void]
|  
|  Description:
|    Remove half of the learnt clauses, minus the clauses locked by the current assignment. Locked
|    clauses are clauses that are reason to some assignment. Binary clauses are never removed.
|________________________________________________________________________________________________@*/
struct reduceDB_lt { 
    ClauseAllocator& ca;
    reduceDB_lt(ClauseAllocator& ca_) : ca(ca_) {}
    bool operator () (CRef x, CRef y) { 
        return ca[x].size() > 2 && (ca[y].size() == 2 || ca[x].activity() < ca[y].activity()); } 
};
void Solver::reduceDB()
{
    int     i, j;
    double  extra_lim = cla_inc / learnts.size();    // Remove any clause below this activity

    sort(learnts, reduceDB_lt(ca));
    // Don't delete binary or locked clauses. From the rest, delete clauses from the first half
    // and clauses with activity smaller than 'extra_lim':
    for (i = j = 0; i < learnts.size(); i++){
        Clause& c = ca[learnts[i]];
        if (c.size() > 2 && !locked(c) && (i < learnts.size() / 2 || c.activity() < extra_lim))
            removeClause(learnts[i]);
        else
            learnts[j++] = learnts[i];
    }
    learnts.shrink(i - j);
    checkGarbage();
}


// For decision level 0.
// Assumes that propagation was done.
void Solver::removeSatisfied(vec<CRef>& cs)
{
#ifdef DEBUG
    checkWatches();
#endif

    int i, j;
    for (i = j = 0; i < cs.size(); i++){
        Clause& c = ca[cs[i]];

        if (satisfied(c))
            removeClause(cs[i]);
        // If the clause is a single value constraint and not satisfied
        // then it is not used as a reason for any variable.
        // The same holds for ordinary clauses.
        else if (c.single_value_constraint()) {
            assert(
                // Unsatisfied single value constraint may contain true
                // non-value literal.
                (value(c[0]) == l_True && !value_var[var(c[0])]) ||
                (value(c[1]) == l_True && !value_var[var(c[1])]) ||
                (value(c[0]) == l_Undef && value(c[1]) == l_Undef));
            for (int k = 0; k < c.size(); k++)
                if (value(c[k]) == l_False) {
                    // Unwatch value literal.
                    svc_watches[var(c[k])] = CRef_Undef;
                    c[k--] = c[c.size()-1];
                    c.pop();
                }
            cs[j++] = cs[i];
        }
        else{
            // Trim clause:
            assert(value(c[0]) == l_Undef && value(c[1]) == l_Undef);
            for (int k = 2; k < c.size(); k++)
                if (value(c[k]) == l_False){
                    c[k--] = c[c.size()-1];
                    c.pop();
                }
            cs[j++] = cs[i];
        }
    }
    cs.shrink(i - j);

#ifdef DEBUG
    checkWatches();
#endif
}

void Solver::removeClausesWithLit(Lit p) {
    removeClausesWithLitHelper(p, clauses);
    removeClausesWithLitHelper(p, learnts);

    checkGarbage();
}

void Solver::removeClausesWithLitHelper(Lit p, vec<CRef> & cs) {
#ifdef DEBUG
    checkWatches();
#endif

    int i, j;
    for (i = j = 0; i < cs.size(); i++){
        Clause & c = ca[cs[i]];

        bool found = false;
        for (int k = 0; k < c.size(); k++)
            if (c[k] == p) {
                found = true;
                break;
            }

        if (found)
            removeClause(cs[i]);
        else
            cs[j++] = cs[i];
    }
    cs.shrink(i - j);

#ifdef DEBUG
    checkWatches();
#endif
}

void Solver::rebuildOrderHeap()
{
    vec<Var> vs;
    for (Var v = 0; v < nVars(); v++)
        if (decision[v] && value(v) == l_Undef)
            vs.push(v);
    order_heap.build(vs);
}


/*_________________________________________________________________________________________________
|
|  simplify : [void]  ->  [bool]
|  
|  Description:
|    Simplify the clause database according to the current top-level assigment. Currently, the only
|    thing done here is the removal of satisfied clauses, but more things can be put here.
|________________________________________________________________________________________________@*/
bool Solver::simplify()
{
    assert(decisionLevel() == 0);

    if (!ok || propagate() != CRef_Undef)
        return ok = false;

    if (nAssigns() == simpDB_assigns || (simpDB_props > 0))
        return true;

    // Remove satisfied clauses:
    removeSatisfied(learnts);
    if (remove_satisfied){       // Can be turned off.
        removeSatisfied(clauses);
    }
    checkGarbage();
    rebuildOrderHeap();

    simpDB_assigns = nAssigns();
    simpDB_props   = clauses_literals + learnts_literals;   // (shouldn't depend on stats really, but it will do for now)

    return true;
}


/*_________________________________________________________________________________________________
|
|  search : (nof_conflicts : int) (params : const SearchParams&)  ->  [lbool]
|  
|  Description:
|    Search for a model the specified number of conflicts. 
|    NOTE! Use negative value for 'nof_conflicts' indicate infinity.
|  
|  Output:
|    'l_True' if a partial assigment that is consistent with respect to the clauseset is found. If
|    all variables are decision variables, this means that the clause set is satisfiable. 'l_False'
|    if the clause set is unsatisfiable. 'l_Undef' if the bound on number of conflicts is reached.
|________________________________________________________________________________________________@*/
lbool Solver::search(int nof_conflicts)
{
    assert(ok);
    int         backtrack_level;
    int         conflictC = 0;
    vec<Lit>    learnt_clause;
    starts++;

    for (;;){
        CRef confl = propagate();
        if (confl != CRef_Undef){
            // CONFLICT
            conflicts++; conflictC++;
            if (decisionLevel() == 0) return l_False;

            learnt_clause.clear();
            analyze(confl, learnt_clause, backtrack_level);
            cancelUntil(backtrack_level);

            if (learnt_clause.size() == 1){
                uncheckedEnqueue(learnt_clause[0]);
            }else{
                CRef cr = ca.alloc(learnt_clause, true);
                learnts.push(cr);
                attachClause(cr);
                claBumpActivity(ca[cr]);
                uncheckedEnqueue(learnt_clause[0], cr);
            }

            varDecayActivity();
            claDecayActivity();

            if (--learntsize_adjust_cnt == 0){
                learntsize_adjust_confl *= learntsize_adjust_inc;
                learntsize_adjust_cnt    = (int)learntsize_adjust_confl;
                max_learnts             *= learntsize_inc;

                if (verbosity >= 1)
                    printf("| %9d | %7d %8d %8d | %8d %8d %6.0f |\n", 
                           (int)conflicts, 
                           (int)dec_vars - (trail_lim.size() == 0 ? trail.size() : trail_lim[0]), nClauses(), (int)clauses_literals,
                           (int)max_learnts, nLearnts(), (double)learnts_literals/nLearnts());
            }

        }else{
            // NO CONFLICT
            if ((nof_conflicts >= 0 && conflictC >= nof_conflicts) || !withinBudget()){
                // Reached bound on number of conflicts:
                cancelUntil(0);
                return l_Undef; }

            // Simplify the set of problem clauses:
            if (decisionLevel() == 0 && !simplify())
                return l_False;

            if (learnts.size()-nAssigns() >= max_learnts)
                // Reduce the set of learnt clauses:
                reduceDB();

            Lit next = lit_Undef;
            while (decisionLevel() < assumptions.size()){
                // Perform user provided assumption:
                Lit p = assumptions[decisionLevel()];
                if (value(p) == l_True){
                    // Dummy decision level:
                    newDecisionLevel();
                }else if (value(p) == l_False){
                    analyzeFinal(~p, conflict);
                    return l_False;
                }else{
                    next = p;
                    break;
                }
            }

            if (next == lit_Undef){
                // New variable decision:
                decisions++;
                next = pickBranchLit();

                if (next == lit_Undef)
                    // Model found:
                    return l_True;
            }

            // Increase decision level and enqueue 'next'
            newDecisionLevel();
            uncheckedEnqueue(next);
        }
    }
}

/*
  Finite subsequences of the Luby-sequence:

  0: 1
  1: 1 1 2
  2: 1 1 2 1 1 2 4
  3: 1 1 2 1 1 2 4 1 1 2 1 1 2 4 8
  ...


 */

static double luby(double y, int x){

    // Find the finite subsequence that contains index 'x', and the
    // size of that subsequence:
    int size, seq;
    for (size = 1, seq = 0; size < x+1; seq++, size = 2*size+1);

    while (size-1 != x){
        size = (size-1)>>1;
        seq--;
        x = x % size;
    }

    return pow(y, seq);
}

// NOTE: assumptions passed in member-variable 'assumptions'.
lbool Solver::solve_()
{
    model.clear();
    conflict.clear();
    if (!ok) return l_False;

    solves++;

    max_learnts = nClauses() * learntsize_factor;
    if (max_learnts < min_learnts_lim)
        max_learnts = min_learnts_lim;

    learntsize_adjust_confl   = learntsize_adjust_start_confl;
    learntsize_adjust_cnt     = (int)learntsize_adjust_confl;
    lbool   status            = l_Undef;

    if (verbosity >= 1){
        printf("============================[ Search Statistics ]===================\n");
        printf("| Conflicts |          ORIGINAL         |          LEARNT          |\n");
        printf("|           |    Vars  Clauses Literals |    Limit  Clauses Lit/Cl |\n");
        printf("====================================================================\n");
    }

    // Search:
    int curr_restarts = 0;
    while (status == l_Undef){
        double rest_base = luby_restart ? luby(restart_inc, curr_restarts) : pow(restart_inc, curr_restarts);
        status = search(rest_base * restart_first);
        if (!withinBudget()) break;
        curr_restarts++;
    }

    if (verbosity >= 1)
        printf("===============================================================================\n");


    if (status == l_True){
        // Extend & copy model:
        model.growTo(nVars());
        for (int i = 0; i < nVars(); i++) model[i] = value(i);
    }else if (status == l_False && conflict.size() == 0)
        ok = false;

    cancelUntil(0);
    return status;
}

//=================================================================================================
// Statistics:
//

void Solver::printStats() const
{
    double cpu_time = cpuTime();
    double mem_used = memUsedPeak();
    printf("restarts              : %"PRIu64"\n", starts);
    printf("conflicts             : %-12"PRIu64"   (%.0f /sec)\n", conflicts   , conflicts   /cpu_time);
    printf("decisions             : %-12"PRIu64"   (%4.2f %% random) (%.0f /sec)\n", decisions, (float)rnd_decisions*100 / (float)decisions, decisions   /cpu_time);
    printf("propagations          : %-12"PRIu64"   (%.0f /sec)\n", propagations, propagations/cpu_time);
    printf("conflict literals     : %-12"PRIu64"   (%4.2f %% deleted)\n", tot_literals, (max_literals - tot_literals)*100 / (double)max_literals);
    if (mem_used != 0) printf("Memory used           : %.2f MB\n", mem_used);
    printf("CPU time              : %g s\n", cpu_time);
}


//=================================================================================================
// Garbage Collection methods:

void Solver::relocAll(ClauseAllocator& to)
{
    ca.reloc(svc_implicit_clause, to);

    // All watchers:
    //
    watches.cleanAll();
    for (int v = 0; v < nVars(); v++)
        for (int s = 0; s < 2; s++){
            Lit p = mkLit(v, s);
            vec<Watcher>& ws = watches[p];
            for (int j = 0; j < ws.size(); j++)
                ca.reloc(ws[j].cref, to);
        }

    // Single value constraint watchers:
    for (int v = 0; v < nVars(); v++)
        if (svc_watches[v] != CRef_Undef) {
            // Clause which isn't removed is watched
            // and so it must be already relocated.
            assert(ca[svc_watches[v]].reloced());
            ca.reloc(svc_watches[v], to);
        }

    // All reasons:
    //
    for (int i = 0; i < trail.size(); i++){
        Var v = var(trail[i]);

        if (reason(v) != CRef_Undef) {
            // Trail must not link removed reasons.
            assert(!isRemoved(reason(v)));
            // Clause which isn't removed is watched
            // and so it must be already relocated.
            assert(ca[reason(v)].reloced());

            ca.reloc(vardata[v].reason, to);
        }
    }

    // All learnt:
    //
    int i, j;
    for (i = j = 0; i < learnts.size(); i++)
        if (!isRemoved(learnts[i])){
            // Clause which isn't removed is watched
            // and so it must be already relocated.
            assert(ca[learnts[i]].reloced());

            ca.reloc(learnts[i], to);
            learnts[j++] = learnts[i];
        }
    learnts.shrink(i - j);

    // All original:
    //
    for (i = j = 0; i < clauses.size(); i++)
        if (!isRemoved(clauses[i])){
            // Clause which isn't removed is watched
            // and so it must be already relocated.
            assert(ca[clauses[i]].reloced());

            ca.reloc(clauses[i], to);
            clauses[j++] = clauses[i];
        }
    clauses.shrink(i - j);
}


void Solver::garbageCollect()
{
#ifdef DEBUG
    checkWatches();
#endif

    // Initialize the next region to a size corresponding to the estimated utilization degree. This
    // is not precise but should avoid some unnecessary reallocations for the new region:
    ClauseAllocator to(ca.size() - ca.wasted()); 

    relocAll(to);
    if (verbosity >= 2)
        printf("|  Garbage collection:   %12d bytes => %12d bytes             |\n", 
               ca.size()*ClauseAllocator::Unit_Size, to.size()*ClauseAllocator::Unit_Size);
    to.moveTo(ca);

#ifdef DEBUG
    checkWatches();
#endif
}

#ifdef DEBUG

void Solver::checkWatches() {
    // Assert that each watched literal is the first literal
    // or the second literal in the clause.
    for (int i = 0; i < 2 * next_var; i++) {
        Lit p = toLit(i);
        vec<Watcher> & ws = watches[p];

        for (Watcher * w = (Watcher*)ws, * end = w + ws.size(); w != end; w++) {
            CRef cr = w->cref;
            Clause & c = ca[cr];

            assert(c.size() >= 2);
            assert(c.mark() || c[0] == ~p || c[1] == ~p);
        }
    }

    // Assert that each watched value variable really occurs
    // in the single value constraint.
    for (Var v = 0; v < next_var; v++) {
        CRef cr = svc_watches[v];

        if (cr == CRef_Undef)
            continue;

        assert(value_var[v]);

        Clause & c = ca[cr];

        assert(c.single_value_constraint());
        assert(!c.mark());

        bool found = false;
        for (int i = 0; i < c.size(); i++) {
            if (c[i] == mkLit(v, false)) {
                found = true;
                break;
            }
        }

        assert(found);
    }

    // Assert that the first two literals of each clause are watched.
    // Assert that all value literals of each single value constraint
    // are watched.

    for (int i = 0; i < clauses.size(); i++) {
        checkClauseWatches(clauses[i]);
    }

    for (int i = 0; i < learnts.size(); i++) {
        checkClauseWatches(learnts[i]);
    }
}

void Solver::checkClauseWatches(CRef cr) {
    assert(cr != CRef_Undef);

    Clause & c = ca[cr];

    assert(c.size() >= 2);
    assert(!c.mark());

    checkClauseIsWatched(cr, watches[~c[0]]);
    checkClauseIsWatched(cr, watches[~c[1]]);

    if (c.single_value_constraint()) {
        for (int i = 0; i < c.size(); i++) {
            Var v = var(c[i]);

            if (value_var[v])
                assert(svc_watches[v] == cr);
        }
    }
}

void Solver::checkClauseIsWatched(CRef cr, vec<Watcher> & ws) {
    for (Watcher * w = (Watcher*)ws, * end = w + ws.size(); w != end; w++) {
        if (w->cref == cr)
            return;
    }
    assert(false);
}

#endif
