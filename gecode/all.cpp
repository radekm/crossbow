/* Copyright (c) 2013, 2015 Radek Micek */

#include "gecode/int/arithmetic.cpp"
#include "gecode/int/arithmetic/mult.cpp"
#include "gecode/int/array.cpp"
#include "gecode/int/bin-packing/conflict-graph.cpp"
#include "gecode/int/bin-packing.cpp"
#include "gecode/int/bin-packing/propagate.cpp"
#include "gecode/int/bool.cpp"
#include "gecode/int/bool/eqv.cpp"
#include "gecode/int/branch/activity.cpp"
#include "gecode/int/branch.cpp"
#include "gecode/int/branch/val-sel-commit.cpp"
#include "gecode/int/branch/view-sel.cpp"
#include "gecode/int/branch/view-values.cpp"
#include "gecode/int/channel.cpp"
#include "gecode/int/channel/link-multi.cpp"
#include "gecode/int/channel/link-single.cpp"
#include "gecode/int/circuit.cpp"
#include "gecode/int/count.cpp"
#include "gecode/int/cumulative.cpp"
#include "gecode/int/cumulatives.cpp"
#include "gecode/int/distinct.cpp"
#include "gecode/int/dom.cpp"
#include "gecode/int/element.cpp"
#include "gecode/int/element/pair.cpp"
#include "gecode/int/exec.cpp"
#include "gecode/int/exec/when.cpp"
#include "gecode/int/extensional.cpp"
#include "gecode/int/extensional/dfa.cpp"
#include "gecode/int/extensional/tuple-set.cpp"
#include "gecode/int/gcc.cpp"
#include "gecode/int/int-set.cpp"
#include "gecode/int/ldsb.cpp"
#include "gecode/int/ldsb/sym-imp.cpp"
#include "gecode/int/ldsb/sym-obj.cpp"
#include "gecode/int/linear-bool.cpp"
#include "gecode/int/linear/bool-post.cpp"
#include "gecode/int/linear-int.cpp"
#include "gecode/int/linear/int-post.cpp"
#include "gecode/int/member.cpp"
#include "gecode/int/no-overlap.cpp"
#include "gecode/int/nvalues.cpp"
#include "gecode/int/precede.cpp"
#include "gecode/int/rel.cpp"
#include "gecode/int/sequence.cpp"
#include "gecode/int/sorted.cpp"
#include "gecode/int/unary.cpp"
#include "gecode/int/unshare.cpp"
#include "gecode/int/var/bool.cpp"
#include "gecode/int/var-imp/bool.cpp"
#include "gecode/int/var-imp/int.cpp"
#include "gecode/int/var/int.cpp"
#include "gecode/kernel/activity.cpp"
#include "gecode/kernel/afc.cpp"
#include "gecode/kernel/archive.cpp"
#include "gecode/kernel/branch.cpp"
#include "gecode/kernel/core.cpp"
#include "gecode/kernel/memory-manager.cpp"
#include "gecode/kernel/region.cpp"
#include "gecode/kernel/rnd.cpp"
#include "gecode/search/bab.cpp"
#include "gecode/search/cutoff.cpp"
#include "gecode/search/dfs.cpp"
#include "gecode/search/engine.cpp"
#include "gecode/search/meta/nogoods.cpp"
#include "gecode/search/meta/rbs.cpp"
#include "gecode/search/options.cpp"
#include "gecode/search/parallel/bab.cpp"
#include "gecode/search/parallel/dfs.cpp"
#include "gecode/search/parallel/engine.cpp"
#include "gecode/search/parallel/path.cpp"
#include "gecode/search/sequential/path.cpp"
#include "gecode/search/stop.cpp"
#include "gecode/support/exception.cpp"
#include "gecode/support/heap.cpp"
#include "gecode/support/hw-rnd.cpp"
#include "gecode/support/thread/pthreads.cpp"
#include "gecode/support/thread/thread.cpp"
#include "gecode/support/thread/windows.cpp"
