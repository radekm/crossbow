
cnf(a, axiom, g(c) = 0).

% Include without formula selection.
include('02_inc_all.p').

cnf(a, axiom, q(c)).

% Include with formula selection.
% Formula selection is always nonempty.
include('02_inc_sel.p', [foo, bar]).

cnf(a, axiom, q(d)).
