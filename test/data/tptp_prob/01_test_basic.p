
cnf(a, axiom, q(X, c)).
cnf(b, axiom, ~q(c, d)).
cnf(c, axiom, X = Y | f(X, Y) != f(Y, X)).
cnf(d, axiom, ~q("hi", 12.5)).
cnf(e, axiom, q(7) | f(U) = "hello world").
