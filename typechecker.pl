lookup(X, [ ], V) :- fail.
lookup( X, [ (X,V) | R ], V) :- !.
lookup(X,[ _ | R], V) :- lookup(X,R, V).

hastype(Gamma, v(X), T) :- lookup(X, Gamma, T).
hastype(Gamma, abs(X, E), arrow(T1, T2)) :-	hastype([(X,T1) | Gamma ], E, T2).
hastype(Gamma, app(E1, E2), T) :- hastype(Gamma, E1, arrow(T1,T)), hastype(Gamma, E2, T1).

hastype(Gamma, plus(E1, E2), t_int) :- hastype(Gamma, E1, t_int), hastype(Gamma, E2, t_int), !.
hastype(Gamma, times(E1, E2), t_int) :- hastype(Gamma, E1, t_int), hastype(Gamma, E2, t_int),!.
hastype(Gamma, neg(E), t_bool) :- hastype(Gamma, E, t_bool),!.
hastype(Gamma,conj(E1,E2),t_bool) :- hastype(Gamma,E1,t_bool),hastype(Gamma,E2,t_bool),!.
hastype(Gamma,disj(E1,E2),t_bool) :- hastype(Gamma,E1,t_bool),hastype(Gamma,E2,t_bool),!.
hastype(Gamma, geq(E1, E2), t_bool) :- hastype(Gamma, E1, t_int), hastype(Gamma, E2, t_int), !.
hastype(Gamma, leq(E1, E2), t_bool) :- hastype(Gamma, E1, t_int), hastype(Gamma, E2, t_int), !.
hastype(Gamma, equals(E1, E2), t_bool) :- hastype(Gamma, E1, t_int), hastype(Gamma, E2, t_int),!.
hastype(Gamma, unit, t_unit).
hastype(Gamma, pair(E1, E2), product(T1, T2)) :- hastype(Gamma, E1, T1), hastype(Gamma, E2, T2),!.
hastype(Gamma,proj1(E),T1) :- hastype(Gamma,E,product(T1,T2)).
hastype(Gamma,proj2(E),T2) :- hastype(Gamma,E,product(T1,T2)).
hastype(Gamma,inl(E),sum(T1,T2)) :- hastype(Gamma,E,T1).
hastype(Gamma,inr(E),sum(T1,T2)) :- hastype(Gamma,E,T2).
hastype(Gamma,case(E0, inl(X), E1, inr(Y), E2),sum(T1,T2)) :- hastype(Gamma,E1,T),hastype(Gamma,E2,T),hastype(Gamma,X,T1),hastype(Gamma,Y,T2).
hastype(Gamma, ifte(E0, E1, E2), T) :- hastype(Gamma, E0, t_bool), hastype(Gamma, E1, T), hastype(Gamma, E2, T).


/* queries */
hastype( [ (x,t_int)], v(x), T).
hastype( [ ], v(x), T).

hastype( [ ], abs(x, v(x)), T).
hastype( [ ] , abs(x, abs(y, v(x))), T).
hastype( [ ], abs(x, abs(y, abs(z, app( app(v(x), v(z)), app(v(y), v(z)) ) ) )), T).

hastype( [ ], app(app(abs(x, abs(y, abs(z, app( app(v(x), v(z)),app(v(y), v(z)) ) ) )), abs(x, abs(y, v(x))) ), abs(x, abs(y, v(x))) ),T).

hastype([(X, t_int),(Y,t_int)], plus(v(X), v(Y)),T).
hastype([], plus(E1, E2),T).
hastype([(X, t_int),(Y,t_int)], times(v(X), v(Y)),T).
hastype(Gamma, times(E1, E2),T).

hastype(Gamma,neg(E),T).
hastype([X,t_bool],neg(v(X)),T).
hastype(Gamma,conj(E1,E2),T).
hastype(Gamma,disj(E1,E2),T).

hastype(Gamma, leq(E1, E2), T).
hastype(Gamma, geq(E1, E2), T).
hastype(Gamma, equals(E1, E2), T).

hastype(Gamma, unit, t_unit).
hastype([], pair(E1,E2), T).
hastype(Gamma,proj1(E),T).
hastype(Gamma,proj2(E),T).
hastype(Gamma,inl(E),T).
hastype(Gamma,inr(E),T).
hastype(Gamma,case(E0, inl(X), E1, inr(Y), E2),sum(T1,T2)).
hastype(Gamma, ifte(E0, E1, E2), T).
