hastype(_,N,intT) :- integer(N), !.
hastype(_,true,boolT).
hastype(_,false,boolT).

hastype(G,E1 + E2,intT) :- hastype(G,E1,intT), hastype(G,E2,intT), !.
hastype(G,E1 * E2,intT) :- hastype(G,E1,intT), hastype(G,E2,intT), !.
hastype(G,E1 - E2,intT) :- hastype(G,E1,intT), hastype(G,E2,intT), !.
hastype(G,E1 / E2,intT) :- hastype(G,E1,intT), hastype(G,E2,intT), !.
hastype(G,E1 ** E2,intT) :- hastype(G,E1,intT), hastype(G,E2,intT), !.
hastype(G,E1 mod E2,intT) :- hastype(G,E1,intT), hastype(G,E2,intT), !.
hastype(G,E1 // E2,intT) :- hastype(G,E1,intT), hastype(G,E2,intT), !.

hastype(G,E1 > E2, boolT) :- hastype(G,E1,intT), hastype(G,E2,intT), !.
hastype(G,E1 < E2, boolT) :- hastype(G,E1,intT), hastype(G,E2,intT), !.
hastype(G,E1 >= E2, boolT) :- hastype(G,E1,intT), hastype(G,E2,intT), !.
hastype(G,E1 =< E2, boolT) :- hastype(G,E1,intT), hastype(G,E2,intT), !.
hastype(G,E1 \== E2, boolT) :- hastype(G,E1,intT), hastype(G,E2,intT), !.
hastype(G,E1 == E2, boolT) :- hastype(G,E1,intT), hastype(G,E2,intT), !.

hastype(G,and(E1,E2),boolT) :- hastype(G,E1,boolT), hastype(G,E2,boolT), !.
hastype(G,or(E1,E2),boolT) :- hastype(G,E1,boolT), hastype(G,E2,boolT), !.
hastype(G,xor(E1,E2),boolT) :- hastype(G,E1,boolT), hastype(G,E2,boolT), !.
hastype(G,xand(E1,E2),boolT) :- hastype(G,E1,boolT), hastype(G,E2,boolT), !.
hastype(G,nor(E1,E2),intT) :- hastype(G,E1,boolT), hastype(G,E2,boolT), !.
hastype(G,nand(E1,E2),intT) :- hastype(G,E1,boolT), hastype(G,E2,boolT), !.
hastype(G,not(E1,E2),intT) :- hastype(G,E1,boolT), hastype(G,E2,boolT), !.

hastype(G,E,T) :- member((E,T),G), !.