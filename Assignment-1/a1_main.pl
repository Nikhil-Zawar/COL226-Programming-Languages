mem(A,[A|_]).
mem(A,[_|Tail]) :- mem(A,Tail).

%---------------  PART - A   ----------------------
mem((X,Y), inv(R)) :- mem((Y,X), R).

rt_mem((X,X),reftransclos(R),S,Visited) :- mem(X,S), !.
rt_mem((X,Y),reftransclos(R),S,Visited) :- mem((X,Y),R), !.
rt_mem((X,Y),reftransclos(R),S,Visited) :- \+mem((X,Y),Visited),mem((X,Z),R),rt_mem((Z,Y),reftransclos(R),S,[(X,Y)|Visited]).

/*
| ?- rt_mem((4,5),reftransclos([(4,2),(2,5)]),[1,2,4,5],[]).
true ? 
yes

| ?- rt_mem((2,2),reftransclos([(2,3),(2,4),(4,6),(6,3),(3,5),(6,5)]),[1,2,3,4,5],[]).
yes

| ?- rt_mem((3,3),reftransclos([(4,6),(4,6),(6,8),(8,5),(5,7),(8,7)]),[4,5,6,7,8],[]).
no

| ?- rt_mem((7,9),reftransclos([(6,7),(7,8)]),[6,7,8],[]).
no

| ?- rt_mem((6,9),reftransclos([(7,8),(7,6),(8,8),(8,7),(6,7),(6,8),(7,9),(8,9),(9,aa),(9,ab)]),[7,8,6,9,aa,ab],[]).
true ? 
yes
*/

rst_mem((X,Y),equivalence(R),S,Visited) :- mem((X,Y),R),!.
rst_mem((X,Y),equivalence(R),S,Visited) :- mem((X,Y),inv(R)),!.
rst_mem((X,X),equivalence(R),S,Visited) :- mem(X,S),!.
rst_mem((X,Y),equivalence(R),S,Visited) :- \+mem((X,Y),Visited),mem((X,Z),R),rst_mem((Z,Y),equivalence(R),S,[(X,Y)|Visited]).

/*
| ?- rst_mem((1,13),equivalence([(1,2),(2,13)]),[1,2,1,13],[]).
true ? 
yes

| ?- rst_mem((2,2),equivalence([(2,3),(2,1),(1,17),(17,3),(3,13),(17,13)]),[1,2,3,1,13],[]).
yes

| ?- rst_mem((7,9),equivalence([(17,7),(7,14)]),[17,7,14],[]).
no

| ?- rst_mem((17,9),equivalence([(7,14),(7,17),(14,14),(14,7),(17,7),(17,14),(7,9),(14,9),(9,aa),(9,ab)]),[7,14,17,9,aa,ab],[]).
true ? 
yes
*/

%---------------  PART - B   --------------------
del(X, [ ] , [ ]).
del(X, [X|R], Z) :- del(X, R, Z).
del(X, [Y|R], [Y|Z]) :- del(X, R, Z).

remdups([ ], [ ]).
remdups([X|R], [X|Z]) :- del(X, R, L), remdups(L, Z).

unionI([ ], S2, S2).
unionI(S1, [ ], S1).
unionI([X|R], S2, [X|Z]) :- del(X, S2, S3),  unionI(R, S3, Z).
/*
| ?- unionI([],[],Union).
Union = []
yes

| ?- unionI([],[2,3],Union).
Union = [2,3]
yes

| ?- unionI([3,4,1],[],Union).
Union = [3,4,1]
yes

| ?- unionI([3,4,1,14,18,6],[2,6,5,12,14],Union).
Union = [3,4,1,14,18,6,2,5,12]
yes

| ?- unionI(S1,[5,3,1,4,6],[5,3,1,4,6,12,13]).
S1 = [5,3,1,4,6,12,13]
yes

| ?- unionI(S1,[13,12,6],[5,3,1,4,6,12,13]).
S1 = [5,3,1,4,6,12] --------multiple possible S1 are there
yes
*/

interI([],Z,[]).
interI([X|Y],Z,[X|W]) :- mem(X,Z), interI(Y,Z,W),!.
interI([X|Y],Z,W) :- \+ mem(X,Z), interI(Y,Z,W).

/*
| ?- interI([],[],Intersection).
Intersection = []
yes

| ?- interI([2,3],[],Intersection).
Intersection = []
yes

| ?- interI([],[4,5],Intersection).
Intersection = []
yes

| ?- interI([1,2,3],[6,7,8],Intersection).
Intersection = []
yes

| ?- interI([1,2,3,4],[8,3,4,9],Intersection).
Intersection = [3,4]
yes

| ?- interI([4,5,6,7],[7,5,4,6],Intersection).
Intersection = [4,5,6,7]
yes
*/

diffI([],_,[]):-!.
diffI([X|S1],S2,S3) :- mem(X,S2), diffI(S1,S2,S3),!.
diffI([X|S1], S2, [X|S3]) :- diffI(S1,S2,S3),!.
/*
| ?- diffI([],[],Set_Diff).
Set_Diff = []
yes

| ?- diffI([1,2,3,5],[],Set_Diff).
Set_Diff = [1,2,3,5]
yes

| ?- diffI([],[4,5,6],Set_Diff).
Set_Diff = []
yes

| ?- diffI([1,4,5,6,33,7],[7,4],Set_Diff).
Set_Diff = [1,5,6,33]
yes

| ?- diffI([1,4,6],[1,4,6,9],Set_Diff).
Set_Diff = []
yes
*/


appendL( [ ], L, L).
appendL( [X|R], L, [X|Z]) :- appendL(R, L, Z).

mapcons(X, [ ], [ ]).
mapcons(X, [Y|R], [ [X|Y] | Z ]) :- mapcons(X, R, Z).

powerI([ ], [ [ ] ]).
powerI([X|R], P) :- powerI(R, P1),  mapcons(X, P1, P2), append(P2, P1, P).
/*
| ?- powerI([],PowerSet).
PowerSet = [[]]
yes

| ?- powerI([0],PowerSet).
PowerSet = [[0],[]]
yes

| ?- powerI([0,1,5,12],PowerSet).
PowerSet = [[0,1,5,12],[0,1,5],[0,1,12],[0,1],[0,5,12],[0,5],[0,12],[0],[1,5,12],[1,5],[1,12],[1],[5,12],[5],[12],[]]
yes

| ?- powerI([0,1,[2,3]],PowerSet).
PowerSet = [[0,1,[2,3]],[0,1],[0,[2,3]],[0],[1,[2,3]],[1],[[2,3]],[]]
yes

| ?- powerI([0,1,[a,3]],PowerSet).
PowerSet = [[0,1,[a,3]],[0,1],[0,[a,3]],[0],[1,[a,3]],[1],[[a,3]],[]]
yes

| ?- powerI([0,1,[]],PowerSet).
PowerSet = [[0,1,[]],[0,1],[0,[]],[0],[1,[]],[1],[[]],[]]
yes
*/


cartesianI([],_,[]):-!.
cartesianI([X|S1],S2,S3) :- mapcons(X,S2,Temp1),cartesianI(S1,S2,Temp2),appendL(Temp1, Temp2, S3),!.
/*
| ?- cartesianI([],[],Cartesian_Product).
Cartesian_Product = []
yes

| ?- cartesianI([2,3,5],[],Cartesian_Product).
Cartesian_Product = []
yes

| ?- cartesianI([],[1,7,32],Cartesian_Product).
Cartesian_Product = []
yes

| ?- cartesianI([a,b,dcg],[42,69,71,82],Cartesian_Product).
Cartesian_Product = [[a|42],[a|69],[a|71],[a|82],[b|42],[b|69],[b|71],[b|82],[dcg|42],[dcg|69],[dcg|71],[dcg|82]]
yes

| ?- cartesianI([1,2,4],[1,2,7],Cartesian_Product).
Cartesian_Product = [[1|1],[1|2],[1|7],[2|1],[2|2],[2|7],[4|1],[4|2],[4|7]]
yes
*/

/* 
AIM : two different valid representations of powerset of sets (elements given in different order) are equal.

For this we will use the inbuilt sort function in prolog to sort the sets
*/
powerset([], [[]]).
powerset([X|S], P) :-powerset(S, Temp), append(Temp, [X|Temp], P).
sort_set(S, Sorted_set) :-powerset(S, P), sort(P, Sorted_set).

equal_powersets(S1, S2) :- sort_set(S1, Sorted_powerset1), sort_set(S2, Sorted_powerset2), Sorted_powerset1 == Sorted_powerset2.