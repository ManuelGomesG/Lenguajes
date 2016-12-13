ocho([1/Y1,2/Y2,3/Y3,4/Y4,5/Y5,6/Y6,7/Y7,8/Y8]).

queens([]).
queens([X/Y|Otras]) :-
  queens(Otras),
  member(Y,[1,2,3,4,5,6,7,8]),
  noattack(X/Y,Otras).

noattack(_,[]).
noattack(X/Y, [X1/Y1 | Otras] ) :-
  Y =\= Y1,
  Y1-Y =\= X1-X,
  Y1-Y =\= X-X1,
  noattack( X/Y, Otras ).










% findall(L,queens(L),Soluciones)

horiz(0) :- write('+'), nl.
horiz(N) :- N > 0, write('+-'), N1 is N-1, horiz(N1).

spacer(0) :- write('|').
spacer(N) :- N > 0, write('| '), N1 is N-1, spacer(N1).

draw(L) :- length(L,N), draw(L,N).

draw([],N) :- horiz(N).
draw([_/Y|Otras],N) :-
        horiz(N),
        L is Y-1, R is N-Y,
        spacer(L), write('Q'), spacer(R), nl,
        draw(Otras,N).

template(N,L) :- template(N,[],L).
template(0,L,L).
template(N,A,R) :- N > 0, N1 is N-1, template(N1,[N/_|A],R).

gen(N,L) :- gen(N,[],L).
gen(0,A,A).
gen(N,A,R) :- N > 0, N1 is N-1, gen(N1,[N|A],R).

nqueens(L) :- length(L,N), nqueens(L,N).

nqueens([],N).
nqueens([X/Y|Otras],N) :-
  gen(N,Posibles),
  nqueens(Otras,N),
  member(Y,Posibles),
  noattack(X/Y,Otras).
