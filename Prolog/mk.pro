
% N = Tamano del tablero, K = cantidad de caballos, L = lista de los caballos


same(k(X,Y),k(X1,Y1)) :-
  X =:= X1,
  Y =:= Y1.

%attacks(_,[]) :- fail.
attacks(k(X,Y), [k(X1, Y1)| Otros]) :-
  X =:= X1+1,
  Y =:= Y1+2,
  attacks(k(X,Y), Otros).
attacks(k(X,Y), [k(X1, Y1)| Otros]) :-
  X =:= X1+1,
  Y =:= Y1-2,
  attacks(k(X,Y), Otros).
attacks(k(X,Y), [k(X1, Y1)| Otros]) :-
  X =:= X1+2,
  Y =:= Y1+1,
  attacks(k(X,Y), Otros).
attacks(k(X,Y), [k(X1, Y1)| Otros]) :-
  X =:= X1+2,
  Y =:= Y1-1,
  attacks(k(X,Y), Otros).
attacks(k(X,Y), [k(X1, Y1)| Otros]) :-
  X =:= X1-2,
  Y =:= Y1+1,
  attacks(k(X,Y), Otros).
attacks(k(X,Y), [k(X1, Y1)| Otros]) :-
  X =:= X1-2,
  Y =:= Y1-1,
  attacks(k(X,Y), Otros).
attacks(k(X,Y), [k(X1, Y1)| Otros]) :-
  X =:= X1-1,
  Y =:= Y1-2,
  attacks(k(X,Y), Otros).
attacks(k(X,Y), [k(X1, Y1)| Otros]) :-
  X =:= X1-1,
  Y =:= Y1-2,
  attacks(k(X,Y), Otros).

noattack(_,[]).
noattack(k(X,Y), [k(X1,Y1) | Otros] ) :-
  \+ same(k(X,Y), k(X1,Y1)),
  \+ attacks(k(X,Y), [k(X1,Y1) | Otros]),
  noattack( k(X,Y), Otros ).

gen(N,L) :- gen(N,[],L).
gen(0,A,A).
gen(N,A,R) :- N > 0, N1 is N-1, gen(N1,[N|A],R).


knights([], N).
knights([k(X,Y)|Otros], N) :-
  gen(N,Posibles),
  %write(Posibles),
  member(X,Posibles),
  member(Y,Posibles),
  noattack(k(X,Y), Otros),
  knights(Otros,N).
