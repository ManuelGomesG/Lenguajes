/*
 * bfs - Infraestructura para resolver problemas por búsqueda BFS
 *
 * bfs(Estado,Posibilidades,Movimientos)
 *
 *   -> Estado es una estado posible del problema.
 *   -> Posibilidades es una lista de caminos abiertos, los caminos
 *      se mantienen en orden inverso (estado más reciente de primero).
 *   <- Movimientos son los movimientos que resuelven el problema.
 *
 * Para su funcionamiento es necesario:
 *
 *   - Crear una definición propia para el estado del problema.
 *   - final/1 debe triunfar para los posibles estados finales.
 *   - movimiento/2 debe generar (via backtracking) los movimientos
 *     posibles a partir de un estado particular.
 *   - moverse/3 debe efectuar el cambio de estado según un
 *     movimiento particular, generando el nuevo estado.
 *   - legal/1 determina si un estado es legal o no.
 */

:- discontiguous([inicial/2,final/2,movimiento/3,moverse/4,legal/2]).

bfs(Problema,[[Estado|Estados]|_],Solucion) :-
	final(Problema,Estado),
    reverse([Estado|Estados],Solucion).
bfs(Problema,[Camino|Caminos],Solucion) :-
    extender(Problema,Camino,Nuevos),
    append(Caminos,Nuevos,Posibilidades),
    bfs(Problema,Posibilidades,Solucion).

extender(Problema,[Estado|Camino],Caminos) :-
    findall( [Proximo,Estado|Camino],
           (
             movimiento(Problema,Estado,Movimiento),
  	         moverse(Problema,Estado,Movimiento,Proximo),
  	         legal(Problema,Proximo),
	         \+ member(Proximo,[Estado|Camino])
           ),
           Caminos
         ), !.
extender(_,_,[]).


resolver(Problema,Movimientos) :-
    inicial(Problema,Estado),
    bfs(Problema,[[Estado]],Movimientos).

simular(Problema) :-
    inicial(Problema,Estado),
    bfs(Problema,[[Estado]],Movimientos), !,
    mostrar(Problema,Estado,Movimientos).




/*
 * 	Problema de los n-caballos
 *
 *
 *
 *
 *
 */





inicial(mk,mk(ListaPiezas,Tamano,NumeroPiezas)) :-
	fullTable(ListaPiezas,Tamano),
	length(ListaPiezas,NumeroPiezas).
final(mk,mk(V)) :-	validTable(V).

gen(N,L) :- gen(N,[],L).
gen(0,A,A).
gen(N,A,R) :- N > 0, N1 is N-1, gen(N1,[N|A],R).
fullTable(F,T) :-
	gen(T,Posibilidades),
	findall(k(X,Y),(member(X,Posibilidades),member(Y,Posibilidades)),F).

validTable([_|[]]).
validTable([k(X,Y)|T]) :-
	X1 is X+2,
	Y1 is Y+1,
	\+ member(k(X1,Y1),T),
	X2 is X+2,
	Y2 is Y-1,
	\+ member(k(X2,Y2),T),
	X3 is X-2,
	Y3 is Y+1,
	\+ member(k(X3,Y3),T),
	X4 is X-2,
	Y4 is Y-1,
	\+ member(k(X4,Y4),T),
	X5 is X-1,
	Y5 is Y+2,
	\+ member(k(X5,Y5),T),
	X6 is X-1,
	Y6 is Y-2,
	\+ member(k(X6,Y6),T),
	X7 is X+1,
	Y7 is Y+2,
	\+ member(k(X7,Y7),T),
	X8 is X+1,
	Y8 is Y-2,
	\+ member(k(X8,Y8),T),
	validTable(T).

movimiento(mk,Edo,Victima) :- member(Victima,Edo).
moverse(mk,EdoAct,Victima,ProxEdo) :-
	delete(EdoAct,Victima,ProxEdo).
legal(mk,_).

arre(TamanoTablero, CantidadPiezas, ListaPiezas) :-
	inicial(mk,(mk(ListaPiezas,TamanoTablero,CantidadPiezas))),
	final(mk,mk(ListaPiezas)).

horiz(0) :- write('+'), nl.
horiz(N) :- N > 0, write('+-'), N1 is N-1, horiz(N1).

spacer(0) :- write('|').
spacer(N) :- N > 0, write('| '), N1 is N-1, spacer(N1).

draw(L) :- length(L,N), draw(L,N).

draw([],N) :- horiz(N).
draw([k(_,Y)|Otras],N) :-
        horiz(N),
        L is Y-1, R is N-Y,
        spacer(L), write('K'), spacer(R), nl,
        draw(Otras,N).

caballito(TamanoTablero, ListaPiezas) :- draw(TamanoTablero,ListaPiezas).









/*
 * Problema del lobo, la chivo y el repollo.
 *
 * Representaremos el Estado del problema como
 *
 *    lcr(Bote,OrillaIzquierda,OrillaDerecha)
 *
 * donde Bote indica la posición como izquierda o derecha, mientras que
 * OrillaIzquierda y OrillaDerecha son listas con atomes (lobo, chivo
 * y repollo) que indican la carga que * hay en cada orilla. Ambas
 * listas se mantendrán ordenadas según lobo < chivo < repollo.
 *
 */
inicial(lcr,lcr(izquierda,[lobo,chivo,repollo],[])).

final(lcr,lcr(derecha,[],[lobo,chivo,repollo])).

movimiento(lcr,lcr(izquierda,I,_),Carga) :- member(Carga,I).
movimiento(lcr,lcr(derecha,_,D),Carga)   :- member(Carga,D).
movimiento(lcr,lcr(_,_,_),solo).

moverse(lcr,lcr(B,I,D),Carga,lcr(B1,I1,D1)) :-
   cambios_en_bote(B,B1),
   cambios_en_orillas(Carga,B,I,D,I1,D1).

cambios_en_bote(izquierda,derecha).
cambios_en_bote(derecha,izquierda).

cambios_en_orillas(solo,_,I,D,I,D).
cambios_en_orillas(Carga,izquierda,I,D,I1,D1) :-
    select(Carga,I,I1), insert(Carga,D,D1).
cambios_en_orillas(Carga,derecha,I,D,I1,D1) :-
    select(Carga,D,D1), insert(Carga,I,I1).

insert(X,[Y|Ys],[X,Y|Ys]) :-
    precede(X,Y).
insert(X,[Y|Ys],[Y|Zs]) :-
    precede(Y,X), insert(X,Ys,Zs).
insert(X,[],[X]).

precede(lobo,_).
precede(_,repollo).

legal(lcr,lcr(izquierda,_,D)) :- \+ peligrosa(D).
legal(lcr,lcr(derecha,I,_))   :- \+ peligrosa(I).

peligrosa(Orilla) :- member(lobo,Orilla), member(chivo,Orilla).
peligrosa(Orilla) :- member(chivo,Orilla), member(repollo,Orilla).

mostrar(lcr,_,[])     :- !.
mostrar(lcr,E,[M|Ms]) :-
  mostrar_estado(lcr,E),
  moverse(lcr,E,M,N),
  mostrar_movimiento(lcr,E,M), nl,
  mostrar(lcr,N,Ms), !.

mostrar_estado(lcr,lcr(_,I,D)) :-
  write('Orilla Izquierda: '), write(I), nl,
  write('Orilla Derecha:   '), write(D), nl.

mostrar_movimiento(lcr,lcr(B,_,_),solo) :-
  name(solo,Str),
  mostrar_bote(B,Str).
mostrar_movimiento(lcr,lcr(B,_,_),Carga) :-
  name(Carga,Str),
  append("llevando el ",Str,Msg),
  mostrar_bote(B,Msg).

mostrar_bote(Desde,Con) :-
  opuesto(Desde,Hacia),
  format("Viajar de ~a a ~a ~s.",[Desde,Hacia,Con]), nl.

opuesto(izquierda,derecha).
opuesto(derecha,izquierda).


/*
 * Problema del paseo del caballo en un tablero de ajedrez de
 * dimensiones arbitrarias.
 *
 * Representaremos el Estado del problema como
 *
 *    knight(F,C,MaxF,MaxC)
 *
 * donde F y C indican la fila y columna actual del caballo en el
 * tablero, mientras que MaxF y MaxC indican las dimensiones máximas
 * del tablero.
 *
 * Los movimientos posibles de un caballo están definidos por la lista
 * de cambios de coordenadas (en el sentido de las agujas del reloj)
 *
 * [ [2,1], [1,2], [-1,2], [-2,1], [-2,-1], [-1,-2], [1,-2], [-1,2] ]
 */

inicial(knight,knight(1,1,8,8)).
final(knight,knight(8,8,8,8)).

movimiento(knight,_,Move) :-
  member(Move, [[2,1],[1,2],[-1,2],[-2,1],[-2,-1],[-1,-2],[1,-2],[-1,2]]).

moverse(knight,knight(F,C,MaxF,MaxC),[DF,DC],knight(NF,NC,MaxF,MaxC)) :-
  NF is F + DF,
  NC is C + DC.

legal(knight,knight(F,C,MaxF,MaxC)) :-
  F >= 1, F =< MaxF,
  C >= 1, C =< MaxC.
