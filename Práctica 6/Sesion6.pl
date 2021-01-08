% Nombre: Álvaro Corrochano López

% Ejercicio 1

%  a)

% ?- por_encima_de(X,c).
% X = d .

% ?- por_encima_de(c,X).
% X = b .

% ?- por_arriba(b,X).
% X = [c, d] .

% ?- por_arriba(X,Y).
% X = d,
% Y = [] .

% ?- poner_encima(X,f).
% false.

% ?- poner_encima(g,d).
% true .

% ?- poner_encima(d,g).
% true .

% ?- poner_encima(X,g).
% X = d .

% b)

% ?- por_encima_de(X,Y), cima(Y).
% false.

% ?- cima(Y), pila_izquierda(X,Y), cima(X).
% Y = g,
% X = d .

% ?- por_arriba(a,X), member(Y,X), por_encima_de(Z,Y).
% X = [b, c, d],
% Y = b,
% Z = c .

% c)
% Necesitamos por_arriba(X,Y).

mas_por_encima_que(X,Y) :-  por_arriba(X,L1), por_arriba(Y,L2), length(L1, N1),
    length(L2,N2), N1 > N2.

% ?- mas_por_encima_que(a,h).
% true .
% ?- mas_por_encima_que(a,d).
% true .
% ?- mas_por_encima_que(d,c).
% false.
% ?- mas_por_encima_que(e,c).
% true
% ?- mas_por_encima_que(b,f).
% true .
% ?- mas_por_encima_que(f,b).
% false.

% Ejercicio 2

mezcla([],_, []).
mezcla(_,[],[]).
mezcla(_,_,[]) :- false.

mezcla([X|T1],L2, [X|T]) :- mezcla2(T1,L2,T).
mezcla([X|T1],_,[_|T]) :- false.

mezcla2([],_, []).
mezcla2(_,[],[]).
mezcla2(_,_,[]) :- false.

mezcla2(L1,[X|T2], [X|T]) :- mezcla(L1,T2,T).
mezcla2(_,[X|T2], [_|T]) :- false.


% Ejercicio 3

% a)

sublista([],_).
sublista([X|T1], [X|T2]) :- sublista2(T1,T2).
sublista([X|T1], [_|T2]) :- sublista([X|T1],T2).

sublista2([],_).
sublista2([X|T1],[Y|T2]) :- X == Y, sublista2(T1,T2).


% b)

contenida([], _).
contenida([X|T1], L2) :- member(X,L2),contenida(T1,L2).

% Ejercicio 4

nat(c).
nat(s(X)) :- nat(X).

sum(X,c,X).
sum(X,s(Y),s(Z)):- sum(X,Y,Z).

sum(X,Y,Z) :- nat(X), nat(Y), nat(Z), Z = X + Y.

arbol(void).
arbol(arbol(_,I,D)):- arbol(I), arbol(D).

num_nodos(void,X) :- X = c.
num_nodos(arbol(_,I,D), X) :- num_nodos(I,Y), num_nodos(D,Z),
   sum(Y,Z,W), X = s(W).

% ?- num_nodos(arbol(5,arbol(2,void,void),arbol(1,void,void)),X).
% X = s(s(s(c))) .
% ?-num_nodos(arbol(5,arbol(2,arbol(2,void,void),void),arbol(1,void,void)),X).
% X = s(s(s(s(c))))
% num_nodos(arbol(2,void,void),X).
% X = s(c) .
% ?- num_nodos(void,X).
% X = c.
% ?- num_nodos(arbol(2,arbol(3,void,void),void),X).
% X = s(s(c)) .
% ?- num_nodos(arbol(2,void,arbol(3,void,void)),X).
% X = s(s(c)) .
