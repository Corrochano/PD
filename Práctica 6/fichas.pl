% Tenemos un conjunto de fichas apiladas en tres columnas sobre una mesa.
%  segun rl esquema siguiente:
%
%   D
%   C   G
%   B   F   I
%   A   E   H
%  -----------
% Esta informacion se representa mediante los siguientes predicados.
% sobre(X,Y) <-> la ficha X esta sobre la ficha Y.
% izquierda(X,Y) <-> la ficha X esta inmediatamente a la izquierda
% de la ficha Y.
% cima(X) <-> la ficha X esta en la cima de una columna.

% hechos
%   D
%   C   G
%   B   F   I
%   A   E   H
%  -----------

cima(d).
sobre(d,c).
sobre(c,b).
sobre(b,a).


cima(g).
sobre(g,f).
sobre(f,e).


cima(i).
sobre(i,h).


izquierda(c,g).

izquierda(b,f).
izquierda(f,i).

izquierda(a,e).
izquierda(e,h).

% Se definen nuevos predicados para manejar esta informacion.


% por_encima_de(X,Y).
% la ficha X esta en la misma pila que la ficha Y y mas arriba.
% uso: por_encima_de(e/s,e/s).
por_encima_de(X,Y) :- sobre(X,Y).
por_encima_de(X,Y) :- sobre(X,Z), por_encima_de(Z,Y).

% por_encima_de_ERROR(X,Y).
%Llamadas recursivas infinitas cuando no hay m‡s soluciones o el objetivo es falso.
% uso: por_encima_de_ERROR(e/s, e/s).
por_encima_de_ERROR(X,Y) :- sobre(X,Y).
por_encima_de_ERROR(X,Y) :- por_encima_de_ERROR(X,Z), sobre(Z,Y).


% pila_izquierda(X,Y)
% la ficha X est‡ en la pila situada inmediatamente a la izquierda de
%la pila en la que est‡ la ficha Y
% uso: pila_izquierda(e/s,e/s)
pila_izquierda(X,Y) :- izquierda(X,Y).				% misma altura
pila_izquierda(X,Y) :- izquierda(Z,Y), por_encima_de(X,Z).	% X mas arriba que Y
pila_izquierda(X,Y) :- izquierda(X,Z), por_encima_de(Y,Z).	% X mas abajo que Y


% por_arriba(X,L).
% L es la lista que contiene todas las fichas que estan por encima de la ficha X.
% uso:  por_arriba(e/s,e/s).
por_arriba(X,[]) :- cima(X).
por_arriba(X,[Y|L]) :- sobre(Y,X), por_arriba(Y,L).



% poner_encima(X,Y)
% la ficha X se puede poner encima de la ficha Y si ambas estan en
% la cima de su pila, y en pilas contiguas.por
% uso: poner_encima(e/s,e/s)
poner_encima(X,Y) :- cima(X), cima(Y), pilas_contiguas(X,Y).


% pilas_contiguas(X,Y)
% la pila de la ficha X y la de la ficha Y estan una al lado de la otra.
% uso: pilas_contiguas(e/s,e/s).
pilas_contiguas(X,Y) :- pila_izquierda(X,Y).
pilas_contiguas(X,Y) :- pila_izquierda(Y,X).

% Función añadida

mas_por_encima_que(X,Y) :-  por_arriba(X,L1), por_arriba(Y,L2), length(L1, N1),
    length(L2,N2), N1 > N2.









