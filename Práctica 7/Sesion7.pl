% Nombre: Álvaro Corrochano López

% Ejercicio 1

% a) Usando igualdad sintáctica
elimina1([ ],X,[ ]).
elimina1([X|R],Y,NR) :- Y == X, elimina1(R,Y, NR).
elimina1([X|R],Y,[X|NR]) :- Y \== X, elimina1(R,Y,NR).
% b) Usando unificación
elimina2([ ],X,[ ]).
elimina2([X|R],Y,NR) :- Y = X, elimina2(R,Y, NR).
elimina2([X|R],Y,[X|NR]) :- Y \= X, elimina2(R,Y,NR).
% c) Combinando las dos  anteriores
elimina3([ ],X,[ ]).
elimina3([X|R],X,NR) :- elimina3(R,X,NR).
elimina3([X|R],Y,[X|NR]) :- Y \== X, elimina3(R,Y,NR).

% Ejecución de eliminai([a,b,a,c],a,L). (i= 1,2,3)
% Elimina1: L = [b,c].
% Elimina2: L = [b,c].
% Elimina3: L = [b,c].

% Ejecucuión de eliminai([a,b,a,c],X,L). (i= 1,2,3)
% Elimina1: L = [a,b,a,c].
% Elimina2: X = a,
%           L = [b,c].
% Elimina3: X = a,
%           L = [b,c].

% *Ejecución extra: eliminai([a,b,a,c],X,[b,c]). (i= 1,2,3)
% Elimina1: false
% Elimina2: X = a
% Elimina3: X = a

% Observamos que en la primera ejecución se obtiene siempre el mismo
% resultado pero en la segunda, elimina1 no elimina nada de la lista
% mientras que elimina2 y elimina3 deciden eliminar a (X = a).
% Probando a dar una lista con las a eliminadas, elimina2 y elimina3
% devuelven X = a, pero elimina1 no, devuelve false, por lo que se
% puede observar que elimina1 no puede deducir cuál es el elemento
% eliminado.
% Esto concluye que elimina1 no es una buena solución, al no poder
% averiguar el valor de una X eliminada dado que hace una comparación
% nada más empezar (motivo por el que devuelve false en el tercer
% ejemplo), siendo mejores soluciones elimina2 y elimina3.

% Ejercicio 2

arbol(void).
arbol(arbol(_,I,D)):- arbol(I), arbol(D).

% a) sumatree(A,N) <-> A

sumatree(void, N) :- N = 0.
sumatree(arbol(X,I,D),N) :- sumatree(I,Y), sumatree(D,Z), N is X + Y + Z.

% ?- sumatree(arbol(2,void,void),N).
% N = 2.

% ?- sumatree(arbol(2,arbol(3,void,arbol(5,void,void)),arbol(4,void,void)),N).
% N = 14.

% ?- sumatree(void,X).
% X = 0


% b) maximo(A,X) <-> A

maximo(void, M) :- M = 0.
maximo(arbol(X,I,D),M) :- maximo(I,M1), maximo(D,M2),
    (   X > M1 ->
        (   X > M2 -> M is X;
            M is M2
        );

        (   M1 > M2 -> M is M1;
            M is M2
        )
    ).

% ?- maximo(void, M).
% M = 0.

% ?- maximo(arbol(2,void,void),M).
% M = 2.

% ?- maximo(arbol(2,void,arbol(3,void,void)),M).
% M = 3.

% ?- maximo(arbol(4,arbol(3,void,void),void),M).
% M = 4.

% ?- maximo(arbol(2,arbol(3,void,void),void),M).
% M = 3.

% ?- maximo(arbol(2,arbol(3,void,arbol(5,void,void)),arbol(4,void,void)),N).
% N = 5.


% Ejercicio 3

sublistasde([],L2) :- L2 = [].
sublistasde([X|L],L2) :- sublistasde(L,L3), sublistasIzquierdas(X,L3,L6), append([L],[[X]],L4), append(L3,L4,L5), append(L5,L6,L7),sort(L7,L2).

sublistasIzquierdas(_,[],L2) :- L2 = [].
sublistasIzquierdas(X,[Y|L],L2) :- sublistasIzquierdas(X,L,L3), append([X],Y,L4),append([L4],L3,L5), sort(L5,L2).

% Ejercico 4
hanoi(0, _, _, _, []). % Si no hay fichas, no mueve nada
hanoi(1, A, B, _, [A,B]). % Si solo hay una ficha, la muevo a la torre final
hanoi(N, A, B, C, M) :-
    Naux is (N-1), % En tamaño 2:
    hanoi(Naux,A,C,B,M1),% Primero, de ini a aux
    hanoi(1,A,B,C,M2), % Segundo, de ini a fin
    hanoi(Naux,C,B,A,M3), % Tercero, de aux a fin
    append(M1,M2,Aux),
    append(Aux,M3,M).

