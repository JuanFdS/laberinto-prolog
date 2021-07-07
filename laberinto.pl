:- use_module(library(tty)).
:- use_module(library(thread)).

% GUARDA con tres cosas:
% 1- como corre cada laberinto en un hilo diferente, puede que con hacer ctrl C + a de abort no pare :P
% en ese caso se puede hacer ctrl C y luego e (de exit). O escribir la consulta halt. para matar prolog

% 2- por como se dibujan tiene que entrar el laberinto en la terminal, así que pone la terminal ocupando
% toda la pantalla o baja el tamaño de la fuente si vas a usar el laberinto grande 

% 3- Solo funciona bien dibujar en terminales UNIX

% Predicado de entrada simplificado, resuelve un laberinto con pos inicial
% Usa depth first y unos colores (?, se podría consultar como
% laberinto_grande(L), resolver_laberinto(posicion(1, 22), L, Camino).

resolver_laberinto(PosInicial, Laberinto, Camino):-
    resolver_laberinto_con(PosInicial, Laberinto, [], [resolver(depth_first, tema1)], Camino).

% Predicado de entrada con todos sus parametros

% Ejemplos:
% Laberinto chico
% laberinto(L), resolver_laberinto_con(posicion(1,1), L, [color(tema2)], [resolver(depth_first, tema2)], Camino).

% Laberinto grande
% laberinto_grande(L), resolver_laberinto_con(posicion(1,22), L, [color(tema2)], [resolver(depth_first, tema2)], Camino).
% Laberinto grande 3d (?
% laberinto_grande(L), resolver_laberinto_con(posicion(1,22), L, [torcido, color(tema3)], [resolver(depth_first, tema3)], Camino).

% Hacer competir a 2
% laberinto_grande(L), resolver_laberinto_con(posicion(1,22), L, [torcido, color(tema1)], [resolver(depth_first, tema1), resolver(breadth_first, ghost(pinky))], Camino).
resolver_laberinto_con(PosInicial, Laberinto, Opciones, Estrategias, Camino):-
    maze_writer_for(Opciones, Writer),
    mutex_create(Mutex),
    dibujar_laberinto_con(PosInicial, [], Laberinto, Writer, maze, Mutex),
    findall(call(Estrategia, PosInicial, Laberinto, color_write(Color, Writer), Mutex, Camino), member(resolver(Estrategia, Color), Estrategias), Resolvedores),
    ligar_camino(Camino, Resolvedores),
    first_solution(Camino, Resolvedores, []).

% Esto se que se debería poder hacer de otra manera pero aun no se me ocurrió como, si no estuviese esta
% linea en resolver_laberinto_con, se llegaría a la solucion pero no se ligaria. 
ligar_camino(_, []).
ligar_camino(Camino, [ call(_, _, _, _, _, Camino) | Resolvedores]):- ligar_camino(Camino, Resolvedores).

% Estrategias posibles
depth_first(Pos, Laberinto, Writer, Mutex, Camino):-
    resolver_laberinto_depth_first(Pos, Laberinto, [], Writer, Mutex, Camino).
breadth_first(Pos, Laberinto, Writer, Mutex, Camino):-
    resolver_laberinto_breadth_first([pos(Pos, [])], Laberinto, Writer, Mutex, Camino).

% Operaciones sobre matrices y listas

replace_matrix1(posicion(X, Y), Matrix, Element, NewMatrix) :-
    nth1(Y, Matrix, OldColumn),
    replace1(X, OldColumn, Element, NewColumn),
    replace1(Y, Matrix, NewColumn, NewMatrix).

replace1(Index, List, Element, NewList) :-
    nth1(Index, List, _, Rest),
    nth1(Index, NewList, Element, Rest).

first(List, Element):-
    nth1(1, List, Element).

write_matrix_with(Matrix, Writer):-
    forall(nth1(Y, Matrix, Column),
            (forall(nth1(X, Column, Value), call(Writer, Value, posicion(X, Y))),
             call(Writer, "\n", posicion(0, Y)))).

nth_matrix1(posicion(X, Y), Matrix, Element):-
    length(Matrix, Ly),
    between(0, Ly, Y),
    nth1(Y, Matrix, Column),
    length(Column, Lx),
    between(0, Lx, X),
    nth1(X, Column, Element).

% Laberintos

laberinto([
  [//, .., //, //, //, //, <>],
  [.., .., //, //, //, .., ..],
  [.., //, //, //, .., .., //],
  [.., .., .., //, .., //, //],
  [.., //, .., //, .., //, //],
  [.., .., .., .., .., .., ..],
  [//, .., //, .., //, //, ..],
  [.., .., //, .., //, //, ..],
  [//, //, //, .., .., .., ..]  
]).

laberinto_grande([
    [//,//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//],
[//,..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//],
[//,..,	//,	//,	//,	//,	//,	..,	//,	..,	//,	..,	//,	//,	//,	//,	//,	..,	//,	..,	//,	..,	//,	//,	//,	//,	//,	//,	//,	//,	//,	..,	//],
[//,..,	..,	..,	..,	..,	//,	..,	//,	..,	//,	..,	//,	..,	..,	..,	//,	..,	//,	..,	//,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	//],
[//,..,	//,	//,	//,	..,	//,	//,	//,	..,	//,	//,	//,	..,	//,	..,	//,	..,	//,	//,	//,	//,	//,	//,	//,	..,	//,	..,	//,	//,	//,	..,	//],
[//,..,	//,	..,	//,	..,	..,	..,	//,	..,	//,	..,	..,	..,	//,	..,	//,	..,	..,	..,	//,	..,	..,	..,	//,	..,	//,	..,	//,	..,	//,	..,	//],
[//,..,	//,	..,	//,	..,	//,	//,	//,	..,	//,	//,	//,	..,	//,	..,	//,	//,	//,	..,	//,	..,	//,	//,	//,	..,	//,	..,	//,	..,	//,	..,	//],
[//,..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	//,	..,	//,	..,	..,	..,	//,	..,	..,	..,	//,	..,	..,	..,	..,	..,	//,	..,	//,	..,	//],
[//,..,	//,	//,	//,	//,	//,	//,	//,	..,	//,	..,	//,	//,	//,	..,	//,	..,	//,	//,	//,	..,	//,	//,	//,	//,	//,	//,	//,	..,	//,	..,	//],
[//,..,	//,	..,	..,	..,	..,	..,	//,	..,	//,	..,	..,	..,	..,	..,	//,	..,	..,	..,	//,	..,	..,	..,	//,	..,	..,	..,	..,	..,	//,	..,	//],
[//,..,	//,	//,	//,	..,	//,	//,	//,	..,	//,	..,	//,	//,	//,	..,	//,	//,	//,	..,	//,	//,	//,	..,	//,	//,	//,	..,	//,	//,	//,	..,	//],
[//,..,	..,	..,	//,	..,	//,	..,	..,	..,	//,	..,	//,	..,	..,	..,	..,	..,	//,	..,	..,	..,	//,	..,	..,	..,	//,	..,	//,	..,	..,	..,	//],
[//,//,	//,	..,	//,	..,	//,	//,	//,	//,	//,	..,	//,	..,	//,	//,	//,	//,	//,	..,	//,	//,	//,	//,	//,	//,	//,	..,	//,	//,	//,	..,	//],
[//,..,	//,	..,	//,	..,	..,	..,	..,	..,	..,	..,	//,	..,	//,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//,	..,	//],
[//,..,	//,	..,	//,	..,	//,	//,	//,	//,	//,	..,	//,	//,	//,	..,	//,	..,	//,	//,	//,	..,	//,	//,	//,	//,	//,	//,	//,	..,	//,	..,	//],
[//,..,	..,	..,	//,	..,	//,	..,	//,	..,	..,	..,	//,	..,	..,	..,	//,	..,	//,	..,	..,	..,	//,	..,	..,	..,	..,	..,	//,	..,	//,	..,	//],
[//,..,	//,	//,	//,	..,	//,	..,	//,	//,	//,	..,	//,	//,	//,	//,	//,	..,	//,	..,	//,	..,	//,	..,	//,	//,	//,	..,	//,	..,	..,	..,	//],
[//,..,	//,	..,	..,	..,	//,	..,	..,	..,	//,	..,	..,	..,	//,	..,	..,	..,	//,	..,	//,	..,	//,	..,	//,	..,	..,	..,	//,	..,	//,	..,	//],
[//,..,	//,	..,	//,	..,	//,	//,	//,	..,	//,	//,	//,	..,	//,	//,	//,	..,	//,	//,	//,	..,	//,	..,	//,	//,	//,	//,	//,	..,	//,	..,	//],
[//,..,	//,	..,	//,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	//,	..,	//,	..,	..,	..,	..,	..,	..,	..,	//,	..,	//],
[//,//,	//,	//,	//,	//,	//,	..,	//,	//,	//,	//,	//,	//,	//,	..,	//,	..,	//,	//,	//,	..,	//,	//,	//,	..,	//,	//,	//,	..,	//,	//,	//],
[..,..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	//,	..,	..,	..,	//,	..,	..,	..,	//,	..,	//,	..,	..,	..,	..,	..,	//],
[//,..,	//,	//,	//,	//,	//,	..,	//,	//,	//,	..,	//,	//,	//,	//,	//,	//,	//,	..,	//,	//,	//,	//,	//,	..,	//,	//,	//,	//,	//,	..,	//],
[//,..,	..,	..,	..,	..,	//,	..,	..,	..,	//,	..,	//,	..,	..,	..,	..,	..,	//,	..,	//,	..,	..,	..,	..,	..,	..,	..,	//,	..,	//,	..,	//],
[//,..,	//,	//,	//,	..,	//,	//,	//,	//,	//,	..,	//,	//,	//,	..,	//,	..,	//,	..,	//,	..,	//,	//,	//,	//,	//,	..,	//,	..,	//,	..,	//],
[//,..,	//,	..,	//,	..,	//,	..,	..,	..,	..,	..,	..,	..,	//,	..,	//,	..,	//,	..,	//,	..,	//,	..,	..,	..,	//,	..,	..,	..,	//,	..,	//],
[//,//,	//,	..,	//,	..,	//,	..,	//,	//,	//,	..,	//,	//,	//,	..,	//,	//,	//,	..,	//,	//,	//,	..,	//,	..,	//,	//,	//,	..,	//,	..,	//],
[//,..,	..,	..,	..,	..,	//,	..,	//,	..,	..,	..,	//,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	//,	..,	//,	..,	//],
[//,//,	//,	//,	//,	..,	//,	..,	//,	//,	//,	//,	//,	..,	//,	..,	//,	//,	//,	..,	//,	//,	//,	..,	//,	//,	//,	..,	//,	..,	//,	//,	//],
[//,..,	..,	..,	//,	..,	//,	..,	//,	..,	..,	..,	..,	..,	//,	..,	..,	..,	//,	..,	//,	..,	..,	..,	..,	..,	//,	..,	//,	..,	..,	..,	<>],
[//,..,	//,	//,	//,	..,	//,	//,	//,	..,	//,	//,	//,	//,	//,	//,	//,	//,	//,	..,	//,	//,	//,	//,	//,	//,	//,	..,	//,	//,	//,	//,	//],
[//,..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//],
[//,//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//]
    ]).

% en este la pasan mal tanto depth first como breadth first
laberinto_muy_abierto([
    [//,//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//],
[//,..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//],
[//,..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//],
[//,..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//],
[//,..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//],
[//,..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//],
[//,..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//],
[//,..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//],
[//,..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//],
[//,..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//],
[//,..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//],
[//,..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//],
[//,..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//],
[//,..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//],
[//,..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//],
[//,..,	..,	..,	//,	..,	//,	..,	//,	..,	..,	..,	//,	..,	..,	..,	//,	..,	//,	..,	..,	..,	//,	..,	..,	..,	..,	..,	//,	..,	//,	..,	//],
[//,..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//],
[//,..,	//,	..,	..,	..,	//,	..,	..,	..,	//,	..,	..,	..,	//,	..,	..,	..,	//,	..,	//,	..,	//,	..,	//,	..,	..,	..,	//,	..,	//,	..,	//],
[//,..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//],
[//,..,	//,	..,	//,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	//,	..,	//,	..,	..,	..,	..,	..,	..,	..,	//,	..,	<>],
[//,..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//],
[..,..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	//,	..,	..,	..,	//,	..,	..,	..,	//,	..,	//,	..,	..,	..,	..,	..,	//],
[//,..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//],
[//,..,	..,	..,	..,	..,	//,	..,	..,	..,	//,	..,	//,	..,	..,	..,	..,	..,	//,	..,	//,	..,	..,	..,	..,	..,	..,	..,	//,	..,	//,	..,	//],
[//,..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//],
[//,..,	//,	..,	//,	..,	//,	..,	..,	..,	..,	..,	..,	..,	//,	..,	//,	..,	//,	..,	//,	..,	//,	..,	..,	..,	//,	..,	..,	..,	//,	..,	//],
[//,..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//],
[//,..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//],
[//,..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//],
[//,..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//],
[//,..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//],
[//,..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//,	..,	..,	..,	..,	..,	..,	..,	..,	..,	//],
[//,//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//,	//]
    ]).

% Casilleros

libre(..).
libre(<>).

llegada(<>).

path(SimboloDireccion):- simbolo_direccion(_, SimboloDireccion).
path("-@").

% Direcciones

direccion(derecha).
direccion(abajo).
direccion(arriba).
direccion(izquierda).

opuestas(arriba, abajo).
opuestas(izquierda, derecha).
opuestas(abajo, arriba).
opuestas(derecha, izquierda).

% Movimientos

% Esto es solo porque era mas divertido si cada vez que se reproducía iba agarrando caminos diferentes,
% da los mismos resultados que direccion(Direccion) 1 vez cada uno
% pero no necesariamente los encuentra en el mismo orden.
direccion_al_azar(Direccion):-
    findall(D, direccion(D), Direcciones),
    random_permutation(Direcciones, DireccionesAlAzar),
    member(Direccion, DireccionesAlAzar).

paso(Pos, Laberinto, Direccion, NuevaPosicion, Migas):-
    direccion_al_azar(Direccion),
    % ^ Se puede reemplazar por direccion(Direccion) para que
    % haga siempre exactamente el mismo recorrido
    nth_matrix1(Pos, Laberinto, _),
    tras_moverse(Direccion, Pos, NuevaPosicion),
    es_paso_valido(Laberinto,
                   miga(NuevaPosicion, Direccion),
                   [ miga(Pos, Direccion) | Migas]).

es_paso_valido(Laberinto, miga(NuevaPosicion, NuevaDireccion), Migas):-
    casillero_libre(Laberinto, NuevaPosicion),
    no_fue_recorrido(miga(NuevaPosicion, NuevaDireccion), Migas),
    forall(first(Migas, miga(_, DireccionAnterior)),
           not(opuestas(NuevaDireccion, DireccionAnterior))).

no_fue_recorrido(miga(NuevaPosicion, _), Migas):-
    not(member(miga(NuevaPosicion, _), Migas)).

casillero_libre(Laberinto, Posicion):-
    nth_matrix1(Posicion, Laberinto, Casillero),
    libre(Casillero).

% Resoluciones de los laberintos, mucho del codigo
% es para dibujar ahora mismo, lo tengo que separar (?

resuelto(Posicion, Laberinto):-
    nth_matrix1(Posicion, Laberinto, Casillero),
    llegada(Casillero).

%% Resolucion breadth first
resolver_laberinto_breadth_first([ pos(Posicion, MigasDejadas) | _ ], Laberinto, Writer, Mutex, MigasDejadas):-
    resuelto(Posicion, Laberinto),
    dibujar_laberinto_con(Posicion, MigasDejadas, Laberinto, Writer, path, Mutex).

resolver_laberinto_breadth_first([ pos(Posicion, MigasDejadas) | Positions ], Laberinto, Writer, Mutex, Camino):-
    not(resuelto(Posicion, Laberinto)),
    findall(pos(NuevaPosicion, [ miga(Posicion, Direccion) | MigasDejadas ]), paso(Posicion, Laberinto, Direccion, NuevaPosicion, MigasDejadas), Pasos),
    forall(member(_, Pasos),
            (
                take(1, MigasDejadas, MigasDejadasEnPasoAnterior),
                dibujar_laberinto_con(Posicion, MigasDejadasEnPasoAnterior, Laberinto, Writer, path, Mutex)
            )
            ),
    forall(length(Pasos, 0),
            (
            take(1, MigasDejadas, MigasDejadasEnPasoAnterior),
            dibujar_laberinto_con(Posicion, MigasDejadasEnPasoAnterior, Laberinto, Writer, discarded_path, Mutex)
            )
           ),
    append(Positions, Pasos, NuevosPasos),
    resolver_laberinto_breadth_first(NuevosPasos, Laberinto, Writer, Mutex, Camino).


%% Resolucion Depth First
resolver_laberinto_depth_first(PosInicial, Laberinto, Migas, Writer, Mutex, []):-
    resuelto(PosInicial, Laberinto),
    dibujar_laberinto_con(PosInicial, Migas, Laberinto, Writer, path, Mutex).

resolver_laberinto_depth_first(PosInicial, Laberinto, MigasDejadas, Writer, Mutex, [ miga(PosInicial, Direccion) | CaminoRestante ]):-
    not(resuelto(PosInicial, Laberinto)),
    paso(PosInicial, Laberinto, Direccion, NuevaPos, MigasDejadas),
    take(1, MigasDejadas, MigasDejadasEnPasoAnterior),
    dibujar_laberinto_con(PosInicial, MigasDejadasEnPasoAnterior, Laberinto, Writer, path, Mutex),
    resolver_laberinto_depth_first(NuevaPos, Laberinto,
                [ miga(PosInicial, Direccion) | MigasDejadas ],
                Writer,
                Mutex,
                CaminoRestante).

resolver_laberinto_depth_first(Pos, Laberinto, _, Writer, Mutex, _):-
    dibujar_laberinto_con(Pos, [], Laberinto, Writer, discarded_path, Mutex),
    false.

tras_moverse(arriba, posicion(X, Y), posicion(X, NuevaY)):- NuevaY is Y - 1.
tras_moverse(izquierda, posicion(X, Y), posicion(NuevaX, Y)):- NuevaX is X - 1.
tras_moverse(derecha, posicion(X, Y), posicion(NuevaX, Y)):- NuevaX is X + 1.
tras_moverse(abajo, posicion(X, Y), posicion(X, NuevaY)):- NuevaY is Y + 1.



%
% Para lo unico que se usa todo el resto del codigo es para dibujar, ni lo vean (?
%

ghost(Ghost, discarded_path, S, _, Color):- path(S), ghost(Ghost, path, S, _, Color), !.
ghost(pinky, path, S, _, [fg(255,182,193)]):- path(S), !.
ghost(blinky, path, S, _, [fg(red)]):- path(S), !.
ghost(inky, path, S, _, [fg(cyan)]):- path(S), !.
ghost(clyde, path, S, _, [fg(255,165,0)]):- path(S), !.
ghost(_, path, Simbolo, _, [fg(110, 110, 110)]):- path(Simbolo).

% Decorator de dibujadores de laberintos

maze_writer_for([ color(ColorChooser) | Opciones ], color_write(ColorChooser, PreviousWriter)):-
    maze_writer_for(Opciones, PreviousWriter).
maze_writer_for([ torcido | Opciones ], torcido_write(PreviousWriter)):-
    maze_writer_for(Opciones, PreviousWriter).
maze_writer_for([ ], regular_write).

take(_, [], []).
take(0, _, []).
take(N, [ X | List ], [ X | NewList ]):-
    N > 0,
    NextN is N - 1,
    take(NextN, List, NewList).

write_in_color(Color, Text):-
    ansi_format(Color, Text, []).

with_color(Color, WriteAction):-
    with_output_to(string(S), WriteAction),
    write_in_color(Color, S).

do_times(_, 0):- !.
do_times(Action, Times):- Times > 0, call(Action), NextTimes is Times - 1, do_times(Action, NextTimes).

color_write(ChooseCellColor, Writer, TypeOfWrite, Cell, Position):-
    call(ChooseCellColor, TypeOfWrite, Cell, Position, Color),
    with_color(Color, call(Writer, TypeOfWrite, Cell, Position)).
color_write(ChooseCellColor, Writer, TypeOfWrite, Cell, Position):-
    not(call(ChooseCellColor, TypeOfWrite, Cell, Position, _)),
    call(Writer, TypeOfWrite, Cell, Position).

torcido_write(Writer, maze, "\n", posicion(X, Y)):-
    call(Writer, maze, "\n", posicion(X, Y)), 
    do_times(write(" "), Y),
    !.
torcido_write(Writer, TypeOfWrite, "\n", posicion(X, Y)):-
    TypeOfWrite \= maze,
    call(Writer, TypeOfWrite, "\n", posicion(X, Y)),
    do_times(tty:string_action(nd), Y),
    !.
torcido_write(Writer, TypeOfWrite, Cell, Position):-
    call(Writer, TypeOfWrite, Cell, Position).

regular_write(path, "\n", _):-
    tty:string_action(do),
    !.
regular_write(path, Cell, _):-
    not(path(Cell)),
    tty:string_action(nd),
    tty:string_action(nd),
    !.
regular_write(path, Cell, _):-
    path(Cell),
    write(Cell),
    !.
regular_write(discarded_path, Cell, _):-
    path(Cell),
    write(--),
    !.
regular_write(discarded_path, Cell, _):-
    not(path(Cell)),
    regular_write(path, Cell, _),
    !.
regular_write(maze, .., _):- write("  "), !.
regular_write(maze, Cell, _):- write(Cell), !.

even(N):- mod(N,2) =:= 0.
odd(N):- mod(N,2) =:= 1.

tema3(maze, //, posicion(_, Y), [fg(200, 200, 200)]):- even(Y), !.
tema3(maze, //, posicion(_, Y), [fg(255, 255, 255)]):- odd(Y), !.
tema3(maze, <>, _, [fg(cyan)]):- !.
tema3(path, "-@", _, [fg(yellow)]):- !.
tema3(discarded_path, Cell, _, [fg(80, 80, 80)]):- path(Cell), !.
tema3(path, Cell, _, [fg(red)]):- path(Cell), !.


tema2(maze, //, posicion(_, N), [fg(O, M, P)]):-
    O is max(0, min(255, 255 - N * 3)),
    M is min(255, 150 + N * 3),
    P is 255 - O,
    !.
tema2(maze, <>, _, [fg(cyan)]):- !.
tema2(discarded_path, Cell, _, [fg(80, 80, 80)]):- path(Cell), !.
tema2(path, "-@", posicion(_, N), [fg(M, M, 0)]):- M is min(255, 150 + N * 3), !.
tema2(path, _, _, [fg(cyan)]).

tema1(maze, //, posicion(_, N), [fg(M, 50, O)]):- O is max(0, min(255, 255 - N * 3)), M is min(255, 150 + N * 3), !.
tema1(maze, <>, _, [fg(cyan)]):- !.
tema1(discarded_path, Cell, _, [fg(80, 80, 80)]):- path(Cell), !.
tema1(path, "-@", posicion(_, N), [fg(M, M, 0)]):- M is min(255, 150 + N * 3), !.
tema1(path, Cell, posicion(_, N), [fg(M, 50, 50)]):- path(Cell), M is min(255, 200 + N * 3), !.


dibujar_laberinto_con(Pos, Migas, Laberinto, Writer, TypeOfWrite, Mutex):-
    % with_mutex(Mutex, write(Mutex)).
    laberinto_con_personaje_y_migas(Laberinto, personaje(Pos, "-@"), Migas, LaberintoDibujable),
    with_mutex(Mutex,
        (
            tty:string_action(ho),
            write_matrix_with(LaberintoDibujable, call(Writer, TypeOfWrite))
        )),
    sleep_after_dibujar(TypeOfWrite).

sleep_after_dibujar(path):- sleep(0.05), !.
sleep_after_dibujar(_).

laberinto_con_personaje_y_migas(Laberinto, personaje(Posicion, Dibujo), [], LaberintoResultado):-
    replace_matrix1(Posicion, Laberinto, Dibujo, LaberintoResultado).
laberinto_con_personaje_y_migas(Laberinto, Personaje, [ miga(Posicion, Direccion) | Migas ], LaberintoResultado):-
    simbolo_direccion(Direccion, Simbolo),
    replace_matrix1(Posicion, Laberinto, Simbolo, L),
    laberinto_con_personaje_y_migas(L, Personaje, Migas, LaberintoResultado).

simbolo_unico_direccion(arriba, "^").
simbolo_unico_direccion(abajo, "v").
simbolo_unico_direccion(izquierda, ">").
simbolo_unico_direccion(derecha, "<").

simbolo_direccion(arriba, ^^).
simbolo_direccion(abajo, vv).
simbolo_direccion(izquierda, <<).
simbolo_direccion(derecha, >>).
