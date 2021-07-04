:- use_module(library(tty)).
:- use_module(library(tipc/tipc_linda)).

write_matrix(Matrix):-
    forall(member(Column, Matrix), write_column(Column)).

write_column(Column):-
    forall(member(X, Column), write(X)),
    write("\n").

write_matrix_with(Matrix, Writer):-
    length(Matrix, L),
    forall(nth1(N, Matrix, Column),
            (forall(member(X, Column), call(Writer, X, N)), (S is L - N, call(Writer, "\n", S)))).

nth_matrix1(posicion(X, Y), Matrix, Element):-
    length(Matrix, Ly),
    between(0, Ly, Y),
    nth1(Y, Matrix, Column),
    length(Column, Lx),
    between(0, Lx, X),
    nth1(X, Column, Element).


laberinto([
  [.., .., .., //, //, //, <>],
  [.., .., .., //, //, .., ..],
  [.., .., .., //, .., .., //],
  [.., .., .., //, .., //, //],
  [.., //, .., //, .., //, //],
  [.., .., .., .., .., <>, //],
  [//, .., //, .., //, //, //],
  [.., .., //, .., //, //, //],
  [//, //, //, .., .., <>, //]  
]).

laberinto2([
    [vv, .., .., //, //, //, <>],
    [>>, vv, .., //, //, .., ..],
    [.., >>, vv, //, .., .., //],
    [.., .., .., //, .., //, //],
    [.., //, .., //, .., //, //],
    [.., .., .., .., .., <>, //],
    [//, .., //, .., //, //, //],
    [.., .., //, .., //, //, //],
    [//, //, //, .., .., <>, //]  
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

libre(..).
libre(<>).

llegada(<>).
direccion(derecha).
direccion(abajo).
direccion(arriba).
direccion(izquierda).

opuestas(arriba, abajo).
opuestas(izquierda, derecha).
opuestas(abajo, arriba).
opuestas(derecha, izquierda).

tras_moverse(arriba, posicion(X, Y), posicion(X, NuevaY)):- NuevaY is Y - 1.
tras_moverse(izquierda, posicion(X, Y), posicion(NuevaX, Y)):- NuevaX is X - 1.
tras_moverse(derecha, posicion(X, Y), posicion(NuevaX, Y)):- NuevaX is X + 1.
tras_moverse(abajo, posicion(X, Y), posicion(X, NuevaY)):- NuevaY is Y + 1.

resolver_laberinto(PosInicial, Laberinto, Camino):-
    % dibujar_laberinto(PosInicial, [], Laberinto),
    tty:string_action(ho),
    length(Laberinto, L),
    do_times(write(" "), L),
    write_matrix_with(Laberinto, write_cell),
    resolver_laberinto_dejando_migas(PosInicial, Laberinto, [], Camino).

resolver_laberinto_dejando_migas(PosInicial, Laberinto, Migas, []):-
    nth_matrix1(PosInicial, Laberinto, Casillero),
    llegada(Casillero),
    dibujar_laberinto(PosInicial, Migas, Laberinto).

resolver_laberinto_dejando_migas(PosInicial, Laberinto, [], [ miga(PosInicial, Direccion) | CaminoRestante ]):-
    nth_matrix1(PosInicial, Laberinto, Casillero),
    not(llegada(Casillero)),
    paso(PosInicial, Laberinto, Direccion, NuevaPos),
    dibujar_laberinto(PosInicial, [], Laberinto),
    resolver_laberinto_dejando_migas(NuevaPos, Laberinto, [ miga(PosInicial, Direccion) ], CaminoRestante).

resolver_laberinto_dejando_migas(PosInicial, Laberinto, MigasDejadas, [ miga(PosInicial, Direccion) | CaminoRestante ]):-
    nth_matrix1(PosInicial, Laberinto, Casillero),
    not(llegada(Casillero)),
    paso(PosInicial, Laberinto, Direccion, NuevaPos),
    last(MigasDejadas, miga(_, DireccionAnterior)),
    not((opuestas(Direccion, DireccionAnterior), dibujar_camino_descartado(PosInicial, [ ], Laberinto))),
    not((member(miga(PosInicial, _), MigasDejadas), dibujar_camino_descartado(PosInicial, [ ], Laberinto))),
    dibujar_laberinto(PosInicial, MigasDejadas, Laberinto),
    append(MigasDejadas, [ miga(PosInicial, Direccion) ], MigasSiguientes),
    (resolver_laberinto_dejando_migas(NuevaPos, Laberinto, MigasSiguientes, CaminoRestante) -> true;
    (dibujar_camino_descartado(PosInicial, [ ], Laberinto), false)).

dibujar_camino_descartado(Pos, Migas, Laberinto):-
    tty:string_action(ho),
    laberinto_con_personaje_y_migas(Laberinto, personaje(Pos, "-@"), Migas, LaberintoDibujable),
    write_matrix_with(LaberintoDibujable, write_discarded_path).

write_in_color(Color, Text):-
    ansi_format(Color, Text, []).

% write_cell(..):- write("  "), !.

write_cell(.., _):-
    write("  "), !.

% write_cell("\n", N):-
%     write("\n"), !.
write_cell("\n", N):-
    write("\n"),
    do_times(write(" "), N).

write_cell(Cell, N):-
    cell_color(Cell, Color, N),
    write_in_color(Color, Cell).

do_times(_, 0):- !.
do_times(Action, Times):- Times > 0, call(Action), NextTimes is Times - 1, do_times(Action, NextTimes).

write_discarded_path("-@", _):-
    write_in_color([fg(80, 80, 80)], --), !.
write_discarded_path(Cell, _):-
    path(Cell), !,
    write_in_color([fg(80, 80, 80)], --).
write_discarded_path(Cell, N):-
    write_path(Cell, N), !.

write_path(Cell, N):-
    path(Cell), !,
    write_cell(Cell, N).
% write_path(_, _):- !.
write_path("\n", N):-
    tty:string_action(do),
    do_times(tty:string_action(nd), N),
    !.
write_path(Cell, _):-
    not(path(Cell)), !,
    tty:string_action(nd),
    tty:string_action(nd).

path(SimboloDireccion):- simbolo_direccion(_, SimboloDireccion).
path("-@").


cell_color(//, [fg(M, 50, O)], N):- O is max(0, min(255, 255 - N * 3)), M is min(255, 150 + N * 3), !.
cell_color(.., [fg(black)], _):- !.
cell_color("-@", [fg(M, M, 0)], N):- M is min(255, 150 + N * 3), !.
cell_color(SimboloDireccion,  [fg(M, 50, 50)], N):- simbolo_direccion(_, SimboloDireccion), M is min(255, 200 + N * 3), !.
cell_color(<>, [fg(cyan)], _):- !.
cell_color(_, [fg(white)], _).

go_back("\n"):-
    tty:string_action(kR), !.
go_back(_).

go_forward("\n"):-
    tty:string_action(kF), !.
go_forward(_).

resuelto(Pos, Laberinto):-
    nth_matrix1(Pos, Laberinto, Casillero),
    llegada(Casillero).

dibujar_laberinto(Pos, Migas, Laberinto):-
    laberinto_con_personaje_y_migas(Laberinto, personaje(Pos, "-@"), Migas, LaberintoDibujable),
    % with_output_to(string(A),
    %                 (set_stream(current_output, tty(true)),
    %                 write_matrix_with(LaberintoDibujable, write_path))
    %                ),
    tty:string_action(ho),
    write_matrix_with(LaberintoDibujable, write_path),
    % write(A),
    sleep(0.05).

% adelantar_espacio_si_resuelto(Pos, Laberinto, LaberintoDibujable):-
%     resuelto(Pos, Laberinto), write_matrix_with(LaberintoDibujable, go_forward).
adelantar_espacio_si_resuelto(Pos, Laberinto, _):-
    not(resuelto(Pos, Laberinto)).

direccion_al_azar(Direccion):-
    findall(D, direccion(D), Direcciones),
    random_permutation(Direcciones, DireccionesAlAzar),
    member(Direccion, DireccionesAlAzar).

paso(Pos, Laberinto, Direccion, NuevaPosicion):-
    direccion_al_azar(Direccion),
    nth_matrix1(Pos, Laberinto, _),
    tras_moverse(Direccion, Pos, NuevaPosicion),
    casillero_libre(Laberinto, NuevaPosicion).

siguiente_paso(Pos, Laberinto, Direccion, DireccionAnterior, NuevaPosicion):-
    paso(Pos, Laberinto, Direccion, NuevaPosicion),
    not(opuestas(Direccion, DireccionAnterior)).

casillero_libre(Laberinto, Posicion):-
    nth_matrix1(Posicion, Laberinto, Casillero),
    libre(Casillero).

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

replace_matrix1(posicion(X, Y), Matrix, Element, NewMatrix) :-
    nth1(Y, Matrix, OldColumn),
    replace1(X, OldColumn, Element, NewColumn),
    replace1(Y, Matrix, NewColumn, NewMatrix).

replace1(Index, List, Element, NewList) :-
    nth1(Index, List, _, Rest),
    nth1(Index, NewList, Element, Rest).

