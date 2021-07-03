:- use_module(library(tty)).

write_matrix(Matrix):-
    forall(member(Column, Matrix), write_column(Column)).

write_column(Column):-
    forall(member(X, Column), write(X)),
    write("\n").

write_matrix_with(Matrix, Writer):-
    length(Matrix, L),
    call(Writer, "\n", L),
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
    % write_matrix_with(Laberinto, write_cell),
    resolver_laberinto_dejando_migas(PosInicial, Laberinto, [], Camino).

resolver_laberinto_dejando_migas(PosInicial, Laberinto, Migas, []):-
    nth_matrix1(PosInicial, Laberinto, Casillero),
    llegada(Casillero),
    dibujar_laberinto(PosInicial, Migas, Laberinto).

resolver_laberinto_dejando_migas(PosInicial, Laberinto, MigasDejadas, [ miga(PosInicial, Direccion) | CaminoRestante ]):-
    nth_matrix1(PosInicial, Laberinto, Casillero),
    not(llegada(Casillero)),
    paso(PosInicial, Laberinto, Direccion, NuevaPos),
    not(member(miga(PosInicial, _), MigasDejadas)),
    dibujar_laberinto(PosInicial, MigasDejadas, Laberinto),
    append(MigasDejadas, [ miga(PosInicial, Direccion) ], MigasSiguientes),
    resolver_laberinto_dejando_migas(NuevaPos, Laberinto, MigasSiguientes, CaminoRestante).

write_in_color(Color, Text):-
    ansi_format(Color, Text, []).

write_cell(.., _):-
    write("  "), !.

% write_cell(..):- write("  "), !.

write_space_times(1):- write(" ").
write_space_times(N):- N > 1, write(" "), NextN is N - 1, write_space_times(NextN).

write_cell("\n", N):-
    write("\n"),
    write_space_times(N).


write_cell(Cell, N):-
    cell_color(Cell, Color, N),
    write_in_color(Color, Cell).

write_path(Cell):-
    path(Cell), !,
    write_cell(Cell, _).
write_path("\n"):-
    go_forward("\n"), !.
write_path(Cell):-
    not(path(Cell)), !,
    tty:string_action(nd),
    write_path(Cell).

path(SimboloDireccion):- simbolo_direccion(_, SimboloDireccion).


cell_color(//, [fg(M, 0, O)], N):- O is max(0, min(255, 255 - N * 3)), M is min(255, 150 + N * 3), !.
cell_color(.., [fg(black)], _):- !.
cell_color("-@", [fg(M, M, 0)], N):- M is min(255, 150 + N * 3), !.
cell_color(SimboloDireccion,  [fg(M, M, M)], N):- simbolo_direccion(_, SimboloDireccion), M is min(255, 100 + N * 3), !.
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
    with_output_to(string(A),
                    (set_stream(current_output, tty(true)),
                    write_matrix_with(LaberintoDibujable, write_cell))
                   ),
    % writeln(Pos),
    % write_matrix_with(LaberintoDibujable, write_cell),
    % write_matrix_with(LaberintoDibujable, go_back),
    tty:string_action(ho),
    write(A),
    % tty:string_action(ho),
    % adelantar_espacio_si_resuelto(Pos, Laberinto, LaberintoDibujable),
    sleep(0.05).

% adelantar_espacio_si_resuelto(Pos, Laberinto, LaberintoDibujable):-
%     resuelto(Pos, Laberinto), write_matrix_with(LaberintoDibujable, go_forward).
adelantar_espacio_si_resuelto(Pos, Laberinto, _):-
    not(resuelto(Pos, Laberinto)).

% direccion_al_azar(Direccion):-
%     findall(D, direccion(D), Direcciones),
%     random_permutation(Direcciones, DireccionesAlAzar),
%     member(Direccion, DireccionesAlAzar).

paso(Pos, Laberinto, Direccion, NuevaPosicion):-
    direccion(Direccion),
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

simbolo_direccion(arriba, "^^").
simbolo_direccion(abajo, "vv").
simbolo_direccion(izquierda, "<<").
simbolo_direccion(derecha, ">>").

replace_matrix1(posicion(X, Y), Matrix, Element, NewMatrix) :-
    nth1(Y, Matrix, OldColumn),
    replace1(X, OldColumn, Element, NewColumn),
    replace1(Y, Matrix, NewColumn, NewMatrix).

replace1(Index, List, Element, NewList) :-
    nth1(Index, List, _, Rest),
    nth1(Index, NewList, Element, Rest).

