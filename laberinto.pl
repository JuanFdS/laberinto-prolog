:- use_module(library(tty)).
:- use_module(library(tipc/tipc_linda)).
:- use_module(library(thread)).

write_matrix(Matrix):-
    forall(member(Column, Matrix), write_column(Column)).

write_column(Column):-
    forall(member(X, Column), write(X)),
    write("\n").

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

laberinto([
  [.., .., .., //, //, //, <>],
  [.., .., .., //, //, .., ..],
  [.., .., .., //, .., .., //],
  [.., .., .., //, .., //, //],
  [.., //, .., //, .., //, //],
  [.., .., .., .., .., .., //],
  [//, .., //, .., //, //, //],
  [.., .., //, .., //, //, //],
  [//, //, //, .., .., .., //]  
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

laberinto_grande2([
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
    resolver_laberinto_con(PosInicial, Laberinto, Camino, []).

resolver_laberinto_con(PosInicial, Laberinto, Opciones, Camino):-
    maze_writer_for(Opciones, Writer),
    dibujar_laberinto_con(PosInicial, [], Laberinto, Writer, maze),
    resolver_laberinto_dejando_migas(PosInicial, Laberinto, [], Writer, Camino).

maze_writer_for([ color(ColorChooser) | Opciones ], color_write(ColorChooser, PreviousWriter)):-
    maze_writer_for(Opciones, PreviousWriter).
maze_writer_for([ torcido | Opciones ], torcido_write(PreviousWriter)):-
    maze_writer_for(Opciones, PreviousWriter).
maze_writer_for([ ], regular_write).

resuelto(Posicion, Laberinto):-
    nth_matrix1(Posicion, Laberinto, Casillero),
    llegada(Casillero).

resolver_laberinto_dejando_migas(PosInicial, Laberinto, Migas, Writer, []):-
    resuelto(PosInicial, Laberinto),
    dibujar_laberinto_con(PosInicial, Migas, Laberinto, Writer, path).

resolver_laberinto_dejando_migas(PosInicial, Laberinto, MigasDejadas, Writer, [ miga(PosInicial, Direccion) | CaminoRestante ]):-
    not(resuelto(PosInicial, Laberinto)),
    paso(PosInicial, Laberinto, Direccion, NuevaPos, MigasDejadas),
    dibujar_laberinto_con(PosInicial, MigasDejadas, Laberinto, Writer, path),
    resolver_laberinto_dejando_migas(NuevaPos, Laberinto,
                                     [ miga(PosInicial, Direccion) | MigasDejadas ],
                                     Writer,
                                     CaminoRestante).

resolver_laberinto_dejando_migas(Pos, Laberinto, _, Writer, _):-
    dibujar_laberinto_con(Pos, [], Laberinto, Writer, discarded_path),
    false.

write_in_color(Color, Text):-
    ansi_format(Color, Text, []).

with_color(Color, WriteAction):-
    with_output_to(string(S), (set_stream(current_output, tty(true)), WriteAction)),
    write_in_color(Color, S).

do_times(_, 0):- !.
do_times(Action, Times):- Times > 0, call(Action), NextTimes is Times - 1, do_times(Action, NextTimes).

cuando_esta(Opcion, Opciones, Accion):-
    forall(option(Opcion, Opciones), Accion).

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

path(SimboloDireccion):- simbolo_direccion(_, SimboloDireccion).
path("-@").

even(N):- mod(N,2) =:= 0.
odd(N):- mod(N,2) =:= 1.

cell_color3(maze, //, posicion(X, Y), [fg(200, 200, 200)]):- even(Y), !.
cell_color3(maze, //, posicion(X, Y), [fg(255, 255, 255)]):- odd(Y), !.
cell_color3(maze, <>, _, [fg(cyan)]):- !.
cell_color3(path, "-@", _, [fg(yellow)]):- !.
cell_color3(discarded_path, Cell, _, [fg(80, 80, 80)]):- path(Cell), !.
cell_color3(path, Cell, _, [fg(red)]):- path(Cell), !.


cell_color2(maze, //, posicion(_, N), [fg(O, M, P)]):-
    O is max(0, min(255, 255 - N * 3)),
    M is min(255, 150 + N * 3),
    P is 255 - O,
    !.
cell_color2(maze, <>, _, [fg(cyan)]):- !.
cell_color2(discarded_path, Cell, _, [fg(80, 80, 80)]):- path(Cell), !.
cell_color2(path, "-@", posicion(_, N), [fg(M, M, 0)]):- M is min(255, 150 + N * 3), !.
cell_color2(path, _, _, [fg(cyan)]).

cell_color(maze, //, posicion(_, N), [fg(M, 50, O)]):- O is max(0, min(255, 255 - N * 3)), M is min(255, 150 + N * 3), !.
cell_color(maze, <>, _, [fg(cyan)]):- !.
cell_color(discarded_path, Cell, _, [fg(80, 80, 80)]):- path(Cell), !.
cell_color(path, "-@", posicion(_, N), [fg(M, M, 0)]):- M is min(255, 150 + N * 3), !.
cell_color(path, Cell, posicion(_, N), [fg(M, 50, 50)]):- path(Cell), M is min(255, 200 + N * 3), !.


dibujar_laberinto_con(Pos, Migas, Laberinto, Writer, TypeOfWrite):-
    laberinto_con_personaje_y_migas(Laberinto, personaje(Pos, "-@"), Migas, LaberintoDibujable),
    tty:string_action(ho),
    write_matrix_with(LaberintoDibujable, call(Writer, TypeOfWrite)),
    sleep_after_dibujar(TypeOfWrite).

sleep_after_dibujar(path):- sleep(0.05), !.
sleep_after_dibujar(_).

direccion_al_azar(Direccion):-
    findall(D, direccion(D), Direcciones),
    random_permutation(Direcciones, DireccionesAlAzar),
    member(Direccion, DireccionesAlAzar).

paso(Pos, Laberinto, Direccion, NuevaPosicion, Migas):-
    direccion_al_azar(Direccion),
    nth_matrix1(Pos, Laberinto, _),
    tras_moverse(Direccion, Pos, NuevaPosicion),
    es_paso_valido(Laberinto,
                   miga(NuevaPosicion, Direccion),
                   [ miga(Pos, Direccion) | Migas]).

es_paso_valido(Laberinto, miga(NuevaPosicion, NuevaDireccion), Migas):-
    casillero_libre(Laberinto, NuevaPosicion),
    no_fue_recorrido(miga(NuevaPosicion, NuevaDireccion), Migas),
    forall(nth0(0, Migas, miga(_, DireccionAnterior)),
           not(opuestas(NuevaDireccion, DireccionAnterior))).

no_fue_recorrido(miga(NuevaPosicion, _), Migas):-
    not(member(miga(NuevaPosicion, _), Migas)).

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
