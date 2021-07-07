# GUARDA con tres cosas:
1- como corre cada laberinto en un hilo diferente, puede que con hacer ctrl C + a de abort no pare :P.

En ese caso se puede hacer ctrl C y luego e (de exit). O escribir la consulta halt. para matar prolog

2- por como se dibujan tiene que entrar el laberinto en la terminal, así que pone la terminal ocupando toda la pantalla o baja el tamaño de la fuente si vas a usar el laberinto grande 

3- Solo funciona bien dibujar en terminales UNIX

## Predicado de entrada simplificado, resuelve un laberinto con pos inicial

Usa depth first y unos colores (?, se podría consultar como
```prolog
> laberinto_grande(L), resolver_laberinto(posicion(1, 22), L, Camino).
```

## Predicado de entrada con todos sus parametros

### Ejemplos:

Laberinto chico
```prolog
laberinto(L), resolver_laberinto_con(posicion(1,1), L, [color(tema2)], [resolver(depth_first, tema2)], Camino).
```

Laberinto grande
```prolog
laberinto_grande(L), resolver_laberinto_con(posicion(1,22), L, [color(tema2)], [resolver(depth_first, tema2)], Camino).
```

Laberinto grande 3d (?
```prolog
laberinto_grande(L), resolver_laberinto_con(posicion(1,22), L, [torcido, color(tema3)], [resolver(depth_first, tema3)], Camino).
```

Hacer competir a 2

```prolog
laberinto_grande(L), resolver_laberinto_con(posicion(1,22), L, [torcido, color(tema1)], [resolver(depth_first, tema1), resolver(breadth_first, ghost(pinky))], Camino).

```