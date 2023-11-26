:- use_module(library(pce)).
:- encoding(utf8).

% Definir las recetas
receta(pasta, [tomate, pasta]).
receta(ensalada, [lechuga, tomate, cebolla]).
receta(sopa, [pollo, zanahoria, papa]).
receta(huevos, [huevo]).

% Definir las instrucciones de preparación
instrucciones(pasta, 'Cocina la pasta y añade salsa de tomate.').
instrucciones(ensalada, 'Corta la lechuga, el tomate y la cebolla y mézclalos.').
instrucciones(sopa, 'Cocina el pollo con zanahoria y papa.').
instrucciones(huevos, 'Rompe los huevos y cocínalos.').

% Crear la interfaz gráfica
crear_interfaz :-
    new(D, dialog('Recetas')),
    new(T, text_item('Ingresa un ingrediente')),
    new(L, label('')),
    send(D, append(T)),
    send(D, append(button('Agregar', message(@prolog, agregar_ingrediente, T?selection, L, T)))),
    send(D, append(button('Buscar recetas', message(@prolog, buscar_recetas, L)))),
    send(D, append(button('Limpiar ingredientes', message(@prolog, limpiar_ingredientes, L)))),
    send(D, append(L)),
    send(D, open).

% Lista dinámica para almacenar los ingredientes seleccionados
:- dynamic ingredientes/1.

% Agregar un ingrediente a la lista
agregar_ingrediente(Ingrediente, L, T) :-
    assertz(ingredientes(Ingrediente)),
    send(L, selection, 'Ingrediente agregado: '),
    send(L, append, Ingrediente),
    send(L, append, '\n'),
    send(T, clear).

% Limpiar la lista de ingredientes
limpiar_ingredientes(L) :-
    retractall(ingredientes(_)),
    send(L, clear).

% Buscar recetas que se pueden hacer con los ingredientes disponibles
buscar_recetas(L) :-
    findall(I, ingredientes(I), Ingredientes),
    findall(Comida-Instrucciones, (
        receta(Comida, IngredientesReceta),
        subset(IngredientesReceta, Ingredientes),
        instrucciones(Comida, Instrucciones)
    ), ListaRecetas),
    mostrar_recetas(L, ListaRecetas).

% Mostrar las recetas en la interfaz
mostrar_recetas(L, ListaRecetas) :-
    ( ListaRecetas \= [] ->
        mostrar_lista_recetas(L, ListaRecetas)
    ; send(L, selection, 'No hay recetas disponibles con esos ingredientes.\n')
    ).

% Mostrar lista de recetas en la interfaz
mostrar_lista_recetas(L, [Comida-Instrucciones | Resto]) :-
    send(L, selection, 'La receta del menú sugerida es: '),
    send(L, append, Comida),
    send(L, append, '\nReceta: '),
    send(L, append, Instrucciones),
    send(L, append, '\n\n'),
    mostrar_lista_recetas(L, Resto).
mostrar_lista_recetas(_, []).  % Caso base: lista vacía

% Iniciar el programa
:- crear_interfaz.