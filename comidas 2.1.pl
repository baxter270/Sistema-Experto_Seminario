:- use_module(library(pce)).
:- encoding(utf8).

% Definir las recetas
receta(pasta, [tomate, pasta, hierbas]).
receta(ensalada, [lechuga, tomate, cebolla]).
receta(sopa, [pollo, zanahoria, papa]).
receta(huevos, [huevo, mantequilla]).

% Definir las instrucciones de preparación
instrucciones(pasta, [
    '1. Hierve agua en una olla y añade sal.',
    '2. Cocina la pasta en el agua hirviendo según las instrucciones del paquete.',
    '3. Mientras tanto, prepara una salsa de tomate con tomates frescos y hierbas.',
    '4. Escurre la pasta cocida y mezcla con la salsa de tomate.',
    '5. Sirve caliente y disfruta.'
]).

instrucciones(ensalada, [
    '1. Lava y escurre la lechuga.',
    '2. Corta el tomate en rodajas finas.',
    '3. Pela y corta la cebolla en aros delgados.',
    '4. Mezcla la lechuga, el tomate y la cebolla en un tazón.',
    '5. Opcionalmente, añade aderezo de tu elección y mezcla bien.'
]).

instrucciones(sopa, [
    '1. Hierve agua en una olla.',
    '2. Añade el pollo, zanahoria y papa a la olla con agua hirviendo.',
    '3. Cocina a fuego medio hasta que el pollo esté bien cocido y las verduras estén tiernas.',
    '4. Sazona al gusto con sal y pimienta.',
    '5. Sirve caliente y disfruta de tu sopa casera.'
]).

instrucciones(huevos, [
    '1. Rompe los huevos en un tazón y bátelos bien con un tenedor.',
    '2. Calienta una sartén antiadherente y agrega un poco de mantequilla o aceite.',
    '3. Vierte los huevos batidos en la sartén caliente.',
    '4. Cocina a fuego medio, revolviendo ocasionalmente, hasta que los huevos estén bien cocidos.',
    '5. Sirve caliente y disfruta.'
]).

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
    send(L, append, '\nReceta:\n'),
    mostrar_instrucciones(L, Instrucciones),
    send(L, append, '\n\n'),
    mostrar_lista_recetas(L, Resto).
mostrar_lista_recetas(_, []).  % Caso base: lista vacía

% Mostrar instrucciones detalladas en la interfaz
mostrar_instrucciones(L, [Paso | Resto]) :-
    send(L, append, Paso),
    send(L, append, '\n'),
    mostrar_instrucciones(L, Resto).
mostrar_instrucciones(_, []).  % Caso base: lista vacía

% Iniciar el programa
:- crear_interfaz.
