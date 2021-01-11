/*
 ->> Practica #2: PROLOG
 Asignatura: Inteligencia Articial
 Curso: 2020/2021
 Grado de Ingenieria Informatica
 Autor: Eduardo Da Silva Yanes
 Fecha de entrega: 13/01/2021
 
 ->> Descripcion de la practica:
 En esta práctica he realizado un recomendador de contenido/entretenimiento.
 Tenemos una base de conocimiento con diversas peliculas, series y artistas musicales.
 Cada una de ellas tiene asociada una serie de generos que al compararla con los gustos 
 introducidos podemos determinar si es un contenido apto o no apto.
 
 ->> Metodo de uso:
 Ejecutar la sentencia:
 ?- recomendador.
*/

:- use_module(library(pairs)).
:- discontiguous genero/2.
:- discontiguous musica/1.
:- discontiguous videojuego/1.
:- discontiguous serie/1.
:- discontiguous pelicula/1.

/*
 ## PARTE PRINCIPAL ##
 Sentencia principal del programa. Aqui tenemos un menu donde se nos pregunta que queremos ver
 y nuestros gustos. Esto se almacena y se invoca a la sentencia "recomienda" que es la que hara 
 el trabajo de buscar.
*/
recomendador:-
    write('Sistema recomendador de contenido - Prolog'), nl,
    write('<< Por favor, escribe todo en minuscula >>'), nl, 
    write('Como te llamas?'),
    read(Name),
	write('Hola '), write(Name), nl,
    % Seleccionamos el tipo de entretenimiento
    write('¿Que te apetece disfrutar?'), nl,
    write('1. Serie'), nl,
    write('2. Pelicula'), nl,      
    write('3. Musica'), nl,
    write('4. Videojuegos'), nl,
    % En base a la respuesta asignamos un valor a Tipo
    read(DatoTipo),nl,
    (  DatoTipo == 1 -> Tipo = 'serie';
    	DatoTipo == 2 ->  Tipo = 'pelicula';
    	DatoTipo == 3 ->  Tipo = 'musica';
        DatoTipo == 4 ->  Tipo = 'videojuego'
	),
    write('----------------------'), nl,
    write('Vamos a buscar '), write(Tipo), write(' para ti'),nl,
	% Recogemos los gustos del usuario en una lista "ListaGustos"
    write('Cuentame que géneros te gustaría. Cuando hayas acabado escribe stop.'), nl,
    leer_lista_gustos(ListaGustos, stop),
    write('En base a tus gustos te recomendamos:'), nl,
   	% Mostramos los resultados
    forall(recomienda(Tipo,ListaGustos,X), writeln(X)).

/*
 Sentencia para recoger la entrada del usuario y almacenarla en una lista. En este caso la usamos
 para almacenar los gustos del usuario.
*/
leer_lista_gustos(L, End) :-
    (   leer_gusto(E, End)
    ->  L = [E|L1],
        leer_lista_gustos(L1, End)
    ;   L = []
    ).

leer_gusto(E, End) :-
    read(E),
    dif(E, End).


/* Sentencias para realizar la recomendacion */
recomienda(serie,Gustos,Recomendacion):-
	findall(Serie,serie(Serie),Series),						
	make_recomendation(Gustos,Series,Recomendaciones),			
	member(Recomendacion,Recomendaciones).	

recomienda(videojuego,Gustos,Recomendacion):-
	findall(Videojuego,videojuego(Videojuego),Videojuegos),						
	make_recomendation(Gustos,Videojuegos,Recomendaciones),			
	member(Recomendacion,Recomendaciones).	

recomienda(musica,Gustos,Recomendacion):-
	findall(Musica,musica(Musica),Musicos),						
	make_recomendation(Gustos,Musicos,Recomendaciones),			
	member(Recomendacion,Recomendaciones).	

recomienda(pelicula,Gustos,Recomendacion):-
	findall(Pelicula,pelicula(Pelicula),Peliculas),				
	make_recomendation(Gustos,Peliculas,Recomendaciones),		
	member(Recomendacion,Recomendaciones).						

make_recomendation(_,[],[]).								
make_recomendation(Gustos,Elementos,SortedRecomendaciones):-	
	findall(Calidad-Elemento,(member(Elemento,Elementos),calidad(Elemento,Gustos,Calidad),Calidad>0),Pairs),
	sort(Pairs,AuxPairs),																															
	invert(AuxPairs,SortedPairs),								
	pairs_values(SortedPairs,SortedRecomendaciones).			

%Indica que tanto coinciden los gustos con los generos
calidad(Elemento,Gustos,Calidad):-
	genero(Elemento,Generos),
	my_intersect(Gustos,Generos,Comunes),
	tam(Comunes,Calidad).

% Devuelve la interseccion entre dos listas
my_intersect([],_,[]).
my_intersect([A|As],Bs,[A|Cs]):-
    member(A,Bs),
    !,
    my_intersect(As,Bs,Cs).
my_intersect([_|As],Bs,Cs):-
	my_intersect(As,Bs,Cs).

% Invierte una lista
invert([],[]).
invert([H|L],L2):-
	invert(L,L3),
	addend(H,L3,L2).

% Añade un elemento al final de una lista
addend(X,[],[X]).
addend(X,[C|R],[C|R1]):-
	addend(X,R,R1). 

% Devuelve el tamaño de una lista
tam([],0).
tam([_|L],N):-
	tam(L,X),
	N is X + 1.

%-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-

/************************
 * BASE DE CONOCIMIENTO *
 ************************/
%-----------------------------
% >>> Series de television <<<
%-----------------------------
serie('The Big Bang Theory').
genero('The Big Bang Theory',[comedia, humor, romance, amigos, ciencia, sitcom, friki]).

serie('Stranger Things').
genero('Stranger Things',[horror, fantasia, misterio, drama, monstruos, sci-fi, scifi, thriller]).

serie('Black Mirror').
genero('Black Mirror',[drama, sci-fi, scifi, thriller, tecnologia, sociedad]).

serie('Game of Thrones').
genero('Game of Thrones',[drama, sci-fi, scifi, accion, fantasia, dragones, medieval]).

serie('Doctor Who').
genero('Doctor Who',[drama, sci-fi, scifi, accion, fantasia, tiempo, misterio, famosa, robot, alien]).

serie('Homeland').
genero('Homeland',[drama, accion, misterio, thriller, cia, terrorista, terrorismo, bipolar]).

serie('The Boys').
genero('The Boys',[sci-fi, scifi, accion, fantasia, comedia, heroes, superheroes, crimen]).

serie('Breaking Bad').
genero('Breaking Bad',[drama, thriller, accion, crimen, drogas, ilegal]).

serie('Elite').
genero('Elite',[drama, thriller, crimen, escuela, gay, española]).

serie('La Casa de Papel').
genero('La Casa de Papel',[accion, thriller, crimen, drama, dinero, robo, atraco, española]).

serie('Rick y Morty').
genero('Rick y Morty',[dibujos, adultos, ciencia, comedia, humor, borracho, animacion, sci-fi, scifi]).

serie('American Dad').
genero('American Dad',[dibujos, adultos, comedia, humor, cia, animacion, pez, alien]).

serie('13 razones').
genero('13 razones',[drama, thriller, crimen, escuela, gay, bullying, suicidio, cintas, carrete, violacion]).

% -----------------
% >>> Peliculas <<<
% -----------------
pelicula('Star Wars').
genero('Star Wars',[espacial, espacio, scifi, sci-fi, blockbuster, accion, robot, alien]).

pelicula('Titanic').
genero('Titanic',[romance, catastrofe, drama, tragedia, real, oceano]).

pelicula('El Padrino').
genero('El Padrino',[drama, suspense, mafia, crimen, gangster]).

pelicula('Ghost in the Shell').
genero('Ghost in the Shell',[scifi, sci-fi, ficcion, cyberpunk, ciberpunk, thriller, animacion, futuro]).

pelicula('Alien').
genero('Alien',[terror, scifi, sci-fi, ficcion, suspense]).

pelicula('1917').
genero('1917',[guerra, belica, accion, drama, thriller, secuencia, soldados]).

pelicula('Fast and Furious Saga').
genero('Fast and Furious Saga',[accion, drama, thriller, policia, carreras, coches, robos, crimen]).

pelicula('Joker').
genero('Joker',[drama, thriller, policia, payaso, crimen, asesino, villano]).

pelicula('Interstellar').
genero('Interstellar',[nasa, espacio, scifi, sci-fi, aventura, ficcion, ciencia]).

pelicula('Mad Max').
genero('Mad Max',[accion, aventura, thriller, scifi, sci-fi, ficcion, apocalipsis, desierto, fantasia]).

pelicula('Coco').
genero('Coco',[mexico, disney, familiar, muertos, animacion, emotiva]).

%--------------------
% >>> Videojuegos <<<
%--------------------
videojuego('Watch Dogs').
genero('Watch Dogs',[hacker, tecnologia, crimen, heroe, venganza, abierto, campaña, multijugador, explorar]).

videojuego('Counter Strike Global Offensive').
genero('Counter Strike Global Offensive',[armas, fps, disparos, terroristas, policias, bombas, guerra, belico, competitivo, ranked, tactico]).

videojuego('Assetto Corsa').
genero('Assetto Corsa',[carreras, coches, vehiculos, simulacion, realismo, mods]).

videojuego('Cyberpunk 2077').
genero('Cyberpunk 2077',[historia, campaña, futuro, rpg, disparos, abierto, cuidad, explorar]).

videojuego('Fortnite').
genero('Fortnite',[construccion, disparos, competitivo, ranked, multijugador, campaña, creativo, battleroyale]).

videojuego('Minecraft').
genero('Minecraft',[sandbox, explorar, supervivencia, multijugador, solo, infinito]).

videojuego('Among Us').
genero('Among Us',[detective, asesino, indie, multijugador, multiplataforma]).

videojuego('Life Is Strange').
genero('Life Is Strange',[campaña, historia, decision, poderes, lineal]).

videojuego('Resident Evil 5').
genero('Resident Evil 5',[campaña, historia, miedo, horror, zombies, supervivencia, apocalipsis]).

%---------------
% >>> Musica <<<
%---------------
musica('Machine Gun Kelly').
genero('Machine Gun Kelly',[rock, pop, punk, ingles, hombre, hiphop, rap, trap]).

musica('BTS').
genero('BTS',[pop, kpop, koreano, coreano, boyband, chicos, korea]).

musica('BlackPink').
genero('BlackPink',[pop, femenino, chicas, kpop, korea, coreano]).

musica('Bring Me The Horizon').
genero('Bring Me The Horizon',[metal, rock, pop, masculino, alternativo]).

musica('Queen').
genero('Queen',[rock, opera, pop, clasico, ingles, metal]).

musica('Nirvana').
genero('Nirvana',[gringe, rock, alternativo, clasico]).

musica('Led Zeppelin').
genero('Led Zeppelin',[clasico,rock]).

musica('Enrique Iglesias').
genero('Enrique Iglesias',[reggaeton, bachata, rb, pop ,latino]).

musica('Don Omar').
genero('Don Omar',[reggaeton,latino]).

musica('Bad Bunny').
genero('Bad Bunny',[reggaeton, hiphop, trap,latino]).

musica('Shakira').
genero('Shakira',[reggaeton, pop, rb, latino]).

musica('Little Big').
genero('Little Big',[rave, rap, rusos]).

musica('Juan Luis Guerra').
genero('Juan Luis Guerra',[bachata, salsa, merengue, mambo, latino, pop]).
