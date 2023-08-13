:- module(_,_,[classic,assertions,regtypes]).

author_data('Jiang', '', 'Lucia', 'b19m042').

:- doc(title, "M-arias y Arañas").
:- doc(author, "Luc@'{i}a Jiang, b19m042").

:- doc(hide,[author_data/4]).
:- doc(module, "Esta práctica consiste en repasar los conceptos fundamentales estudiados en el bloque 
    temático de programación con ISO-Prolog. El trabajo se divide en 2 partes. 
    
    @section{Parte 1: Particiones M-arias de un número}
    El objetivo de la primera es escribir un predicado @var{maria/3} tal que el tercer argumento es el número de 
    particiones @var{M}-arias del segundo argumento siendo @var{M} el primer argumento.
    @subsection{Predicado 1.1}
    Dados @var{M} y @var{N} enteros, @var{Ps} es la lista con las potencias de @var{M} que son menores o 
    iguales que @var{N} en orden descendiente. 
    @includedef{pots/3}

    Para eso llamamos a la función auxiliar @var{pots_aux/4} que nos devuelve la lista que queremos en 
    orden creciente, por lo que invertimos utilizando el predicado @var{reverse} predefinido.

    @var{P} es la lista en orden creciente de las potencias de @var{M} menores que @var{N}.  
    @var{Ins} se utiliza para tener un registro de cuál ha sido el último número introducido.

    Terminamos el método cuando @var{Ins} > @var{N} y realizamos corte.
    @includedef{pots_aux/4}

    @subsection{Predicado 1.2}
    Dados @var{M} y @var{N} enteros, devuelve @var{P} por backtracking todas las particiones 
    @var{M}-arias de @var{N} representadas como listas de enteros. Para ello recurrimos a un método
    auxiliar.
    @includedef{mpart/3}

    El método auxiliar @var{mpart/4} se llama con los mismos parámetros que mpart, con un cuarto extra que almacena el 
    último valor de la lista, de manera que los elementos de más a la derecha son siempre menores 
    que los de su izquierda.

    Obtenemos la lista @var{P} de las potencias menores que @var{M} utilizando el predicado definido en
    pots. Aprovechamos el método predefinido member para ir escogiendo los elementos de @var{P} con 
    backtracking para mostrar todas las particiones @var{M}-arias.
    @includedef{mpart_aux/4} 

    @subsection{Predicado 1.3}
    @var{NPart} es el número de particiones @var{M}-arias de @var{N}. 
    
    Utilizaremos los predicados predefinidos @var{findall} para obtener en una lista todas las @var{M}-particiones 
    posibles, y length para obtener el número @var{NPart}.
    @includedef{maria/3}

    @section{Parte 2: Arañas de expansión de un grafo}
    La segunda consiste en buscar si existen arañas de expansión de un grafo. Una araña de expansión de un 
    grafo es un árbol de expansión que además es una araña. 
    
    Un árbol de expansión es un subgrafo que es árbol y que contiene todos los vértices del grafo. 
    Una araña es un grafo con como máximo un vértice cuyo grado (número de aristas incidentes a él) es 3 o más. 
    
    Por tanto, una araña de expansión de un grafo es un subgrafo que es un árbol, contiene todos los vértices 
    del grafo, y como máximo un solo vértice de grado 3 o más.
    
    @begin{alert}
        Se asume que los grafos de entrada son conexos y no dirigidos, por lo que para representar la conexión 
        entre dos vértices a,b, incluiremos en la base de hechos el hecho arista(a,b) o el hecho arista(b,a),
        pero no ambos.
    @end{alert}

    @subsection{Predicado 2.1}
    Dado un grafo representado como una lista de aristas @var{G}, se aserta en la base de datos como hechos 
    del predicado @var{arista/2} los elementos de @var{G}.

    Para ello, se define @var{arista/2} como predicado dinámico en el programa. Al llamar al predicado @var{guardar_grafo(G)}
    se borran todos los hechos que hubiera anteriormente.

    Cuando se llame a este predicado, se hará un retractall de todos los predicados de la forma @var{arista(_,_)}, y se
    llama al predicado @var{guardar_grafo_aux/1} que añade los hechos recursivamente.
    @includedef{guardar_grafo/1}

    Se utiliza un predicado auxiliar, @var{guardar_grafo_aux/1} que recorre la lista @var{G} que contiene las aristas del 
    grafo para añadir a la base de hechos. Se utiliza el predicado @var{assert} predefinido.
    @includedef{guardar_grafo_aux/1}

    @subsection{Predicado 2.2}
    Sabemos que en un árbol @var{A+1=V}, siendo @var{A} el número de aristas y @var{V} el de vértices. 
    
    Además, para que sea araña hemos visto que tiene que ser conexo y con como máximo un vértice mayor que 3. 
    
    Para estudiar esto, definiremos varios predicados auxiliares previos:
    @begin{itemize}
        @item @bf{Clonar las aristas de la base de hechos.} Todas las operaciones que realizaremos será en una base de hechos con
        los predicados @var{arista_copia}.

        Clonamos todas las @var{arista} de la base de hechos a otras, @var{arista_copia/2}, de manera que 
        todas las modificaciones que realizamos son en esta posterior. Para ello, se recurre a una auxiliar, 
        @var{clonar_aristas_aux/1} con la lista de aristas.
        @includedef{clonar_aristas/0}

        Además, utilizamos una auxiliar para recorrer la base de hechos, que encontramos en la lista @var{L} pasada por parámetro. 
        Se inserta recursivamente en @var{arista_copia/2}, otro predicado dinámico.
        @includedef{clonar_aristas_aux/1}

        @item @bf{Obtener número de vértices a quitar.} Hemos visto que en un árbol se cumple que @var{V=A+1}. Por tanto, miraremos cuántas
        aristas tenemos que quitar para que pueda ser un árbol, @var{N=A-V+1}. Cabe mencionar que esto es una condición necesaria para ser 
        araña pero no suficiente.
        @includedef{num_aristas_a_quitar/1}

        @begin{itemize}
            @item @bf{Obtener listas de aristas}. Tendremos dos predicados, uno para las aristas originales y otra para las clonadas.
            @includedef{lista_aristas/1}
            @includedef{lista_aristas_org/1}

            @item @bf{Obtener lista de vértices.} Para no obtener repetidos, utilizamos la función sort, que nos deja un conjunto ordenado.
            @includedef{lista_vertices/1}

            @item @bf{Obtener número de aristas}. Nos aprovechamos el predicado @var{lista_aristas/1} para calcular el número de aristas
            mirando la longitud de la lista.
            @includedef{num_aristas/1}

            @item @bf{Obtener número de vértices}. Nos aprovechamos el predicado @var{lista_vertices/1} para calcular el número de vértices
            mirando la longitud de la lista.
            @includedef{num_vertices/1}

        @end{itemize}

        @item @bf{Comprobar conexión.} Llamaremos a este predicado cada vez que quitemos una arista, pues no tiene sentido quitar una arista
        y que se desconecte el grafo, dejando de ser un árbol y por tanto araña. 

        Un grafo es conexo si dado cualquier vértice @var{V} del grafo, existe un camino entre @var{V} y cualquier vértice del grafo.

        @begin{itemize}
            @item @bf{Dos vértices unidos.} El predicado es cierto si existe una arista de @var{X} a @var{Y} o una arista de @var{Y} 
            a @var{X}. En definitiva, que estén unidos los dos vértices. 
            @includedef{unido/2}

            @item @bf{Existe camino entre dos vértices.} El predicado es cierto si existe un camino que lleve del vértice @var{X} a @var{Y}
            @includedef{existe_camino/2}
            
            Utilizamos un predicado auxiliar, @var{existe_camino_aux/3} que mira si existe un camino entre @var{X} y @var{Y} y almacena en @var{V}
            la lista de vértices que ya se han recorrido para no volver a acceder y evitar bucles infinitos.
            @includedef{existe_camino_aux/3}

            @item @bf{Conexo. } El predicado es cierto si el grafo registrado en la base de datos es conexo.
            @includedef{conexo/0}
            
            Con otra auxiliar y con la lista de vértices @var{V}, seleccionamos el primer vértice y buscamos si existe
            un camino que los una.
            @includedef{conexo_aux/2}
        @end{itemize}

        @item @bf{Un único vértice con grado mayor que 3.} 
        
        Definiremos un predicado para obtener el grado de un vértice en particular.
        @includedef{grado_vertice/2}

        Y posteriormente, evaluamos el grado de todos los vértices. En el caso de que más de un vértice diera con grado mayor que 3,
        salta un fallo.
        @includedef{un_unico_vertice_mas3/0}
        @includedef{un_unico_vertice_mas3_aux/2}

        @item @bf{Quitar aristas.} Utilizando los predicados declarados anteriormente, quitaremos las aristas que dejen al subgrafo conexo, y comprobaremos que el subgrafo final tenga como máximo un 
        único vértice con grado 3 o más.

        Aprovechamos el predicado member para realizar estas operaciones con backtracking.

        @includedef{quitar_arista/0}
        @includedef{quitar_arista_aux/2}

        Con el propósito de poder realizar backtracking correctamente, no podemos quitar simplemente las aristas de la base de hechos,
        pues en el caso de dar fallo y retornar, la arista se habría perdido. Por tanto, se añade una cláusula al predicado, que se accede si 
        no existe la arista que se quiere retractar, a volver a añadirla y dar un fallo.
        @includedef{eliminar/1} 

    @end{itemize}

    Con todos estos predicados, podemos llamar al que se nos pedía: Dado un grafo @var{G} guardado en la base de datos, tiene éxito si el grafo proporcionado
    contiene una araña de expansión y falla finitamente en caso contrario.
    @includedef{aranya/0}

    @section{Tests}
    Con el propósito de testear los predicados, se han creado unos predicados cuyo objetivo es únicamente comprobar que todos funcionan de manera
    esperada.
    @begin{itemize}
        @item test_pots/3 @includedef{test_pots/3}
        @item test_mpart/3 @includedef{test_mpart/3}
        @item test_maria/3 @includedef{test_maria/3}
        @item test_guardar_grafo/1 @includedef{test_guardar_grafo/1} @includedef{test_guardar_grafo_aux/1}
        @item test_aranya/1  @includedef{test_aranya/1}
    @end{itemize}
    ").

%------------Pred 1.1--------------
:- pred pots(M,N,Ps)
    #"Dados @var{M} y @var{N} enteros, @var{Ps} es la lista con las potencias de @var{M} que son menores o 
    iguales que @var{N} en orden descendiente. @includedef{pots/3} 
    ".
% Llamamos a la auxiliar e invertimos la lista obtenida.
pots(1,_,[1]). %caso base
pots(M,N,Ps) :- M>1, pots_aux(M,N,L,1), reverse(L,Ps).

:- pred pots_aux(M,N,P,Ins)
    #" @var{P} es la lista en orden creciente de las potencias de @var{M} menores que @var{N}.  
    @var{Ins} es el último valor introducido.
    @includedef{pots_aux/4}
    ".
% vamos multiplicando el último valor por M hasta que el valor que se vaya a insertar sea mayor que N
pots_aux(_,N,[],Aux) :- Aux>N, !. %caso base
pots_aux(M,N,[Ins|Ps],Ins) :- Aux is Ins*M, pots_aux(M,N,Ps, Aux).

%------------Pred 1.2--------------
:- pred mpart(M,N,P)
    #"Dados @var{M} y @var{N} enteros, devuelve @var{P} por backtracking todas las particiones 
    @var{M}-arias de @var{N} representadas como listas de enteros.
    @includedef{mpart/3}
    ".
% llamamos al método auxiliar con un cuarto parámetro
mpart(M,N,P) :- mpart_aux(M,N,P,N).

:- pred mpart_aux(M,N,P,Ant)
    #"@var{P} es la lista de particiones @var{M}-arias de @var{N} representadas como listas de enteros.
    @includedef{mpart_aux/4}
    ".
% auxiliar:
mpart_aux(_,0,[],_). %caso base
mpart_aux(M,N,[X|P],Ant) :- pots(M,N,Ps), member(X,Ps), X =< N, X=< Ant, Sup is N-X, mpart_aux(M,Sup,P,X).

%------------Pred 1.3--------------
:- pred maria(M,N,NPart)
    #"@var{NPart} es el número de particiones @var{M}-arias de @var{N}. 
    @includedef{maria/3}
    ".
maria(M,N,NPart) :- findall(_,mpart(M,N,_), R), length(R, NPart).

%------------Pred 2.1--------------
:- pred arista(X,Y)
    #"Predicado dinámico con el que definimos un grafo".
:- dynamic arista/2.

:- pred guardar_grafo(G)
    #"Dado un grafo representado como una lista de aristas @var{G}, se aserta en la base de datos como hechos 
    del predicado arista/2 los elementos de @var{G}.
    @includedef{guardar_grafo/1}
    ".
guardar_grafo(G) :- retractall(arista(_,_)), guardar_grafo_aux(G).

:- pred guardar_grafo_aux(G)
    #"Recorre la lista @var{G} que contiene las aristas del grafo para añadir a la base de hechos.
    @includedef{guardar_grafo_aux/1}
    ".
guardar_grafo_aux([]). %caso base
guardar_grafo_aux([G1|G]) :- assert(G1), guardar_grafo_aux(G).

%------------Pred 2.2--------------
% A-V+1=N -> N >= 0, N nº de vértices a quitar
% conexo
% solo un vertice con grado 3 o mas

:- pred aranya
   #" Dado un grafo @var{G} guardado en la base de datos, tiene éxito si el grafo proporcionado
   contiene una araña de expansión.
   ". 
aranya :- clonar_aristas, quitar_arista.

% predicado auxiliar, copia todas las aristas
:- pred arista_copia(X,Y)
    #"Predicado dinámico que utilizamos para definir un grafo.
    ".
:- dynamic arista_copia/2.

:- pred clonar_aristas
    #"Clona todas las aristas de la base de datos original a @var{aristas_copia/2}
    @includedef{clonar_aristas/0}
    ".
clonar_aristas :- retractall(arista_copia(_,_)), lista_aristas_org(L), clonar_aristas_aux(L).

:- pred clonar_aristas_aux(L)
    #"Clona todas las aristas de la base de datos original a @var{aristas_copia/2}
    @includedef{clonar_aristas_aux/1}
    ".
clonar_aristas_aux([]).
clonar_aristas_aux([arista(X,Y)|L]) :- assert(arista_copia(X,Y)), clonar_aristas_aux(L).

:- pred lista_aristas_org(L)
    #"Obtenemos la lista @var{L} con todas las aristas que hay en la base de datos original.
    @includedef{lista_aristas_org/1}
    ".
%lista de listas con los vértices unidos por una arista
lista_aristas_org(L) :- findall(arista(X,Y), arista(X,Y), L).

:- pred lista_aristas(L)
    #"Obtenemos la lista @var{L} con todas las aristas que hay en la base de datos clonada.
    @includedef{lista_aristas/1}
    ".
lista_aristas(L) :- findall(arista_copia(X,Y), arista_copia(X,Y), L).

:- pred num_aristas(N)
    #" @var{N} es el número de aristas del grafo @var{G}.
    @includedef{num_aristas/1}
    ".
num_aristas(N) :- findall(_, arista(_,_),L), length(L,N).

:- pred lista_vertices(L)
    #"@var{L} es la lista de los vértices del grafo.
    @includedef{lista_vertices/1}
    ".
lista_vertices(Lord) :- findall(X, arista(X,_), L1), findall(Y, arista(_,Y), L2), append(L1,L2,L), sort(L,Lord).

:- pred num_vertices(N)
    #" @var{N} es el número de vértices del grafo @var{G}.
    @includedef{num_vertices/1}
    ".
num_vertices(N) :- lista_vertices(L), length(L, N).


:- pred num_aristas_a_quitar(N)
    #"@var{N=A-V+1}, @var{N} es el número de aristas que hay que quitar del grafo para 
    que sea un árbol (condición necesaria pero no suficiente).
    @includedef{num_aristas_a_quitar/1}
    ".
num_aristas_a_quitar(N) :- num_aristas(A), num_vertices(V), N is A-V+1.

:- pred unido(X,Y)
    #"El predicado es cierto si existe una arista de @var{X} a @var{Y} o una arista de @var{Y} 
    a @var{X}.
    @includedef{unido/2}
    ".
%comprueba que dado dos vértices existe un camino entre los dos
unido(X,X).
unido(X,Y) :- arista_copia(X,Y).
unido(X,Y) :- arista_copia(Y,X).

:- pred conexo 
    #"Cierto si el grafo de la base de datos es conexo.
    @includedef{conexo/0}
    ".
%ver que es un grafo conexo: escogido un vértice, puede llegar a todos los demás
conexo :- lista_vertices([L1|L]), conexo_aux(L1, L).

:- pred conexo_aux(V,L)
    #"Cierto si para el vértice @var{V} existe un camino con todos los elementos de la lista @var{L}
    @includedef{conexo_aux/2}
    ".
conexo_aux(_,[]).
conexo_aux(X,[V1|V]) :- existe_camino(X,V1), conexo_aux(X,V).

:- pred existe_camino(X,Y)
    #"El predicado es cierto si existe un camino que lleve del vértice @var{X} a @var{Y}
    @includedef{existe_camino/2}
    ".
existe_camino(X,Y) :- existe_camino_aux(X,Y,[X]), !.

:- pred existe_camino_aux(X,Y,V)
    #"Predicado auxiliar que mira si existe un camino entre @var{X} y @var{Y} y almacena en @var{V}
    la lista de vértices que ya se han recorrido.
    @includedef{existe_camino_aux/3}
    ".
existe_camino_aux(X,Y,_) :- unido(X,Y).
existe_camino_aux(X,Y,V) :- unido(X,C), C \== Y, \+member(C,V), existe_camino_aux(C,Y,[C|V]). 

:- pred quitar_arista
    #"El predicado es cierto si se consiguen quitar @var{N} manteniendo que sea conexo. 
    @includedef{quitar_arista/0}
    ".
quitar_arista :- num_aristas_a_quitar(N), lista_aristas(L), quitar_arista_aux(N, L).

:- pred quitar_arista_aux(N,L)
    #"Predicado auxiliar que elimina @var{N} aristas de la lista @var{L} manteniendo que sea conexo y que 
    el grafo final resultante tenga solo un vértice con grado mayor que 3.
    @includedef{quitar_arista_aux/2}
    ".
quitar_arista_aux(0,_) :- !, conexo, un_unico_vertice_mas3.
quitar_arista_aux(N,L) :- conexo, member(X,L), call(X), eliminar(X), N2 is N-1, quitar_arista_aux(N2, L).

:- pred eliminar(A)
    #"Predicado que elimina el predicado arista @var{A} de la base de hechos.
    @includedef{eliminar/1}
    ".
eliminar(A) :- retract(A).
eliminar(A) :- assert(A), fail.

:- pred grado_vertice(X,N)
    #"@var{N} es el grado del vértice @var{X}.
    @includedef{grado_vertice/2}
    ".
grado_vertice(X,N) :- findall(X, arista_copia(X,_), L1), findall(X, arista_copia(_,X), L2), length(L1, N1), length(L2, N2), N is N1+N2.

:- pred un_unico_vertice_mas3
    #"Predicado que es cierto si hay como máximo un vértice con grado 3 o más.
    @includedef{un_unico_vertice_mas3/0}
    ".
un_unico_vertice_mas3 :- lista_vertices(L), un_unico_vertice_mas3_aux(L, 0).

:- pred un_unico_vertice_mas3_aux(L,N)
    #"Predicado auxiliar que dado la lista de vértices existente en el grafo @var{L}, determina si hay como máximo un vértice de grado 3 o más.
    @includedef{un_unico_vertice_mas3_aux/2}
    ".
un_unico_vertice_mas3_aux([],N) :- !, N=<1.
un_unico_vertice_mas3_aux([L1|L],N) :- grado_vertice(L1,N1), N1 >= 3, !, N2 is N+1, un_unico_vertice_mas3_aux(L,N2).
un_unico_vertice_mas3_aux([_|L],N) :- un_unico_vertice_mas3_aux(L,N).

%-------------------------------- TESTS --------------------------------

% Predicado 1.1: pots------------------------------
:- pred test_pots(M,N,Ps)
    #"Comprueba que los resultados del predicado sean los esperados.
    @includedef{test_pots/3}
    ".
test_pots(M,N,Ps) :- pots(M,N,Ps).

:- test test_pots(M,N,Ps) : (M=2, N=12) => (Ps = [8,4,2,1]) + not_fails.
:- test test_pots(M,N,Ps) : (M=3, N=50) => (Ps = [27,9,3,1]) + not_fails.
:- test test_pots(M,N,Ps) : (M=4, N=100) => (Ps = [64,16,4,1]) + not_fails.

:- test test_pots(M,N,Ps) : (M=2, N=12, Ps = [8,4,2]) + fails #"Falta el elemento 1".
:- test test_pots(M,N,Ps) : (M=3, N=50, Ps = [1,3,9,27]) + fails #"Lista en orden contrario".
:- test test_pots(M,N,Ps) : (M=4, N=50, Ps = [64,16,4,1]) + fails #"Se excede el límite superior".

% Predicado 1.2: mpart------------------------------
:- pred test_mpart(M,N,Ps)
    #"Comprueba que los resultados del predicado sean los esperados.
    @includedef{test_mpart/3}
    ".
test_mpart(M,N,Ps) :- mpart(M,N,Ps).

:- test test_mpart(M,N,P) : (M=2, N=12, P = [8,4]) +  not_fails.
:- test test_mpart(M,N,P) : (M=2, N=12, P = [8,2,2]) +  not_fails.
:- test test_mpart(M,N,P) : (M=2, N=12, P = [8,2,1,1]) +  not_fails.
:- test test_mpart(M,N,P) : (M=2, N=12, P = [8,1,1,1,1]) +  not_fails.
:- test test_mpart(M,N,P) : (M=2, N=12, P = [4,4,4]) +  not_fails.
:- test test_mpart(M,N,P) : (M=2, N=12, P = [4,4,2,2]) +  not_fails.
:- test test_mpart(M,N,P) : (M=2, N=12, P = [4,4,2,1,1]) +  not_fails.
:- test test_mpart(M,N,P) : (M=2, N=12, P = [4,4,1,1,1,1]) +  not_fails.
:- test test_mpart(M,N,P) : (M=2, N=12, P = [4,2,2,2,2]) +  not_fails.
:- test test_mpart(M,N,P) : (M=2, N=12, P = [4,2,2,2,1,1]) +  not_fails.
:- test test_mpart(M,N,P) : (M=2, N=12, P = [4,2,2,1,1,1,1]) +  not_fails.
:- test test_mpart(M,N,P) : (M=2, N=12, P = [4,2,1,1,1,1,1,1]) +  not_fails.
:- test test_mpart(M,N,P) : (M=2, N=12, P = [4,1,1,1,1,1,1,1,1]) +  not_fails.
:- test test_mpart(M,N,P) : (M=2, N=12, P = [2,2,2,2,2,2]) +  not_fails.
:- test test_mpart(M,N,P) : (M=2, N=12, P = [2,2,2,2,2,1,1]) +  not_fails.
:- test test_mpart(M,N,P) : (M=2, N=12, P = [2,2,2,2,1,1,1,1]) +  not_fails.
:- test test_mpart(M,N,P) : (M=2, N=12, P = [2,2,2,1,1,1,1,1,1]) +  not_fails.
:- test test_mpart(M,N,P) : (M=2, N=12, P = [2,2,1,1,1,1,1,1,1,1]) +  not_fails.
:- test test_mpart(M,N,P) : (M=2, N=12, P = [2,1,1,1,1,1,1,1,1,1,1]) +  not_fails.
:- test test_mpart(M,N,P) : (M=2, N=12, P = [1,1,1,1,1,1,1,1,1,1,1,1]) +  not_fails.

:- test test_mpart(M,N,P) : (M=2, N=12, P = [8,1,2,1]) +  fails #"Las listas tienen que estar en orden decreciente".
:- test test_mpart(M,N,P) : (M=2, N=12, P = [4,8]) +  fails #"Las listas tienen que estar en orden decreciente".
:- test test_mpart(M,N,P) : (M=2, N=12, P = [8,2]) +  fails #"No suman N".

% Predicado 1.3: maria------------------------------
:- pred test_maria(M,N,NPart)
    #"Comprueba que los resultados del predicado sean los esperados.
    @includedef{test_maria/3}
    ".
test_maria(M,N,NPart) :- maria(M,N,NPart).

:- test test_maria(M,N,NPart) : (M=2, N=12) => (NPart = 20) + not_fails.
:- test test_maria(M,N,NPart) : (M=3, N=48) => (NPart = 72) + not_fails.
:- test test_maria(M,N,NPart) : (M=4, N=50) => (NPart = 28) + not_fails.

:- test test_maria(M,N,NPart) : (M=3, N=9, NPart = 1) + fails #"Número incorrecto".

% Predicado 2.1: guardar_grafo------------------------------
% Comprobar que se guardan todos los hechos
:- pred test_guardar_grafo(G)
    #"Comprueba que se han guardado todos los hechos de la lista @var{G}
    @includedef{test_guardar_grafo/1}
    ".
test_guardar_grafo(G) :- guardar_grafo(G), test_guardar_grafo_aux(G).
:- pred test_guardar_grafo_aux(G)
    #"Auxiliar para comprobar que se han guardado todos los hechos de la lista @var{G}
    @includedef{test_guardar_grafo_aux/1}
    ".
test_guardar_grafo_aux([]).
test_guardar_grafo_aux([G1|G]) :- G1, test_guardar_grafo_aux(G).

:- test test_guardar_grafo(G) : (G=[arista(a,b), arista(a,c), arista(a,d), arista(e,a), arista(a,f), arista(a,g)]) + not_fails.
:- test test_guardar_grafo(G) : (G=[arista(a,b), arista(a,d), arista(e,f), arista(a,e), arista(c,b)]) + not_fails.
:- test test_guardar_grafo(G) : (G=[arista(a,b), arista(c,b), arista(x,y)]) + not_fails.

% Predicado 2.2: aranya------------------------------
:- pred test_aranya(G)
    #"Comprueba que el grafo @var{G} contiene una araña.
    @includedef{test_aranya/1}
    ".
test_aranya(G) :- guardar_grafo(G), aranya.
:- test test_aranya(G) : (G=[arista(a,b), arista(a,c), arista(a,d), arista(e,a), arista(a,f), arista(a,g)]) + not_fails.
:- test test_aranya(G) : (G=[arista(a,b), arista(a,d), arista(e,f), arista(a,e), arista(c,b)]) + not_fails.
:- test test_aranya(G) : (G=[arista(a,b), arista(a,c), arista(a,d), arista(a,e), arista(b,c), arista(c,e), arista(d,e), 
    arista(d,b)]) + not_fails.

:- test test_aranya(G) : (G=[arista(a,b), arista(c,b), arista(x,y)]) + fails #"Grafo no conexo".
:- test test_aranya(G) : (G=[arista(a,b), arista(a,c), arista(a,d), arista(b,e), arista(b,g), arista(e,f)]) + fails #"Más de un vértice con grado 3 o más".