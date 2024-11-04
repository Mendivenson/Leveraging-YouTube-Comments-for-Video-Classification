# La idea general es: 
#   - Usando el método Ergos Renyi generar una matriz de adyacencia para una red
#     binaria.
#   - Obtener la vectorización de la matriz.
#   - Obtener el vector de "localizaciones" de la matriz.
#   - Obtener a partir del vector de "localizaciones" la vectorización de la matriz A.
#   - Obtener la matriz de adyacencia a partir de la vectorización.

library(ARS)                                            # Librería de autoría propia
set.seed(1305)
A = ErgosRenyi(n= 10, symmetrical = FALSE, prob = 0.3)  # Creación de la matriz de adyacencia (No dirigida)
Avec = vec(A, symmetrical = FALSE)                      # Vectorización de la matriz
Aloc = which(Avec != 0)                                 # vector de localizaciones de la vectorización

G = igraph::graph_from_adjacency_matrix(A, mode = 'directed')

# En igraph, plot.igraph es como un enmascaramiento del plot normal de R. 
# vertex. *** Para los atributos de los nodos, edge. *** para los atributos de
# las aristas.

# Ahora bien, ya tenemos un grafo, una forma de verificar que las operaciones 
# inversas se realizan bien es generar una red con la matriz obtenida desde Aloc
# y compararla con la red que ya tenemos G.

A1Vec = locToVec(x = Aloc, n = 10, symmetrical = F)    # Vectorización a partir de localizaciones
A1 = vecToA(x = A1Vec, n = 10, symmetrical = F)        # Matriz de adyacencia a partir de vectorización

# Verifiquemos que se obtiene la misma red:
G1 = igraph::graph_from_adjacency_matrix(A1, mode = 'directed') 

par(mfrow = c(1,2))
set.seed(1305); igraph::plot.igraph(G, edge.arrow.size = 0.1,
                                    vertex.color = 'gray', 
                                    vertex.frame.color = NA,
                                    edge.color = 'black',
                                    vertex.label.color = 'black', 
                                    edge.width = 1.5,
                                    main = 'Grafo generado por Ergos-Reyin')
set.seed(1305); igraph::plot.igraph(G1, edge.arrow.size = 0.1,
                                    vertex.color = 'gray', 
                                    vertex.frame.color = NA,
                                    edge.color = 'black',
                                    vertex.label.color = 'black', 
                                    edge.width = 1.5,
                                    main = 'Grafo generado a partir \n del vector de localizaciones')

# Efectivamente, obtenemos la misma red.

# Notas: - Se puede obtener la misma matriz desde el vector de localizaciones directamente.
#        - Falta la implementación para redes bipartitas.