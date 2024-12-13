# ==============================================================================
# =========        TALLER 2. ANÁLISIS DE REDES SOCIALES      ===================
# =========         Michel Mendivenson Barragpan Zabala      ===================
# =========   Estadística. Universidad Nacional de Colombia  ===================
# ==============================================================================

setwd('UN/1) Análisis de redes sociales (ARS)/2) Workplace/Practice /Taller 2/')
library(igraph)
library(dplyr)

# =========================== PUNTO 3 Y 4 ======================================
# Los puntos no necesitan de código, pero para que los gráficos no se vieran 
# distorsionados ni borrosos se generaron nuevos gráficos
# ==============================================================================

# == Grafo circular:

par(mar = c(0,0,0,0))
g =  make_ring(n = 6, directed = F)
set.seed(1)
plot.igraph(g,
            vertex.color = 'gray', 
            vertex.frame.color = NA, 
            edge.color = 'black',
            vertex.label.color = 'black', 
            vertex.label.cex = 3.5,
            edge.width = 6,
            vertex.size = 38)

# == Grafo de estrella:

g = make_star(n = 6, mode = 'undirected')
set.seed(1)
plot.igraph(g, vertex.color = 'gray',  vertex.frame.color = NA, vertex.label.color = 'black', 
            vertex.label.cex = 3.5, vertex.size = 38, edge.color = 'black', edge.width = 6)

# == Grafo punto 4:

g = make_empty_graph(n = 6, directed = F)
g = add_edges(g, edges = c(1,2,2,5,5,1,1,4,4,3,3,6,6,1))
set.seed(2)
plot.igraph(g, vertex.color = 'gray', vertex.frame.color = NA, vertex.label.color = 'black', 
            vertex.label.cex = 3, vertex.size = 28, edge.color = 'black', edge.width = 5)


# ============================= PUNTO 5 ========================================
# Considerando el conjunto de datos comtrade.RData asociado con el crecimiento anual
# del comercio (Diferencia en doláres en escala logarítmica respecto al año 2000). Este 
# conjunto de datos involucra 30 países, 10 años desde 1996 hasta 2005, y 6 clases de 
# productos diferentes, como se muestra a continuación.
# ==============================================================================

# =====> LECTURA DE DATOS

load('data/comtrade.RData')

# =====> UNA SERIE DE TIEMPO PARA LA MEDIA GLOBAL POR CATEGORÍA (NO ES LO QUE SE PIDE EN EL PUNTO):

par(mar = c(5,4,4,2))
años = as.character(1996:2005)
manufact = list()                   # Asociado al índice 5 de la tercera dimensión
miscellaneous = list()              # Asociado al índice 6 de la tercera dimensión
for (i in años){
  manufact[[i]] = sum(apply(X = comtrade[,,"Manufact goods classified chiefly by material", i],
                            MARGIN = c(1,2), FUN = mean), na.rm = T)/(30*29)
  miscellaneous[[i]] = sum(apply(X = comtrade[,,"Miscellaneous manufactured articles", i], 
                                 MARGIN = c(1,2), FUN = mean), na.rm = T)/(30*29)
}
manufact = unlist(manufact); miscellaneous = unlist(miscellaneous)

plot(x = as.numeric(names(miscellaneous)), y = miscellaneous, type = 'b', col = 'aquamarine4', pch = 16, 
  xlab = "Años",  ylab = "Valores", main = "Manufact y Miscellaneous a través de los años",
  ylim = c(-0.10, 0.2)) 
lines( x = as.numeric(names(manufact)),  y = manufact, type = 'b',col = 'coral4', pch = 17)
text(x = as.numeric(names(miscellaneous)), y = miscellaneous + 0.012, labels = round(miscellaneous, 2), 
  pos = 3, cex = 0.8, col = "blue")
text(x = as.numeric(names(manufact)),  y = manufact - 0.022, labels = round(manufact, 2), 
  pos = 3,  cex = 0.8, col = "red")
legend( "topleft", legend = dimnames(comtrade)[[3]][5:6], col = c("aquamarine4", "coral4"), 
  pch = c(16, 17), lty = 1, bty = 'n')


# ======> LO SOLICITADO EN EL PUNTO:

Y = apply(X = comtrade [ , , c (5 ,6) ,] , MARGIN = c (1 ,2) , FUN = mean)   # Matriz de adyacencia Y


# ======> (A) CALCULE LA MEDIA GLOBAL DE Y.

n = nrow(Y)
media = (1/(n * (n-1)) * sum(Y, na.rm = T))

# ======> (B) CALCULE LA MEDIA GLOBAL POR FILA (GRAFIQUE).
 
mFila = apply(MARGIN=1, X = Y, FUN = function(x) sum(x, na.rm = T)/29)

# == Se grafica con un histograma.

hist(mFila, probability = T,
     col = 'coral1',
     border = 'coral1', 
     breaks = 20, xlab = 'Media global',
     ylab = 'Densidad', main = 'Histograma (Sociabilidad por país)')
mtext('Respecto a la media global de las categorías Miscellaneous \n manufactured articles y Manufact goods classified chiefly \n by material  a lo largo de 1996-2005',
      side = 3, cex = 0.83, line = -3, adj = 0.9)
mFilaD = density(mFila)
lines(mFilaD, col = 'aquamarine4')

# ======> (C) CALCULE LA MEDIA GLOBAL POR COLUMNA (GRAFIQUE).

mCol = apply(MARGIN=2, X = Y, FUN = function(x) sum(x, na.rm = T)/29)

# == Se grafica en un histograma.

hist(mCol, probability = T,
     col = 'coral1',
     border = 'coral1', 
     breaks = 20, xlab = 'Media global',
     ylab = 'Densidad', main = 'Histograma (Popularidad por país)')
mtext('Respecto a la media global de las categorías Miscellaneous \n manufactured articles y Manufact goods classified chiefly \n by material  a lo largo de 1996-2005',
      side = 3, cex = 0.83, line = -3, adj = 0.9)
mColD = density(mFila)
lines(mColD, col = 'aquamarine4')

# ======> (D) CALCULAR LA MEDIA DE LAS MEDIA POR COLUMNA Y POR FILA.

mediaFila = mean(mFila)
mediaCol = mean(mCol)

# ======> (E) CALCULAR LA DESVIACIÓN ESTÁNDAR POR COLUMNA Y POR FILA.

sdFila = sd(mFila)
sdCol = sd(mCol)

# ======> (F) CALCULAR EL COEFICIENTE DE CORRLEACIÓN DE LAS MEDIAS POR COLUMNA Y POR FILA.
#             REALICE UN DISPERSOGRAMA

corrMedia = cov(mFila, mCol)/(sdFila * sdCol)


# == PRESENTACIÓN DE RESULTADOS

results = rbind(c(mediaFila, mediaCol),
                c(sdFila,sdCol),
                c(corrMedia, NA))
colnames(results) = c('Sociabilidad', 'Popularidad')
rownames(results) = c('Media', 'Desviación estándar', 'Correlación')

# xtable::xtable(x = results, digits = 4)

## == Dispersograma

A = cbind('Sociabilidad' = mFila, 'Popularidad' = mCol)
plot(A, col = 'coral3', pch = 20,
     main = 'Popularidad vs. Sociabilidad')
lines(x = c(1,-1), y = c(1,-1), col = 'aquamarine4', lty = 'dotdash')
mtext(bquote(bold('La línea punteada representa y = x')),
      col = adjustcolor(col = 'darkgray', alpha = 1),
      side = 1, cex = 0.83, line = -1.1, adj = 0.98)

# == Identificando los puntos más sociables y más populares.

A = as.data.frame(A) |> 
  arrange(-Sociabilidad)

points(x = A$Sociabilidad[1:3], y = A$Popularidad[1:3], cex = 2, col = 'darkred')
text(x = A$Sociabilidad[1:3],  y = A$Popularidad[1:3] - 0.015, labels = rownames(A[1:3,]), 
     pos = 3,  cex = 0.8, col = "darkred")

A = A |> 
  arrange(-Popularidad)

points(x = A$Sociabilidad[2], y = A$Popularidad[2], cex = 2, col = 'darkred')
text(x = A$Sociabilidad[2],  y = A$Popularidad[2] - 0.015, labels = rownames(A[2,]), 
     pos = 3,  cex = 0.8, col = "darkred")

# ============================= PUNTO 6 ========================================
# Considere el conjunto de datos dado en conflict.RData recopilado por Mike Ward 
# y Xun Cao del departamento de Ciencias Políticas de la Universidad de Washington, 
# asociado con datos de conflictos entre países en los años 90. El archivo 
# conflict.RData contiene una lista con tres arreglos, X, Y, y D. X tiene tres 
# campos: population (población en millones), gdp (PIB en millones de dolares),
# polity (puntuación política, un índice de democracia). Y hace referencia a una 
# matriz Y = [yi,j ] en la que yi,j representa el número de conflictos iniciados 
# por el país i hacia el país j. Finalmente, D es un arreglo de tres dimensiones
# dimensionescuya tercera dimensión contiene indices entre cada par de países
# asociados con: comercio (dimensión 1), importaciones (dimensión 2), organizaciones
# intergubernamentales (dimensión 3), y distancia geográfica (dimensión 4).
# ==============================================================================

# ======> CARGANDO LOS DATOS

load('data/conflict.RData')

## ======> (A) HACER UNA VISUALIZACIÓN DECORADA DE LA RED DE CONFLICTOS

# == Grafo inicial

g = graph_from_adjacency_matrix(dat$Y)
set.seed(1305)
layout = layout_with_fr(g)                                    # Layout general 
par(mar = c(3,0,3,0))
plot.igraph(g, edge.arrow.size = 0.35, layout = layout,
            vertex.color = adjustcolor('darkslategray4', 0.4), 
            vertex.frame.color = 'darkslategray4', 
            edge.color = adjustcolor('darkgray', alpha = 0.95),
            vertex.label = NA, 
            vertex.size = 2.9,
            edge.width = 1.8,
            edge.curved = 0)
title(main = 'GUERRAS INICIADAS ENTRE PAÍSES', family = 'courier', font = 2.5, col = 'black', cex.main = 2)
mtext(text = 'Una arista se corresponde a \n el país x inició una guerra con el país y', 
      family = 'courier', cex = 1.1, font = 2, col = 'black', side = 1, padj = 1, adj = 0.5)


# == INCLUSIÓN DE ATRIBUTOS NODALES:

V(g)$gdp = dat$X[, "gdp"]                                               # PIB de cada país
V(g)$population = dat$X[, "population"]                                 # Población de cada país
V(g)$polity = dat$X[, "polity"]                                         # Puntuación política de cada país
V(g)$degree = degree(g)                                                 # Grado total
V(g)$degreein = degree(g, mode = 'in')                                  # Grado de entrada
V(g)$degreeout = degree(g, mode = 'out')                                # Grado de salida
# g = subgraph(g, V(g)[V(g)$degree != 0])                               # Eliminar los vértices desconectados
V(g)$labels = names(V(g))                                               # Nombres de los países
# Nombres top 5 por grado 
V(g)$Top5Degree = ifelse(V(g)$labels %in% names(sort(-degree(g)))[1:5], 
                         V(g)$labels, 
                         NA)
# Nombres top 5 grado de entrada
V(g)$Top5DegreeIn = ifelse(V(g)$labels %in% names(sort(-degree(g, mode = 'in')))[1:5], 
                           V(g)$labels, 
                           NA)
# Nombres top 5 grado de salida
V(g)$Top5DegreeOut = ifelse(V(g)$labels %in% names(sort(-degree(g, mode = 'out')))[1:5], 
                            V(g)$labels, 
                            NA)
# Nombres top 5 PIB
V(g)$Top5PIB = ifelse(V(g)$labels %in% names(sort(-dat$X[,"gdp"]))[1:5], 
                      V(g)$labels, 
                      NA)
# Nombres top 5 población
V(g)$Top5Population = ifelse(V(g)$labels %in% names(sort(-dat$X[,"population"]))[1:5], 
                             V(g)$labels, 
                             NA)
# Nombres top 5 mejores puntajes de democracia
V(g)$Top5Polity = ifelse(V(g)$labels %in% names(sort(-dat$X[,"polity"]))[1:5], 
                         V(g)$labels, 
                         NA)
# Nombres top 5 peores puntaje de democracia
V(g)$Top5Polity = ifelse(V(g)$labels %in% names(sort(dat$X[,"polity"]))[1:5], 
                         V(g)$labels, 
                         V(g)$Top5Polity)


# == GRÁFICOS POR POPULARIDAD Y SOCIABILIDAD
par(mar = c(3,0,3,0), mfrow = c(1,2))
plot.igraph(g, layout = layout,
            vertex.color = ifelse(!is.na(V(g)$Top5DegreeIn), adjustcolor('coral3', alpha = 0.4), adjustcolor('darkslategray4', 0.4)), 
            vertex.frame.color = ifelse(!is.na(V(g)$Top5DegreeIn), 'coral3', 'darkslategray4'), 
            vertex.label = V(g)$Top5DegreeIn, 
            vertex.label.color = adjustcolor('black', alpha = 0.7),
            vertex.label.cex = 0.9,
            vertex.label.font = 2,
            vertex.label.family = 'courier',
            vertex.size = V(g)$degreein/2,
            edge.color = adjustcolor('darkgray', alpha = 0.95),
            edge.width = 1,
            edge.arrow.size = 0.18,
            edge.curved = 0)
title(main = 'POPULARIDAD', family = 'courier', font = 2.5, col = 'black', cex.main = 2, line = -1)
mtext(text = 'El tamaño de cada nodo se corresponde con la \n cantidad de guerras iniciadas hacia un país.', 
      family = 'courier', cex = 0.9, font = 2, col = 'black', side = 1)

plot.igraph(g, layout = layout,
            vertex.color = ifelse(!is.na(V(g)$Top5DegreeOut), adjustcolor('coral3', alpha = 0.4), adjustcolor('darkslategray4', 0.4)), 
            vertex.frame.color = ifelse(!is.na(V(g)$Top5DegreeOut), 'coral3', 'darkslategray4'), 
            vertex.label = V(g)$Top5DegreeOut, 
            vertex.label.color = adjustcolor('black', alpha = 0.7),
            vertex.label.cex = 0.9,
            vertex.label.font = 2,
            vertex.label.family = 'courier',
            vertex.size = V(g)$degreeout/2,
            edge.color = adjustcolor('darkgray', alpha = 0.95),
            edge.width = 1,
            edge.arrow.size = 0.18,
            edge.curved = 0)
title(main = 'SOCIABILIDAD', family = 'courier', font = 2.5, col = 'black', cex.main = 2, line = -1)
mtext(text = 'El tamaño de cada nodo se corresponde con la \n cantidad de guerras iniciadas por un país.', 
      family = 'courier', cex = 0.9, font = 2, col = 'black', side = 1)


# == GRÁFICOS CON VARIABLES NODALES

par(mar = c(3,0,3,0), mfrow = c(1,3))
# set.seed(1305)
# layout = layout_with_fr(g)

# Utilizando el PIB

labels = V(g)$labels
labels[V(g)$degree != 0] = NA
labels = ifelse(labels %in% names(sort(-dat$X[,"gdp"][V(g)$degree == 0]))[1:3], labels, NA) # Etiquetas para países desconectados
labels[!is.na(V(g)$Top5PIB)] = V(g)$Top5PIB[!is.na(V(g)$Top5PIB)]                           # Etiquetas para países conectados

plot.igraph(g, layout = layout,
            vertex.color = ifelse(!is.na(V(g)$Top5PIB), adjustcolor('coral3', alpha = 0.4), adjustcolor('darkslategray4', 0.4)), 
            vertex.frame.color = ifelse(!is.na(V(g)$Top5PIB), 'coral3', 'darkslategray4'), 
            vertex.label = labels, 
            vertex.label.color = adjustcolor('black', alpha = 0.7),
            vertex.label.cex = 0.9,
            vertex.label.font = 2,
            vertex.label.family = 'courier',
            vertex.size = sqrt(V(g)$gdp/10), # 20 * V(g)$gdp/max(V(g)$gdp - 4000),
            # vertex.label.dist = 0.5,      # Modificar la distancia radial de la etiqueta en relación con el nodo
            edge.color = adjustcolor('darkgray', alpha = 0.95),
            edge.width = 1,
            edge.arrow.size = 0.2,
            edge.curved = 0)
title(main = 'Producto Interno Bruto', family = 'courier', font = 2.5, col = 'black', cex.main = 2, line = -1)
mtext(text = 'El tamaño de cada nodo se corresponde con \n el PIB en millones de doláres.', padj = 0.8,
      family = 'courier', cex = 0.8, font = 2, col = 'black', side = 1)


plot.igraph(g, layout = layout,
            vertex.color = ifelse(!is.na(V(g)$Top5Population), adjustcolor('coral3', alpha = 0.4), adjustcolor('darkslategray4', 0.4)), 
            vertex.frame.color = ifelse(!is.na(V(g)$Top5Population), 'coral3', 'darkslategray4'), 
            vertex.label = V(g)$Top5Population, 
            vertex.label.color = adjustcolor('black', alpha = 0.7),
            vertex.label.cex = 0.9,
            vertex.label.font = 2,
            vertex.label.family = 'courier',
            vertex.size = sqrt(V(g)$population),
            vertex.label.offset = 10, 
            edge.color = adjustcolor('darkgray', alpha = 0.95),
            edge.width = 1,
            edge.arrow.size = 0.2,
            edge.curved = 0)
title(main = 'Población', family = 'courier', font = 2.5, col = 'black', cex.main = 2, line = -1)
mtext(text = 'El tamaño de cada nodo se corresponde con \n la población de cada país.', padj =0.8,
      family = 'courier', cex = 0.8, font = 2, col = 'black', side = 1)

colores = ifelse(!is.na(V(g)$Top5Polity), adjustcolor('coral3', alpha = 0.4), adjustcolor('darkslategray4', 0.4))
colores = ifelse(V(g)$polity > 0 & !is.na(V(g)$Top5Polity), adjustcolor('olivedrab4', alpha = 0.4), colores)
plot.igraph(g, layout = layout,
            vertex.color = colores, 
            vertex.frame.color = adjustcolor(colores, alpha = 1), 
            vertex.label = V(g)$Top5Polity, 
            vertex.label.color = adjustcolor('black', alpha = 0.7),
            vertex.label.cex = 0.9,
            vertex.label.font = 2,
            vertex.label.family = 'courier',
            vertex.size = 8 * abs(V(g)$polity/max(V(g)$polity)),
            vertex.label.offset = 10, 
            edge.color = adjustcolor('darkgray', alpha = 0.95),
            edge.width = 1,
            edge.arrow.size = 0.2,
            edge.curved = 0)
title(main = 'Puntuación política', family = 'courier', font = 2.5, col = 'black', cex.main = 2, line = -1)
texto = paste0('El tamaño de cada nodo se corresponde con\n',
               'el valor absoluto del puntaje político de cada país')
mtext(text = texto, padj = 0.8,
      family = 'courier', cex = 0.8, font = 2, col = 'black', side = 1)


# ======> (B) CALCULE LA MEDIA GLOBAL: 
n = nrow(dat$Y)
mediaConflict = sum(dat$Y)/(n * (n - 1)) # La diagonal de la matriz Y son sólo ceros
cat('La media global de la matriz de adyacencia relacionada a los conflictos es:', mediaConflict)

# ======> (C) GRAFIQUE LA DISTRIBUCIÓN DE LOS GRADOS DE SALIDA Y DE ENTRADA, LA MEDIA Y LA DESVIACIÓN ESTÁNDAR

mediaIn = mean(V(g)$degreein)
mediaOut = mean(V(g)$degreeout)
sdIn = sd(V(g)$degreein)
sdOut = sd(V(g)$degreeout)

# == Si bien la función de igraph (degree) no calcula como tal la suma de la fuerza,
#    sino que en cambio calcula la suma de las salidas que existen desde un nodo en
#    específico como la matriz de adyacencia se tomó directamente de dat$Y en este caso
#    se corresponde con el in degree, el out degree y el degree total. 

par(mfrow = c(1,2), mar = c(5,4.5,4,1))
plot(table(factor(V(g)$degreein, levels = 0:130))/130, type = 'h' ,
     col = 'coral1', lwd = 5, xlab = 'Degree in',
     ylab = 'Densidad', main = '',  xlim = c(0,42), ylim = c(0, 0.5))
title(main = 'Popularidad por país',
     family = 'courier', font = 2, cex.main = 1.5, sub = 'Cantidad de guerras iniciadas contra un país')
texto <- paste0('Media:', sprintf('%1.4f', mediaIn), '\n',
                'Desviación:', sprintf('%1.4f', sdIn))
# densityIn = density(V(g)$degreein)
# lines(densityIn, col = 'aquamarine4')
# box(lty = 'dashed', which = 'plot')
abline(v = seq(0,40,5),col = adjustcolor('darkgray', alpha = 0.9),lty = 'dashed')
abline(h = seq(0,0.55,0.05),col = adjustcolor('darkgray', alpha = 0.9),lty = 'dashed')
mtext(texto, side = 3, cex = 1, line = -2, adj = 0.98, family = 'courier', font = 2, col = 'black')

par(mar = c(5.1,1,4.1,2.1))

plot(table(factor(V(g)$degreeout, levels = 0:130))/130, type = 'h',
     col = 'coral1', xlab = 'Degree out', lwd = 5,
     ylab = '', main = '', xlim = c(0,41), ylim = c(0, 0.5), yaxt = 'n')
title(main = 'Sociabilidad por país',
      family = 'courier', font = 2, cex.main = 1.5, sub = 'Cantidad de guerras iniciadas por un país')
texto <- paste0('Media:', sprintf('%1.4f', mediaOut), '\n',
                'Desviación:', sprintf('%1.4f', sdOut))

# densityOut = density(V(g)$degreeout)
# lines(densityOut, col = 'aquamarine4')
# box(lty = 'dashed', which = 'plot')
abline(v = seq(0,40,5),col = adjustcolor('darkgray', alpha = 0.9),lty = 'dashed')
abline(h = seq(0,0.55,0.05),col = adjustcolor('darkgray', alpha = 0.9),lty = 'dashed')
mtext(texto, side = 3, cex = 1, line = -2, adj = 0.98, family = 'courier', font = 2, col = 'black')


# ======> (D) REALICE UN DISPERSOGRAMA DE LOS GRADOS DE SALIDA CONTRA LOS GRADOS DE ENTRADA: 

par(mar = c(5,4,4,2), mfrow = c(1,1))

A = cbind('Sociabilidad' = V(g)$degreeout, 'Popularidad' = V(g)$degreein)
rownames(A) = V(g)$labels
plot(A, col = 'coral3', pch = 20,
     main = 'Popularidad vs. Sociabilidad', xlim = c(0,40))
lines(x = c(50,-30), y = c(50,-30), col = 'aquamarine4', lty = 'dotdash', lwd = 1.5)
# mtext(bquote(bold('La línea punteada representa y = x')),
#       col = adjustcolor(col = 'darkgray', alpha = 1),
#       side = 1, cex = 0.83, line = -1.1, adj = 0.98)


A = as.data.frame(A) |> 
  arrange(-Sociabilidad)

points(x = A$Sociabilidad[1:7], y = A$Popularidad[1:7], cex = 2, col = 'darkred')
text(x = A$Sociabilidad[1:7],  y = A$Popularidad[1:7] - 3.2, labels = rownames(A[1:7,]), 
     pos = 3,  cex = 0.8, col = "darkred", font = 2, family = 'courier')

abline(v = seq(0,40,5),col = adjustcolor('darkgray', alpha = 0.9),lty = 'dashed')
abline(h = seq(0,40,5),col = adjustcolor('darkgray', alpha = 0.9),lty = 'dashed')

corrPaises = cov(V(g)$degreein, V(g)$degreeout)/(sdIn * sdOut)
mtext(paste0('Correlación: ', round(corrPaises, 4)), side = 3, cex = 1, line = -1.1, 
      adj = 0.02, family = 'courier', font = 2, col = 'black')

# ======> (E) IDENTIFIQUE LOS PAÍSES MÁS ACTIVOS


Top10Soc = head(A, n = 10)
Top10Pop = A |> 
  arrange(-Popularidad) |> 
  head(., n = 10)
# xtable::xtable(Top10Soc)
# xtable::xtable(Top10Pop)



# ============================= PUNTO 8 ========================================
# Considere los datos relacionales acerca de los conflictos internacionales del 
# archivo conflict.RData despúes de simetrizarla débilmente y remover los nodos 
# aislados:
# ==============================================================================

G = subgraph(g, vids = V(g)[V(g)$degree != 0])  # Quitar nodos aislados
G = as_undirected(G, mode = 'collapse')         # Simetrizar débilmente la red (Ver ?as_undirected)
Pesos = c()                                     # El peso de las aristas es la cantidad de guerras entre países.
for (i in E(G)){
  Pesos = c(Pesos, 
            dat$Y[get.edgelist(G)[i, 1], get.edgelist(G)[i, 2]] + 
            dat$Y[get.edgelist(G)[i, 2], get.edgelist(G)[i, 1]])
}
E(G)$weight = Pesos

# ======> (A) HACER UNA VISUALIZACIÓN DECORADA DE LA RED:

# == Creación de grupos para cada vértice en base al índice de democracia

V(G)$Faction = ifelse(V(G)$polity > 0, 'Positive', 'Negative')
Positive = V(G)[Faction == 'Positive']
Negative = V(G)[Faction == 'Negative']
V(G)$Faction[V(G)$polity == 0] = 'Zero'
E(G)$color = 'black'
E(G)[Negative %--% Negative]$color = adjustcolor('coral3', alpha = 0.9)
E(G)[Negative %--% Positive]$color = adjustcolor('darkgray', alpha = 0.7)
E(G)[Positive %--% Positive]$color = adjustcolor('olivedrab4', alpha = 0.9)
V(G)$color = adjustcolor('darkslategray4', alpha = 0.55)
V(G)$color[V(G)$polity > 0] = adjustcolor('olivedrab4', alpha = 0.55)
V(G)$color[V(G)$polity < 0] = adjustcolor('coral3', alpha = 0.55)

# == GRÁFICO DECORADO

faccionesG = G
E(faccionesG)$weight = 5
for(i in unique(V(faccionesG)$Faction)[1:2]) {
  GroupV = which(V(G)$Faction == i)
  faccionesG = add_edges(faccionesG, combn(GroupV, 2)[1:770], attr=list(weight=5.01))
}
# La solución fue tomada de aquí: https://stackoverflow.com/questions/37378744/igraph-grouped-layout-based-on-attribute#52672660

rotate_layout <- function(layout, theta) {
  rotation_matrix <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow = 2)
  t(rotation_matrix %*% t(layout))
}

set.seed(1)
layout_faction = layout_with_graphopt(faccionesG)                       # Posición general
layout_faction = rotate_layout(layout_faction, - 1.35 * pi/4)           # La posición de todos los nodos se rota
# Se separan los nodos de democracia positiva y los de democracia negativa un poco más
layout_faction[which(V(G)$Faction == 'Positive'), 1] = layout_faction[which(V(G)$Faction == 'Positive'), 1] - 25
nombres = ifelse(V(G)$labels %in% names(sort(-degree(G)))[1:10],        # Se asignarán nombres al top 10 de países que han entrado en guerra con más países
                 V(G)$labels, NA)
nombres[V(G)$Faction == 'Zero'] = V(G)$labels[V(G)$Faction == 'Zero']   # Y al país con índice de democracia cero.

par(mar = c(0,0,3,8), mfrow = c(1,1))
plot.igraph(G, layout = layout_faction, 
            vertex.size = 4.5 * sqrt(degree(G)),
            vertex.label = nombres,
            vertex.label.cex = 0.8,
            vertex.label.family = 'courier',
            vertex.label.font = 2,
            vertex.label.color = 'black',
            vertex.frame.color = adjustcolor(V(G)$color, alpha = 1),
            vertex.frame.width = 1.5,
            edge.width = E(G)$weight/1.5, edge.curve = 0,
            edge.color = adjustcolor(E(G)$color, alpha = 0.7),
            xpd = T)
title(main = 'Grupos por índice de democracia',
      family = 'courier', font = 2, col = 'black', cex.main = 1.5)
clue = c('Índice de democracia > 0',
         'Índice de democracia > 0',
         'Índice de democracia = 0')
par(family = 'courier', font = 2)
legend(x = 1.2, y = 1.1, 
       title = 'Grupos',
       title.font = 2,
       title.adj = 0.8,
       col = unique(adjustcolor(V(G)$color, alpha = 1)),
       legend = clue, 
       pch = 19, 
       cex = 0.8,
       text.font = 1,
       bty = 'n')

clue = c('Negativo - Negativo',
         'Negativo - Positivo',
         'Positivo - Positivo')
legend(x = 1.3, y = 0.7, 
       title = 'Relaciones',
       title.font = 2,
       title.adj = 0.8,
       col = unique(adjustcolor(E(G)$color, alpha = 1)),
       legend = clue, 
       pch = '',
       lty = 'solid',
       cex = 0.8,
       text.font = 1,
       bty = 'n')
texto = paste0('La agrupación mostrada en la\n',
               'ubicación de los nodos es\n',
               'artificialmente creada. Es\n',
               'decir, no debe tomarse como una\n',
               'comunidad generada por cercanía\n',
               'ni nada que se le parezca.', sep = '')
mtext(texto, cex = 0.8, side = 1, line = -7, padj = 1, adj = 0.98,
      font = 2, las = 1)

# Ahí una diferencia en el grado por calificación política

A = c()
A = rbind(A, c('N - N',sum(E(G)$color == adjustcolor('coral3', alpha = 0.9))/1.6))
A = rbind(A, c('P - N',sum(E(G)$color == adjustcolor('darkgray', alpha = 0.7))/1.6))
A = rbind(A, c('P - P', sum(E(G)$color == adjustcolor('olivedrab4', alpha = 0.9))/1.6))
colnames(A) = c('Tipo de relación', 'Porcentaje')
# xtable::xtable(A)

# == Con el fin de revisar si el índice de democracia tiene alguna correlación con
#    la formación de guerras se calcula la asortatividad tanto nominal como cuantitativa.
#    El país de índice cero se toma como positivo para no tener en cuenta una 
#    categoría con un sólo individuo.

facciones = table(V(G)$Faction)
# xtable::xtable(facciones)
V(G)$Faction[V(G)$Faction == 'Zero'] = 'Positive'
Cualitativa = assortativity_nominal(G, types = ifelse(V(G)$Faction == 'Positive', 1, 2), normalized = T, directed = F)
Cuantitativa = assortativity(G, values = V(G)$polity, normalized = T, directed = F)
Asortatividad = rbind('Cualitativa' = Cualitativa,
                     'Cuantitativa' = Cuantitativa)
colnames(Asortatividad) = 'Asortatividad'
xtable::xtable(Asortatividad)


# == (B) Caracterización estructural y local. 

# Cliques
tabla = table(sapply(X = cliques(graph = g, min = 1, max = 10), FUN = length))
# xtable::xtable(t(tabla), caption = 'Frecuencia de tamaño de cliques')
clique_num(G)
count_max_cliques(G)

# Densidad y otras medidas
medidas = c()
componentes = decompose(G)
GC = decompose(G)[[1]]
medidas = rbind(medidas,
                c('Densidad',edge_density(G)), 
                c('Transitividad global', transitivity(G, type = 'global')),
                c('Compente gigante', max(sapply(X = componentes, FUN = vcount))),
                c('Puntos de articulación', vertex_connectivity(GC)))
# xtable::xtable(medidas)

# Transitividad local y medidas de centralidad

A = matrix(NA, nrow = 91, ncol = 5)
colnames(A) = c('Grado', 'Transitividad local', 'Cercanía', 'Intermediación', 'Propia')
rownames(A) = NULL

A[, "Grado"] = degree(G)
A[, "Transitividad local"] = transitivity(G, type = 'local')
A[, "Cercanía"] = closeness(G)
A[, "Intermediación"] = betweenness(G)
A[, "Propia"] = eigen_centrality(G)$vector

par(mar = c(5,4,4,2), family = '', font = 1)
plot(table(factor(A[,"Grado"], levels = 1:91))/91,type = 'h' ,
     col = 'coral1', lwd = 5, xlab = 'Grado',
     ylab = 'Densidad', main = '',  xlim = c(0,30))
title(main = 'Popularidad por país',
      family = 'courier', font = 2, cex.main = 1.5, sub = 'Cantidad de guerras iniciadas contra un país')
texto <- paste0('Media:', sprintf('%1.4f', mean(A[,"Grado"])), '\n',
                'Desviación:', sprintf('%1.4f', sd(A[, "Grado"])))
abline(v = seq(0,40,5),col = adjustcolor('darkgray', alpha = 0.9),lty = 'dashed')
abline(h = seq(0,0.55,0.05),col = adjustcolor('darkgray', alpha = 0.9),lty = 'dashed')
mtext(texto, side = 3, cex = 1, line = -2.5, adj = 0.98, family = 'courier', font = 2, col = 'black')

# xtable::xtable(as.matrix(abs(sort(-degree(G))[1:5])))

par(mfrow = c(2,2))
for (i in colnames(A)[2:5]){
  datos = A[!is.na(A[,i]),i]
  hist(datos, probability = T,
       col = 'coral1',
       border = 'coral1', 
       breaks = 20, xlab = '',
       ylab = 'Densidad', main = i, cex.main = 1.8)
  texto <- paste0('Media:', sprintf('%1.4f', mean(datos, na.rm = T)), '\n',
                  'Desviación:', sprintf('%1.4f', sd(datos, na.rm = T)))
  mtext(texto, side = 3, cex = 0.8, line = -2.5, adj = 0.9, family = 'courier', font = 2, col = 'black')
  lines(density(datos), col = 'aquamarine4')
}

# ============================= PUNTO 7 ========================================
# Para todos los vértices de los cuatro grafos que se presentan a continuación, 
# calcular el grado y las medidas de centralidad. Para cada grafo completar e
# interpretar la siguiente tabla. Interpretar los resultados.
# ==============================================================================

# == Se guardan todos los grafos en una lista para simplicidad de cálculo de las
#    medidas solicitadas.

Punto7 = list()
empty = make_empty_graph(n = 5, directed = F)
Punto7[[1]] = make_star(n = 5, mode = 'undirected')
Punto7[[4]] = igraph::make_ring(n = 5)
Punto7[[2]] = add_edges(empty, edges =  c(1,2,
                                          2,3,
                                          3,4,
                                          4,5))
Punto7[[3]] = add_edges(empty, c(1,2,
                                 2,3,
                                 3,4,
                                 3,5))

# == GRAFO
par(mfrow = c(1,4), 
    mar = c(1,1,3,1))
set.seed(1305)
for (i in 1:4){
  plot(Punto7[[i]], 
       vertex.label = NA,
       vertex.color = 'black',
       vertex.size = 5,
       edge.width = 3,
       main = paste0('Grafo ', i)) 
}


media = matrix(NA, ncol = 4, nrow = 4)
colnames(media) = c('Grado', 'Cercanía', 'Intermediación', 'Propia')
rownames(media) = paste('Grafo', seq(1:4))

Desv = matrix(NA, ncol = 4, nrow = 4)
colnames(Desv) = c('Grado', 'Cercanía', 'Intermediación', 'Propia')
rownames(Desv) = paste('Grafo', seq(1:4))

for (i in 1:4){
  media[i, "Grado"] = mean(degree(Punto7[[i]]))
  Desv[i, "Grado"] = sd(degree(Punto7[[i]]))
  media[i, "Cercanía"] = mean(closeness(Punto7[[i]]))
  Desv[i, "Cercanía"] = sd(closeness(Punto7[[i]]))
  media[i, "Intermediación"] = mean(betweenness(Punto7[[i]]))
  Desv[i, "Intermediación"] = sd(betweenness(Punto7[[i]]))
  media[i, "Propia"] = mean(eigen_centrality(Punto7[[i]])$vector)
  Desv[i, "Propia"] = sd(eigen_centrality(Punto7[[i]])$vector)
}

# xtable::xtable(media)
# xtable::xtable(Desv)
# 100 * Desv/media