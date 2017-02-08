## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(width=60)
knitr::opts_chunk$set(comment = "", warning = FALSE, message = FALSE, echo = TRUE, tidy = TRUE, size="small")

## ------------------------------------------------------------------------
datos = read.csv("./data.csv", header = TRUE)

## ----message=FALSE-------------------------------------------------------
library(ggplot2)
library(devtools)
library(reshape) 
library(ggbiplot)
library(rgl)  
library(GGally)

## ----message=FALSE-------------------------------------------------------
library(outliers)
library(EnvStats)

## ----message=FALSE-------------------------------------------------------
library(mvoutlier)       
library(CerioliOutlierDetection)
library(robustbase)
library(mvnormtest)   
library(MASS) 

## ----message=FALSE-------------------------------------------------------
library(DMwR)
library(cluster)

## ------------------------------------------------------------------------
MiPlot_Univariate_Outliers = function (datos, indices_de_Outliers, titulo){
  numero.de.datos = nrow(as.matrix(datos))
  vectorTFoutliers =  rep(FALSE, numero.de.datos)
  vectorTFoutliers[indices_de_Outliers] = TRUE
  vector.colores.outlier = rep("black", numero.de.datos)
  vector.colores.outlier [vectorTFoutliers] = "red"
  
  #cat("\nN?mero de datos: ")
  #cat(numero.de.datos)
  #cat("\n?Qui?n es outlier?: ")
  #cat(vectorTFoutliers)
  #cat('\n')
  
  x11()
  plot(datos, col=vector.colores.outlier, main = titulo)
}

## ------------------------------------------------------------------------
vector_es_outlier_IQR = function (datos, indice.de.columna, coef = 1.5){
  columna.datos = datos[,indice.de.columna]
  cuartil.primero = quantile(columna.datos)[2]  #quantile[1] es el m?nimo y quantile[5] el m?ximo.
  cuartil.tercero = quantile(columna.datos)[4] 
  iqr = cuartil.tercero - cuartil.primero
  extremo.superior.outlier = (iqr * coef) + cuartil.tercero
  extremo.inferior.outlier = cuartil.primero - (iqr * coef)
  es.outlier  = columna.datos > extremo.superior.outlier |
    columna.datos < extremo.inferior.outlier
  return (es.outlier)
}

## ------------------------------------------------------------------------
Nombres_de_Filas = function (datos, vector_TF_datos_a_incluir) {
  numero.de.filas = nrow(datos)
  
  if (is.null(row.names(datos)))
    row.names(datos) = rep(1:numero.de.filas)
  
  nombres.de.filas = rep("", numero.de.filas)
  nombres.de.filas[vector_TF_datos_a_incluir==TRUE] = row.names(datos)[vector_TF_datos_a_incluir==TRUE]
  nombres.de.filas
}

## ------------------------------------------------------------------------
vector_claves_outliers_IQR = function(datos, indice, coef = 1.5){
  columna.datos = datos[,indice]
  vector.de.outliers = vector_es_outlier_IQR(datos, indice, coef)
  which(vector.de.outliers  == TRUE)
}

## ------------------------------------------------------------------------
MiBoxPlot_IQR_Univariate_Outliers = function (datos, indice.de.columna, coef = 1.5){
  
  datos = as.data.frame(datos)
  vector.TF.outliers.IQR = vector_es_outlier_IQR(datos, indice.de.columna, coef)
  nombres.de.filas = Nombres_de_Filas(datos, vector.TF.outliers.IQR)
  nombre.de.columna = colnames(datos, indice.de.columna)
  
  ggboxplot = ggplot(data = datos, aes(x=factor(""), y=datos[,indice.de.columna]) , environment = environment()) + 
    xlab(nombre.de.columna) + ylab("") +
    geom_boxplot(outlier.colour = "red") + 
    geom_text(aes(label = nombres.de.filas)) #, position = position_jitter(width = 0.1))   
  
  x11()
  ggboxplot
}

## ------------------------------------------------------------------------
MiBoxPlot_juntos  = function (datos, vector_TF_datos_a_incluir = c()){  
 
  nombres.de.filas = Nombres_de_Filas(datos, vector_TF_datos_a_incluir)
  
  datos = scale(datos)
  datos.melted = melt(datos)
  colnames(datos.melted)[2]="Variables"
  colnames(datos.melted)[3]="zscore"
  factor.melted = colnames(datos.melted)[1]
  columna.factor = as.factor(datos.melted[,factor.melted])
  levels(columna.factor)[!levels(columna.factor) %in% nombres.de.filas] = ""  
  
  ggplot(data = datos.melted, aes(x=Variables, y=zscore), environment = environment()) + 
    geom_boxplot(outlier.colour = "red") + 
    geom_text(aes(label = columna.factor), size = 3) 
}


## ------------------------------------------------------------------------
MiBoxPlot_juntos_con_etiquetas = function (datos, coef = 1.5){
  matriz.datos.TF.outliers = sapply(1:ncol(datos), function(x) vector_es_outlier_IQR(datos, x, coef))  # Aplicamos outlier IQR a cada columna
  vector.datos.TF.outliers = apply(matriz.datos.TF.outliers, 1, sum)   
  vector.datos.TF.outliers[vector.datos.TF.outliers > 1] = 1            # Si un registro es outlier en alguna columna lo incluimos
  
  MiBoxPlot_juntos(datos, vector.datos.TF.outliers)
}

## ------------------------------------------------------------------------
MiPlot_resultados_TestGrubbs = function(datos){
  alpha = 0.05
  
  test.de.Grubbs = grubbs.test(datos, two.sided = TRUE)
  cat('p.value: ')
  cat(test.de.Grubbs$p.value)
  cat('\n')
  
  if (test.de.Grubbs$p.value < alpha){
    indice.de.outlier.Grubbs = order(abs(datos - mean(datos)), decreasing = T)[1]
    indice.de.outlier.Grubbs
    cat('?ndice de outlier: ')
    cat(indice.de.outlier.Grubbs)
    cat('\n')
    valor.de.outlier.Grubbs  = datos[indice.de.outlier.Grubbs]
    cat('Valor del outlier: ')
    cat(valor.de.outlier.Grubbs)
    MiPlot_Univariate_Outliers (datos,indice.de.outlier.Grubbs,"Test de Grubbs")
  }
  else
    cat('No hay outliers')
}

## ------------------------------------------------------------------------
MiPlot_resultados_TestRosner = function(datos){  
  test.de.rosner = rosnerTest(datos, k=4)
  is.outlier.rosner = test.de.rosner$all.stats$Outlier
  k.mayores.desviaciones.de.la.media = test.de.rosner$all.stats$Obs.Num
  indices.de.outliers.rosner = k.mayores.desviaciones.de.la.media[is.outlier.rosner]
  valores.de.outliers.rosner = datos[indices.de.outliers.rosner]
  cat("\nTest de Rosner")
  cat("\n?ndices de las k-mayores desviaciones de la media: ")
  cat(k.mayores.desviaciones.de.la.media)
  cat("\nDe las k mayores desviaciones, ?Qui?n es outlier? ")
  cat(is.outlier.rosner)
  cat("\nLos ?ndices de los outliers son: ")
  cat(indices.de.outliers.rosner)
  cat("\nLos valores de los outliers son: ")
  cat(valores.de.outliers.rosner)
  MiPlot_Univariate_Outliers (datos, indices.de.outliers.rosner, "Test de Rosner")
}

## ------------------------------------------------------------------------
MiBiplot = function(datos){
  PCA.model = princomp(scale(datos))
  biplot = ggbiplot(PCA.model, obs.scale = 1, var.scale=1 , varname.size = 5) 
  x11()
  print(biplot)
}

MiBiPlot_Multivariate_Outliers = function (datos, vectorTFoutliers, titulo){
   identificadores_de_datos = rownames(datos)
   identificadores_de_datos[!vectorTFoutliers] = ''
   cat(identificadores_de_datos)
 
   PCA.model = princomp(scale(datos))
   outlier.shapes = c(".","x") #c(21,8)
   biplot = ggbiplot(PCA.model, obs.scale = 1, var.scale=1 , varname.size = 5,groups =  vectorTFoutliers) #alpha = 1/10, 
   biplot = biplot + labs(color = "Outliers")
   biplot = biplot + scale_color_manual(values = c("black","red"))
   biplot = biplot + geom_text(label = identificadores_de_datos, stat = "identity", size = 3, hjust=0, vjust=0)
   biplot = biplot + ggtitle(titulo)
  
   x11()
   print(biplot)
}

## ------------------------------------------------------------------------
MiBiPlot_Clustering_Outliers = function (datos, titulo){
  PCA.model = princomp(scale(datos))
  outlier.shapes = c("o","x") #c(21,8)
  
  identificadores_de_datos = rownames(datos)
  identificadores_de_datos[!BIPLOT.isOutlier] = ''
  #cat(identificadores_de_datos)
  
  BIPLOT.asignaciones.clusters = factor(BIPLOT.asignaciones.clusters)
  
  biplot = ggbiplot(PCA.model, obs.scale = 1, var.scale=1 , varname.size = 3, alpha = 0) +              
    geom_point(aes(shape = BIPLOT.isOutlier, colour = factor(BIPLOT.asignaciones.clusters)))  +
    scale_color_manual(values = BIPLOT.cluster.colors) +
    scale_shape_manual(values = outlier.shapes) +
    ggtitle(titulo) +
    geom_text(label = identificadores_de_datos, stat = "identity", size = 3, hjust=0, vjust=0)      
  
  x11()
  print(biplot)
}

## ------------------------------------------------------------------------
vectorIQR = apply(datos,2,function(x){iqr = quantile(x,0.75) - quantile(x,0.25)})
vectorQuantiles = apply(datos,2,function(x){quantile(x,.25)})
vectorQuantiles = rbind(vectorQuantiles,apply(datos,2,function(x){quantile(x,.75)}))
vectorIQR = rbind(vectorIQR,vectorQuantiles)

## ------------------------------------------------------------------------
outlierMatrix = matrix(data=FALSE,nrow=(dim(datos)[1]),ncol = (dim(datos)[2]))
for(i in 1:(dim(datos)[1])){
  for(j in 1:(dim(datos)[2])){
    outlierMatrix[i,j] = datos[i,j] > vectorIQR[1,j]*1.5 + vectorIQR[3,j] | datos[i,j] <   vectorIQR[2,j] - vectorIQR[1,j]*1.5
  }
}

## ------------------------------------------------------------------------
outlierMatrix[,c(2,3,4,5,8)] = NA

## ----message=FALSE,warning=FALSE,comment=FALSE,error=FALSE---------------
MiPlot_Univariate_Outliers(datos[,1],which(outlierMatrix[,1]==TRUE),"Number of people")
MiPlot_Univariate_Outliers(datos[,6],which(outlierMatrix[,6]==TRUE),"Apparent Temperature")
MiPlot_Univariate_Outliers(datos[,7],which(outlierMatrix[,7]==TRUE),"Temperature")

## ------------------------------------------------------------------------
testGrubb = grubbs.test(datos[,1], two.sided = TRUE)

if(testGrubb$p.value < 0.025){
  index = order(abs(datos[,1] - mean(datos[,1])), decreasing = T)[1]
  value = datos[index,1]
}
index
value

MiPlot_Univariate_Outliers(datos[,1],index,"Outlier")

## ------------------------------------------------------------------------
testRosner = rosnerTest(datos[,1],k=10)
isRosnerOutlier = testRosner$all.stats$Outlier
kMayoresDesviacionesMedia = testRosner$all.stats$Obs.Num
indexesOutliers = kMayoresDesviacionesMedia[isRosnerOutlier]
indexesOutliers

## ------------------------------------------------------------------------
alpha = 0.05
alphaPenalizado = 1- (1 - alpha) ^ (1/nrow(datos))

## ------------------------------------------------------------------------
moutlier = uni.plot(datos[,c(1,6,7)],symb=FALSE,alpha = alphaPenalizado)

## ------------------------------------------------------------------------
cat("Valores outliers")
sum(moutlier$outliers == TRUE)
cat("Valores normales")
sum(moutlier$outliers == FALSE)
plotData = cbind(datos,moutlier$outliers)
plot3d(x=plotData[,1],y=plotData[,6], z=plotData[,7], colvar =moutlier$outliers, col= c("black","red"), xlab = "Número de personas", ylab="Temperatura aparente", zlab="Temperatura") 
legend3d("topright", c("Valores normales", "Anomalías"), col= c("black","red"), pch=16)

## ------------------------------------------------------------------------
datosNormalizados = scale(datos[,c(1,6,7)])

## ------------------------------------------------------------------------
vecinos = 5

## ------------------------------------------------------------------------
puntuación = lofactor(datosNormalizados, k= vecinos)
puntuaciónOrdenada = sort(puntuación,decreasing = TRUE)
head(puntuaciónOrdenada)

## ------------------------------------------------------------------------
numOutliers = sum(puntuaciónOrdenada == Inf)  #output: 610
indicesOutliersLOF = rownames(datos) %in% order(puntuación,decreasing = T)[1:numOutliers]

## ------------------------------------------------------------------------
plot3d(x=datos[,1],y=datos[,6], z=datos[,7], colvar =indicesOutliersLOF, col= c("black","red"), xlab = "Número de personas", ylab="Temperatura aparente", zlab="Temperatura") 

## ------------------------------------------------------------------------
modeloKmedias = kmeans(datosNormalizados, centers = 4)
indicesKmedias = modeloKmedias$cluster
centroidesKmedias = modeloKmedias$centers

## ------------------------------------------------------------------------
estaciones = c(sum(indicesKmedias == 1), sum(indicesKmedias == 2),sum(indicesKmedias == 3),sum(indicesKmedias == 4))
estaciones

## ------------------------------------------------------------------------
distancias_a_centroides = function (datos.normalizados, 
                                    indices.asignacion.clustering, 
                                    datos.centroides.normalizados){
  
  sqrt(rowSums(   (datos.normalizados - datos.centroides.normalizados[indices.asignacion.clustering,])^2   ))
}


distanciaCentroides = distancias_a_centroides (datosNormalizados, 
                                                indicesKmedias, 
                                                centroidesKmedias)
head(sort(distanciaCentroides,decreasing = T))
tail(sort(distanciaCentroides,decreasing = T))

## ------------------------------------------------------------------------
umbral = (head(sort(distanciaCentroides,decreasing = T))[1] - tail(sort(distanciaCentroides,decreasing = T))[6])/2

totalOutliers = sum(distanciaCentroides > umbral)
totalOutliers

## ------------------------------------------------------------------------
top_clustering_outliers = function(datos.normalizados, 
                                   indices.asignacion.clustering, 
                                   datos.centroides.normalizados, 
                                   numero.de.outliers){
  
  dist_centroides = distancias_a_centroides (datos.normalizados, 
                                             indices.asignacion.clustering, 
                                             datos.centroides.normalizados)
  
  indices = order(dist_centroides, decreasing=T)[1:numero.de.outliers]
  
  list(distancias = dist_centroides[indices]  , indices = indices)
}
topOutliersKmedias = top_clustering_outliers(datosNormalizados,indicesKmedias,centroidesKmedias,totalOutliers)

## ------------------------------------------------------------------------
plot3d(x=datos[,1],y=datos[,6], z=datos[,7], colvar =topOutliersKmedias$indices, col= c("black","red"), xlab = "Número de personas", ylab="Temperatura aparente", zlab="Temperatura") 

## ------------------------------------------------------------------------
logicalKmediasIndexes = 1:26066 %in% topOutliersKmedias$indices
outliersintersection = moutlier$outliers & indicesOutliersLOF & logicalKmediasIndexes
sum(outliersintersection)

## ------------------------------------------------------------------------
head(datos[moutlier$outliers,])

## ------------------------------------------------------------------------
head(datos[indicesOutliersLOF,])

## ------------------------------------------------------------------------
head(datos[logicalKmediasIndexes,])

