#Creación del data set
myMA <- matrix(rnorm(100000), 10000, 10, dimnames=list(1:10000, paste("C", 1:10, sep="")))

#Calcula la media para cada fila
means <- apply(myMA, 1,mean)

#Calcula la media para cada columna 
means_v <- apply(myMA, 2,mean)

#Calcula las filas de la matriz anterior que contienen missing values NA. Antes de calcularlos recuerda utilizar n.rm=T
means_na <- apply(myMA, 1,mean, na.rm = T)
table(is.na(means_na))
  #Ninguno es NA