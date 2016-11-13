#Execute commands

matrix(data = 5,nr =2 ,nc=2)
matrix(1:6,2,3)
matrix(1:6,2,3, byrow = TRUE)

#Crea un vector con los 30 primeros númerso y crea con el una matriz m con 3 filas y 10 columnas.
z <- 1:30
m <- matrix(z,nrow = 3,ncol = 30)

#Escribe la tercera columna en un vector
v <- m[,3]

#Crea en R las siguientes matrices
x_data <- c(3,21,-1,1)
x <- matrix(x_data,byrow = TRUE,ncol = 2)

y_data <- c(1,0,4,1,NA,NA,0,-1)
y <- matrix(y_data,nrow = 2)

#Calcula los efectos de los siguientes comandos
x[1,]
#Output: [1]  3 21
x[2,]
#Output: [1] -1  1
x[,2]
#Output: [1] 21  1
y[1,2]
#Output: [1] 4
y[,2:3]
#      [,1] [,2]
#[1,]    4   NA
#[2,]    1   NA

#Transforma la matriz m que creáste en el ejercicio anterior en un array multidimensional.
dim(m)
#Output: [1]  3 30
m_array <- array(m,dim = c(5,2,3))
m_array

#Crea un array de 5x5 y rellénalo con valores del 1 al 25. Investiga la función array.
x <- array(1:25,dim = c(5,5))

#Escribe el array x en un vector y
y <- c(x)

#Dadas las matruces m1 y m2 usa rbind y cbind para crear matrices nuevas 
m1 <- matrix(1, nr = 2, nc = 2)
m2 <- matrix(2, nr = 2, nc = 2)

M1 <- cbind(m1,m2)
M1
M2 <- rbind(m1,m2)
M2

#Se diferencian en que rbind amplia la primera matriz con la segunda matriz como si la segunda fueran nuevas filas
#mientras que cbind amplia la primera matriz como si la segunda fuera nuevas columnas.

#El operador para el producto de dos matrices es  %*%. Considerando las matrices del ejercicio anterior utilizalo.
m1%*%m2

#Usa la matriz del ejercicio anteriore y aplica la función t(). ¿Qué hace esa función?

M1
#      [,1] [,2] [,3] [,4]
#[1,]    1    1    2    2
#[2,]    1    1    2    2

t(M1)
#      [,1] [,2]
#[1,]    1    1
#[2,]    1    1
#[3,]    2    2
#[4,]    2    2
 #Esta función traspone la matriz

#Ejecuta los siguientes comando basados en la función diag(). ¿Que tipo de acciones puedes ejecutar sobre ella?
diag(m1)
# Diagonal principal: [1] 1 1
diag(rbind(m1, m2) %*% cbind(m1, m2))
#Diagonal principal de la multiplicación de m1 concatenada por filas de m1 y m2 y la concatenación de las mismas matrices por columnas 

diag(m1) <- 10 #Modificar la diagonal principal
diag(3) #Devuelve la matriz diagonal
v <- c(10,20,30)
diag(v) #Devuelve la matriz que tiene por diagonal principal v
diag(2.1, nr = 3, nc = 5) #Devuelve la matriz de 3x5 que tiene en su diagonal principal los elementos con el valor a 2.1


#Crea los siguientes vectores
new_hope = c(460.998007, 314.4)
empire_strikes = c(290.475067, 247.9)
return_jedi = c(309.306177, 165.8)

#Construye la matriz star_wars_matrix con esos vectores
data <- c(new_hope, empire_strikes, return_jedi)
data <- matrix(data,ncol=2,byrow = TRUE)

#Añadele nombres a las columnas y a las filas
colnames(data)<-c("US","Non-US")
rownames(data)<-c("new_hope","empire_strikes","return_jedi")
data

#Calcula las ganancias mundiales de cada película
worldwide_vecto <- apply(data,1,sum)

#Añade este último vector como una columna a la matriz
data <- cbind(data,worldwide_vector)

#Calcula las ganancias totales en USA y fuera de USA.
total <- colSums(data)

#Calcula la media de ganancias para todas las películas fuera de los estados unidos
non_us_all <- mean(data[,2])

#Calcula la media de ganancias para las dos primeras palículas fuera de los estados unidos
non_us_some<-mean(data[1:2,2])

#Calcula el número de visitantes asumiendo que el precio de la entrada es de 5€ o 5$
visitantes <- round(data/5)

#Calcula la media de visitantes en USA y fuera
meanVisits <- apply(visitantes,2,mean)
