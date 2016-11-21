#Inspecciona las primeras filas del dataset y calcula el summary() del mismo con cada atributo del dataset
iris[1:10,]
summary(iris)

#* Crea un histograma de petal.width , teniendo en cuenta que el numero de bins es variable fija este a 9. Añádele color y nombres al eje x "Petal Width"y al gráfico dale el nombre de  "Histogram of Petal Width". 
hist(iris$Petal.Width,breaks = 9,col = "blue",xlab = "Petal Width",main = "Histogram of Petal Width")

#Crea un histograma para cada variable
hist(iris$Petal.Length,breaks = 9,col = "blue",xlab = "Petal Length",main = "Histogram of Petal Length")
hist(iris$Sepal.Length,breaks = 9,col = "blue",xlab = "Sepal Length",main = "Histogram of Sepal Length")
hist(iris$Sepal.Width,breaks = 9,col = "blue",xlab = "Sepal Width",main = "Histogram of Sepal Width")


#Crea los cuartiles del dataset
quantile(iris$Sepal.Length,c(0.25,0.5,0.75,1))
quantile(iris$Sepal.Width,c(0.25,0.5,0.75,1))
quantile(iris$Petal.Width,c(0.25,0.5,0.75,1))
quantile(iris$Petal.Length,c(0.25,0.5,0.75,1))

#Representa en un boxplot la variable de ancho de hoja dependiendo del tipo de hoja que tengan
boxplot(iris$Sepal.Width~iris$Species)


#Crea los boxplot de la longitud del pétalo en función de la especie de Iris.
boxplot(iris$Sepal.Length~iris$Species)

#Compara con scatter plots las variables entre sí.
plot(iris$Sepal.Length,iris$Sepal.Width,xlab = "Longitud", ylab = "Anchhura",main = "Comparación Logitud y Ancho del Sépalo")

