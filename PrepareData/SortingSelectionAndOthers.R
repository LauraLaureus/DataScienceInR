#Ordena este data set de forma ascendente según su valo de hp. 
mtcars[with(mtcars,order(mtcars$hp)),]

#	Hazlo ahora de forma descendente
mtcars[with(mtcars,order(mtcars$hp, decreasing = TRUE)),]

#	Calcula la media de la columna mpg. 
mean(mtcars$mpg)

#	Calcula la media de mpg para aquellos datos cuyo valor de hp sea menor que 150 y por separado para aquellos cuyo valor de hp sea mayor o igual a 150
mean(mtcars[mtcars$hp < 150,1])
mean(mtcars[mtcars$hp >= 150,1])

#Busca los valores únicos de la columna cyl de mtcars
unique(mtcars$cyl)

#	Obten los datos de mpg cyl disp hp para “Toyota Corolla"
mtcars["Toyota Corolla",c("mpg","cyl","disp")]

#	Crea una nueva variable mpgClass de tipo categórico cuyo valor es “Low“ si el valor de mpg es menor que la media de la columna mpg y “High” si es mayor que la media de mpg. PISTA ifelse(). Combina ese comando con with() para añadir la nueva variable a mtcars
mpgClass <- factor(ifelse(mtcars$mpg < mean(mtcars$mpg), "Low","High"))

#¿qué pasa cuando ejecutas este comando?with(mtcars, tapply(hp, list(cyl, gear), mean))
#Devuelve un objeto que contiene las medias de los conjuntos que tienen las marchas que indica su columna y los cilndros que marca su fila.

