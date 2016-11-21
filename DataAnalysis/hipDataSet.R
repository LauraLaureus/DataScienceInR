#Descargate el dataset hip con el siguiente comando
hip  <-read.table("http://astrostatistics.psu.edu/datasets/HIP_star.dat", header=T,fill=T)

#¿Qué dimensión tiene?
dim(hip)
#[1] 2719    9

#¿Qué datos alberga?
colnames(hip)
#[1] "HIP"   "Vmag"  "RA"    "DE"    "Plx"   "pmRA" 
#[7] "pmDE"  "e_Plx" "B.V"  

#Muestra por pantalla la columna de la variable RA
hip$RA

#Calcula las tendencias centrales de todos los datos del dataset usando la función apply
hip_means <- apply(hip,2,mean)
hip_medians <- apply(hip,2,median)

#Haz lo mismo para las medidas de dispersión mínimo y máximo.¿Sería posible hacerlo con un único comando?¿Qué hace la función range?
#Si se puede hacer con la función range que devuelve el mínimo y el máximo de la estructura de datos que se le pasa
hip_range <- apply(hip,2,range)

#Sin embargo las medidas mas populares de dispersión son la varianza (var()), su desviación standard (sd()) y la desviación absoluta de la mediana o MAD. Calcula estas medidas para los valores de RA
hip_RA_var <- var(hip$RA)
hip_RA_std <- sd(hip$RA)
hip_RA_mad <- mad(hip$RA)

#Imagina que quieres calcular dos de estos valores de una sola vez. ¿Te serviría este código?
f = function(x) c(median(x), mad(x))  
f(hip[,1])
#Si que funciona

#¿Cuál sería el resultado de aplicar apply(hip,2,f)?
#se obtiene la mediana y la desviación de la mediana de cada una de las variables del data frame

#Calcula el cuartil .10 y .50 para la columna RA del dataset hip
quantile(hip$RA,c(0.1, 0.5))

#Calcula los cuatro cuartiles para RA con un único comando.
quantile(hip$RA,c(0.25, 0.5,0.75,1))

#Otra medida de dispersion es la diferencia entre el primer y el tercer cuartil conocida como rango intercuartil (IQR) Inter Quantile Range. ¿Obtienes ese valor con la función summary()?
#Nop a no ser que ejecutes
summary(hip$RA)[5]-summary(hip$RA)[2]

#Hasta ahora has ignorado la presencia de  valores perdidos NA. La función any() devuelve TRUE si se encuentra al menos un TRUE en el vector que damos como argumento. Su combinación con is.na es muy útil. ¿qué obtienes cuando ejecutas el siguiente comando? ¿Cómo lo interpretas?
hasNA = function(x) any(is.na(x))
apply(hip,2,hasNA)
#FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE
#Se interpreta como que la única columna que tiene valores perdidos es la de la variable BV

#Para ello podemos utilizar la función na.omit. ¿Que ocurre cuando lo hacemos?. 
#Se eliminan los registros que tienen datos perdidos

#Usando apply calcula la media para hip y hip1. Intenta calcular la media de forma que solo cambie la de B.V cuando ignores los valores NA.
mean(hip$B.V)
mean(hip1$B.V)

#Obten una idea aproximada de tus datos mediante la creación de un boxplot del hop dataset
boxplot(hip)

#Crea un scatterplot que te compare los valores de RA y DE. Representa los puntos con el símbolo ‘.’ Y que estos puntos sean de color rojo si DE excede de 0. Sugerencia ifelse()
plot(hip$RA, hip$DE, pch=16, col = ifelse(hip$DE > 0, "red","black"), xlab = "RA", ylab = "DE", main = "Comparación RA-DE")

#Haz un scatterplot de RA y pmRA. ¿Ves algún patrón?
plot(hip$RA, hip$pmRA,xlab = "RA",ylab="pmRA", main= "Comparación RA-pmRA")
#los datos parecen una parábola

#En vez de crear los plots por separado para cada par de columnas, hazlos con un solo comando con el scatterplot matrix
pairs(hip)

#Para poder acceder a las variables por su nombre usa attach(hip).
attach(hip)

#Vamos a seleccionar las estrellas Hyadas del dataset aplicando los siguientes filtros:
#•	RA in the range (50,100) 
#•	DE in the range (0,25) 
#•	pmRA in the range (90,130) 
#•	pmDE in the range (-60,-10) 
#•	e_Plx <5 
#•	Vmag >4 OR B.V <0.2 (this eliminates 4 red giants) 

hyadas <- hip[hip$RA < 100 & hip$RA > 50,]
hyadas <- hyadas[hyadas$DE < 25 & hyadas$DE > 0,]
hyadas <- hyadas[hyadas$pmRA < 130 & hyadas$pmRA > 90,]
hyadas <- hyadas[hyadas$pmDE < -10 & hyadas$pmDE > -60,]
hyadas <- hyadas[hyadas$e_Plx < 5,]
hyadas <- hyadas[hyadas$Vmag > 4 | hyadas$B.V <0.2,]

#¿Qué dimensiones tiene?
dim(hyadas)
#[1] 88  9

# Grafica un scatterplot de Vmag vs B.V
plot(hyadas$Vmag, hyadas$B.V,xlab = "Vmag", ylab = "B.V", main = "Comparativa Vmag-B.V.")


