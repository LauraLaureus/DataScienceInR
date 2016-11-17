#	Selecciona las variables 1,2,9 y 10 de mtcars() y asignalas a un data frame llamado cars. Utiliza estos numerous como indices para extraer las variables.
cars <- data.frame(mtcars[,c(1,2,9,10)])

#Conveirte la variable gear en un factor ordenado
cars$gear <- as.factor(cars$gear)

#	Transforma la variable am de cars en un factor dónde su valor sea “auto” si su valor original era 1 y “manual” sies era 0. PISTA: mira la función ifelse(). ¿Cómo puedes usar la propiedad de esta función para escribir tu código
cars$am <- as.factor(ifelse(cars$am == 1, "auto","manual"))

#Comprueba la estructura de tu set de datos. Describe tu nuevo data frame
str(cars)

#La estructura es de 2 variables de tipo numéricas mpg y cyl y dos variables de tipo factor, am que tiene dos niveles Auto y Manual mientras que la variable gear tiene tres niveles 3 4 5.