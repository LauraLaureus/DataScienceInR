#	Extrae la columna Frost y asigna el resultado a la variable frost
frost <- state.x77[,7]

#	Tu Nuevo objeto es un vector numérico
str(frost)

#Ahora intenta agrupar los datos en frost en tres niveles. Para crear bins en tus datos puedes utilizar la función cut().
cut(frost,3)

#¿Qué obtienes como nombres de los niveles?
#Levels: (-0.188,62.7] (62.7,125] (125,188]

#	En la realidad no existen estados que tengan frost en días negativos. Esto es porque R añade un poco de padding. Prueba a solucionar el problema utilizando el parámetro include.lowest=TRUE en cut()
cut(frost,3,include.lowest = TRUE)

#Los nombres de los niveles no son demasiado informativos, especifica nuevos nombres para los niveles
levels(frost_fraction) <- c("low","intermediate","high")

#Ahora cuenta el número de estados que  hay en cada uno de los niveles. 
table(frost_fraction)

