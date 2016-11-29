#Carga la base de datos
database <- read.csv("~/DataScience/DataScienceInR/CaliforniaRegression/california.dat", comment.char = "@", header = FALSE)

#Da nombres a cada una de las variables
names(database) <- c("Longitude","Latitude","HousingMedianAge","TotalRooms","TotalBedrooms","Population","Households","MedianIncome","MedianHouseValue")

#Tomamos los 60 primeros como train confiando en que estén suficientemente desordenados para que no afecte a la generalización del algoritmo
train <- database[1:dim(database)[1]*0.6,]
train_results <- database[1:dim(database)[1]*0.6,dim(database)[2]]

test <- database[dim(database)[1]*0.6:dim(database)[1],]

#Hipótesis iniciales sin ver el contenido del dataset

# 1) El número de habitaciones aumenta el precio de forma lineal. 
# 
# 2) Cuanto más viejas las casas más disminuye el precio. 
# 
# 3) A más población más aumenta el precio de la vivienda
# 
# 4) La distancia del centro disminuye el precio
# 
# 5) La media de ingresos hace aumentar el precio 
# 
# 6) El número de unidades familiares en la vivienda hace disminuir el precio.

#Partiendo de esto mostramos gráficamente las variables relacionadas con las hipótesis anteriores menos la 4 que hay que hacer un preprocesamiento previo.
attach(database)
pairs(MedianHouseValue ~ TotalRooms+HousingMedianAge+Population+MedianIncome+Households)

#La única variable que parece lejanamente seguir un modelo lineal es MedianIncome.
#Ninguna de las restante muestra una relación claramente lineal con la salida. Pero no las descartaremos. 
#Pero para terminar de descartar las hipótesis anteriores tenemos que crear una variable nueva que sea la distancia al centro de california. 
#Aunque es tentativo calcular el punto medio de las variables longitud y latitud para obtener el centro, esta medida podría no corresponderse con el centro real del estado.
#Calcular el punto medio del estado tampoco puede ser significativo, sin embargo la distancia a cada una de las ciudades más importantes del estado puede que si.
#Por lo tanto crearemos tres variables que serán la distancia a Los Ángeles, San Diego y San Francisco. 

#Los Ángeles está situado en las coordenadas 34.052235, -118.243683
#San Diego está situado en las coordenadas 32.715736, -117.161087
#San Francisco está situado en las coordenadas 37.733795, -122.446747

#Aunque se podrían utilizar otras ciudades como Sacramento o Tijuana se han elegido las anteriores porque están suficientemente alejadas unas de otras y son ciudades de grandes dimensiones.
