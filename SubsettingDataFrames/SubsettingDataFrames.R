#Obten las dimensiones del data frame utilizando la función dim
dim(USArrests)
#Output: [1] 50  4

#¿Qué hace la función length cuando se aplica sobre un daraframe?
length(USArrests)
#Devuelve el número de columnas del dataframe

#Obtén el número de columnas y el nnúmero de filas
nrow(USArrests)
#Output: [1] 50
ncol(USArrests)
#Output: [1] 4

#Obtén el nombre de las filas y las columnas para este data fram
colnames(USArrests)
#Output: [1] "Murder"   "Assault"  "UrbanPop" "Rape"
row.names(USArrests)

#Échale un vistazo a los datos, por ejemplo a las seis primeras filas
USArrests[1:6,]

#Ordena de forma decreciente las filas de nuestro data frame, según el porcentaje de población en el área urbana. 
USArrests[order(USArrests$UrbanPop,decreasing = TRUE),]

#¿Podrías añadir un segundo criterio de orden?,¿como?
#No con mis conocimientos actuales de R, solo podría determinar como criterio de ordenación operaciones entre columnas. 

#Muestra por pantalla la columna con los datos de asesinato
USArrests$Murder

#Muestra las tasas de asesinato para el segundo,tercer y caurto estado
USArrests[2:4,"Murder"]

#Muestra las primeras cinco filas de todas las columnas
USArrests[1:5,]

#Muestra todas las filas para las dos primeras columnas
USArrests[,1:2]

#Muestra todas las filas de las columnas 1 y 3
USArrests[,c(1,3)]

#Muestra solo las primeras cinco filas de las columnas 1 y 2
USArrests[1:5,1:2]

#¿Qué estado tiene la menor tasa de asesinatos?
USArrests[which(USArrests$Murder == min(USArrests$Murder)),]

#¿Qué estado tienen una tasa inferior al 4%?
USArrests[which(USArrests$Murder <= 4.0),]

#¿Qué estados están en el cuartil superior en lo que a población en zonas urbanas se refiere?
USArrests[which(USArrests$UrbanPop >= quantile(USArrests$UrbanPop)[4]),]

#Leer el fichero tabulado student.txt
student_r <- read.csv("/Volumes/USB DISK/MO_Ciencia de Datos/Obligatorias/Introducción a la ciencia de datos/Ejercicios II/student.txt",sep = "\t")

#Imprime solo los nombres de las columnas
colnames(student_r)

#Selecciona solo a la columna height
student_r[,"height"]

#¿Cuantas observaciones hay en cada grupo?
table(student_r)
#4 
#3
#5
#5

#Crea una variable sym que contenga M si el genero es masculin y F si el genero es femeninio.
sym <-ifelse(student_r$gender == "female","F","M")

#Crea una segunda variable “colours” cuyo valor será “Blue” si el estudiante es de kuopio y “Red” si es de otro sitio.
colours_r <- ifelse(student_r$population == "kuopio","Blue","Red")

#	Con los datos anteriores de height y shoesize y las nuevas variables crea un nuevo data.frame que se llame students.new
student_new <- data.frame(student_r$height, student_r$shoesize, sym,colours_r)

#	Comprueba que la clase de student.new es un dataframe
is.data.frame(student_new)

#Crea dos subsets a partir del dataset student. Divídelo dependiendo del sexo. Para ello primero comprueba que estudiantes son hombres (male). Pista: busca información sobre la función which
students_female <- student_new[which(student_new$sym == "F"),]
students_male <- student_new[which(student_new$sym == "M"),]

#Utiliza la function write.table() para guarder el contenido de student.new en un archivo
write.table(student_new,"/Volumes/USB DISK/MO_Ciencia de Datos/Obligatorias/Introducción a la ciencia de datos/Ejercicios II/students_new.txt")

