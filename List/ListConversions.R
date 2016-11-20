#Ejemplo de cómo crear una lista. Ejecuta los comandos y describe que es lo que ocurre
my_list <- list(name="Fred", wife="Mary", no.children=3, child.ages=c(4,7,9)) 
  #Lista con cuatro elementos

names(my_list) my_list[2]; #Ocurre un error inesperado
names(my_list) [2] #Tal vez se pretendía hacer esto?

my_list[[2]] #Obtiene el valor del segundo parámetro de la lista
my_list$wife #Hace lo mismo que el anterior

my_list[[4]] [2] #Toma el valor del segundo elemento del cuarto parámetro de la lista que casualmente es un vector
length(my_list[[4]]) #Obtiene la longitud del vector almacenado como cuarto elemento de la lista

my_list$wife <- 1:12 #Cambio de tipo de variable almacenada en el elemento "wife"
my_list$wife <- NULL #Borra uno de los elementos de la lista

my_list <- c(my_list, list(my_title2=month.name[1:12])) #Añade al final de la lista el vector de caracteres que representa los meses
unlist(my_list) #Cambia de lista a vector 

data.frame(unlist(my_list)) #Transforma la lista en un dataframe poniendo cada uno de los elementos de cada parámetro coo un registro del dataframe
matrix(unlist(my_list)) #Transforma la lista en una matriz columna donde todos los elementos de cada parámetro pasan a ser un registro y el tipo de datos para todos ellos es caracter.

