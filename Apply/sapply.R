#Construye el vector d1
d1=cbind(c(1:5),c(1,5,2,7,14),c(11:15))

#sustituye los números 1 2 7 y 14 por su palabra en alemán eins, zwei, siebe y vierzehn utilizando sapply()
detector <- function(x) {
  if(x == 1){ return("eins"); }
  else if (x == 2) {return("zwei");}
  else if(x == 7) {return("sieben");}
  else if(x == 14) {return("vierzehn");}
  else{ return(x);}
}

r <- sapply(d1,detector)
r

#descarga la librería lattice y mira que dimensión tiene el dataser barley y que tipo de objeto es
install.packages("lattice")
library(lattice)
str(barley)
 #Es un dataframe de 120 registros y 4 variables


