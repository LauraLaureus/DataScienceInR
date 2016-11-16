#Crea los siguientes vectores

employee <- c('John Doe','Peter Gynn','Jolie Hope') 
salary <- c(21000, 23400, 26800) 
startdate <- as.Date(c('2010-11-1','2008-3-25','2007-3-14'))

#¿Tienen todos los vectores la misma clase?¿Que clases son?
#Employee es vector de caracteres
#Salary es de vector de números
#starDate es vector de Fechas (Date)

#Combina los vectores en un data frame que se llame employ.data
employ.data <- data.frame(employee,salary,startdate)

#	Muestra por pantalla la estructura del nuevo data frame
str(employ.data) #EL vector de caracteres es un factor

#Crear de nuevo el dataframe con la variable stringsAsFactors a False
employ.data <- data.frame(employee,salary,startdate,stringsAsFactors = FALSE)
str(employ.data)

