#Laura del pino Díaz

#install.pagckages("bnlearn")
library(bnlearn)

# Variables de la red: 
# Edad A{ young (<30), adult(30 < A < 60)}
# Sexo S {M,F}
# Nivel Educativo E {high,uni}
# Ocupación O {emp, self}
# Residencia R {small, big}
# Travel T {car,train,other}

# Grafo vacío
dag = empty.graph(nodes = c("A", "S", "E","O","R","T"))
dag

#Añadir los arcos entre los nodos del grafo
dag = set.arc(dag,from = "A", to= "E")
dag = set.arc(dag,from = "S", to= "E")

dag = set.arc(dag,from = "E", to= "O")
dag = set.arc(dag,from = "E", to= "R")

dag = set.arc(dag,from = "O", to= "T")
dag = set.arc(dag,from = "R", to= "T")

dag

modelstring(dag)
modstr =  modelstring(dag) #Obtiene la string del modelo

dag3 = model2network(modstr) #Crear una red a partir de una string

all.equal(dag,dag3) #Muestra si son iguales las redes

nodes(dag) #Muestra la lista de nodos

arcs(dag) #Muestra la lista de arcos entre nodos

#Construcción alternativa

dag2 = empty.graph(nodes = nodes(dag))

arc.set = matrix( c("A","E","S","E","E","O","E","R","O","T","R","T"), byrow = TRUE,ncol = 2,dimnames = list(NULL, c("from","to")))
arcs(dag2) = arc.set

all.equal(dag,dag2)

#Intentar crear un ciclo en el grafo
try(set.arc(dag, from = "T", to = "E"))

#Visualización
plot(dag)

##Uso de Rgraohviz
source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")
graphviz.plot(dag)

###Probando otros layouts
graphviz.plot(dag,layout = "fdp")
graphviz.plot(dag,layout = "circo")

#Especificación de las probabilidades

##Especificación de los estados de las variables
A_sts = c("young", "adult", "old")
S_sts = c("M", "F")
E_sts = c("high","uni")
O_sts = c("emp","self")
R_sts = c("small", "big")
T_sts = c("car","train","other")

##Definición de las distribuciones de probabilidad 

A_prob = array( c(0.3,0.5,0.2), dim = 3, dimnames = list(A = A_sts))
A_prob

S_prob = array(c(0.6,0.4), dim = 2)
names(S_prob) = S_sts
S_prob

O_prob <- array(c(0.96, 0.04, 0.92, 0.08), dim=c(2,2), dimnames = list(O = O_sts, E = E_sts))
O_prob

R_prob <- array(c(0.25, 0.75, 0.20, 0.80), dim=c(2,2),dimnames = list(R = R_sts, E = E_sts))
R_prob

#Modo alternativo
#R_prob <- matrix(c(0.25, 0.75, 0.20, 0.80), ncol = 2, dimnames = list(R = R_sts, E = E_sts))

E_prob <- array(c(0.75, 0.25, 0.72, 0.28, 0.88, 0.12, 0.64,
                  0.36, 0.70, 0.30, 0.90, 0.10), dim=c(2, 3, 2),
                dimnames = list(E = E_sts, A = A_sts, S = S_sts))

T_prob <- array(c(0.48, 0.42, 0.10, 0.56, 0.36, 0.08, 0.58,
                  0.24, 0.18, 0.70, 0.21, 0.09), dim=c(3, 2, 2),
                dimnames = list(T = T_sts, O = O_sts, R = R_sts))

#Creación de la red bayesiana

cpt =  list(A=A_prob,S=S_prob,E=E_prob,O=O_prob, R=R_prob,T=T_prob)

bn = custom.fit(dag,cpt)
nparams(bn)
arcs(bn)

#Obtención de distintos tipos de información.
nodes(bn) #nodos del grafo
parents(bn,"R") #Padres del nodo R #Devuelve E
children(bn,"E")#Hijos del nodos E #Devuelve O y R

bn$R #Distribución de probabilidad del nodo R

#Distribución de probabilidad condicional
R_cpt = coef(bn$R)
R_cpt

#Todas las distribuciones de probabilidad condicional.
bn

#Inferencia en redes bayesianas

#Criterio de d-separación entre S y R 
dsep(dag,x = "S", y= "R")

#Criterio de d-separación entre O y R 
dsep(dag,x = "O", y= "R")

#Saber si existe un camino de un nodo a otro.
path(dag,from = "S", to = "R")

#El camino entre S y R está bloqueado si condicionamos sobre E.
dsep(dag,x="S",y="T",z=c("O","R"))

#Lo mismo con O y R
dsep(dag,x="O",y="R",z="E")

#A y S son independientes marginalmente
dsep(dag,x="A", y="S")
#Pero dependientes al condicionar E
dsep(dag,x="A", y="S",z="E")

#Inferencia exacta

#Instalación de los paquetes necesarios
biocLite(c("graph","RBGL", "Rgraphviz"))
install.packages("gRain")
library("gRain")

#Interpreta bn.fit como grain para construir un árbol de grupos
junction = compile(as.grain(bn))

#Obtener la distribución de probabilidad a posteriori para una variable dada la evidencia. 
querygrain(junction,nodes = "T")$T

#Definimos la evidencia Sexo = F
jsex = setEvidence(junction,nodes = "S", states = "F")

querygrain(jsex,nodes = "T")$T

#Definimos la evidencia R = small
jres = setEvidence(junction,nodes = "R", states = "small")
querygrain(jres,nodes = "T")

#Definimos la evidencia E = high
jedu = setEvidence(junction,nodes="E",states = "high")
SxT_cpt = querygrain(jedu, nodes = c("S","T"), type = "joint") #Distribución conjunta
SxT_cpt


SxT_cpt = querygrain(jedu, nodes = c("S","T"), type = "marginal") #Marginal
SxT_cpt

dsep(bn, x="S", y="T", z="E")

#Inferencia aproximada
cpquery(bn,event = (S == "M") & (T == "car"),evidence = (E == "high"))

#Aumentamos el número de muestras aleatorias
cpquery(bn,event = (S == "M") & (T == "car"),evidence = (E == "high"), n=10^6)

#Ponderación por verosimilitud
cpquery(bn,event = (S == "M") & (T == "car"),evidence = list(E = "high"), method = "lw")

#Probabilidad de que un hombre viaje en coche dado que es joven y su nivel de estudios es universitario. 
cpquery(bn,event = (S == "M") & (T == "car"),evidence = ((A == "young") & (E == "uni")) | (A =="adult"))

SxT = cpdist(bn,nodes = c("S","T"),evidence = (E == "high"))

head(SxT)
prop.table(table(SxT))
