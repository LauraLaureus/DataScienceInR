
lvls = levels(trainImputed[,"TIPO_ACCIDENTE"])

#Tomar el máximo número de instancias e igualar el número de instancias del resto de las clases.
instances = c()
for (i in 1:length(lvls)){
  instances = c( instances, sum(trainImputed == lvls[i]))
}

instances

maxInstances = max(instances)
maxInstances

extendedTrainImputed = trainImputed

for(c in 1:length(lvls)){
  
  #Tomar el subconjunto de instancias que coincide con el tipo de accidentes
  trainOfClassSubset = subset(trainImputed,TIPO_ACCIDENTE == lvls[c]) #???
  
  
  subsetCount = dim(trainOfClassSubset)[1]
  subsetCount
  while(subsetCount < maxInstances){
    # diff = maxInstances - subsetCount
    # if (diff+1 >= subsetCount){
    #   diff = subsetCount-2
      #randSample = trainOfClassSubset[,]
      randSample = sample(trainOfClassSubset,30)
    #}
    # else{
    #   randSample = trainOfClassSubset[sample(trainOfClassSubset,diff,replace = FALSE),]
    # }
    extendedTrainImputed = rbind(extendedTrainImputed,randSample)
    subsetCount = subsetCount + (dim(randSample)[1]) 
  }
}

clss = c()
for (i in 1:length(lvls)){
  clss = c( clss, sum(extendedTrainImputed == lvls[i])/(dim(extendedTrainImputed)[1]))
}

clss