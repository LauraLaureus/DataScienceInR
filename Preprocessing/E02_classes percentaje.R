lvls = levels(trainImputed[,"TIPO_ACCIDENTE"])
clss = c()
for (i in 1:length(lvls)){
  clss = c( clss, sum(trainImputed == lvls[i])/(dim(trainImputed)[1]))
}

clss

#Las clases son desbalanceadas
# 0.12139191 0.03173122 0.55062996 0.06022932 0.20041997 0.03559763