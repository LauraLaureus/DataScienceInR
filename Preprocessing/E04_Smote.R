library(DMwR)
smotedData = SMOTE(TIPO_ACCIDENTE~.,trainImputed)
smotedTrain = rbind(trainImputed,smotedData)
