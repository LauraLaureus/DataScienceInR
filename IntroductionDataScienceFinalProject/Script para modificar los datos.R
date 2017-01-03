#Script para modificar los datos de regresión


lmmMSEtrain<-mean(sapply(1:5,run_lmm_fold,nombre,"train"))
lmmMSEtest<-mean(sapply(1:5,run_lmm_fold,nombre,"test"))

file_r <- read.csv("./WizmirRegression/wizmir/regr_train_alumnos.csv",comment.char = "@")
file_r[18,2:4] = c(lmMSEtrain,knnMSEtrain,lmmMSEtrain)
write.csv(file_r,"./WizmirRegression/wizmir/regr_train_alumnos.csv")
file_r = read.csv("./WizmirRegression/wizmir/regr_test_alumnos.csv",comment.char = "@")
file_r[,2] = NULL
file_r[18,2:4] = c(lmMSEtest,knnMSEtest,lmmMSEtest)
write.csv(file_r,"./WizmirRegression/wizmir/regr_test_alumnos.csv")
