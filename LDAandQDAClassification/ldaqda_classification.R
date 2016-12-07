library(MASS)
library(ISLR)
library(klaR)


data(Smarket)
names(Smarket)
attach(Smarket)

#Try lda with all Lag variables
train_model <- Year< 2005

lda_model <- lda(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5, subset = train_model)
lda_model

plot(lda_model,type = "both")

Smarket2005  <-  subset(Smarket,Year == 2005)

lda_predict = predict(lda_model,Smarket2005[,-9])
lda_confusionTable <- table(lda_predict$class,Smarket2005$Direction)

partimat(Direction~Lag1+Lag2+Lag3+Lag4+Lag5, data=Smarket ,method="lda", main ="Comparación de lda en todas las dimensiones")
#No supera en ningun caso el 50% de acierto 
lda_accuracy <- (lda_confusionTable[1,1]+lda_confusionTable[2,2])/sum(lda_confusionTable)


#Make a quick comparison between logistic regression and lda
lr <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5,data = Smarket,family = binomial, subset = train_model)
summary(lr)

lr_predict <- predict(lr,newdata=Smarket[!train_model,],type = "response")
plot(lr_predict)

lr_labelPrediction <- ifelse(lr_predict >0.5, "Up", "Down")
Direction2005 <- Smarket2005$Direction

lr_confusiontable <- table(lr_labelPrediction,Direction2005)
#Las tablas de confusión son prácticamente iguales, por lo que no parece haber diferencia entre lda y la regresión lineal múltiple.

lr_accuracy <- (lr_confusiontable[1,1]+lr_confusiontable[2,2])/sum(lr_confusiontable)


#Try with qda and compare all three methods. Plot the results.
qda_model <- qda(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5, subset = train_model)
qda_predict <- predict(qda_model,Smarket2005)

qda_confusionTable <- table(qda_predict$class,Smarket2005$Direction)
#aumentan los falsos positivos porque las varianzas de los dos conjuntos son similares.

qda_accuracy <- (qda_confusionTable[1,1]+qda_confusionTable[2,2])/sum(qda_confusionTable)

partimat(Direction~Lag1+Lag2+Lag3+Lag4+Lag5, data=Smarket ,method="qda", main ="Comparación de qda en todas las dimensiones")
#Incluso el mejor de los error. rate disminuye.

plot(as.factor(c("lda","lr","qda")),c(lda_accuracy,lr_accuracy,qda_accuracy), main = "Comparación de los métodos de clasificación", xlab = "Método", ylab="Presición")




##EJERCICIO 2 

#Using only the information in file clasif_train_alumnos.csv_
students <- read.csv("clasif_train_alumnos.csv", header = TRUE)


#Compare lda and qda usign Wilconxon.

#Diferencias entre QDA y LDA
diff <- (students[,2]-students[,3])/students[,2]
wilc_1_2 <- cbind(
        ifelse(diff < 0, abs(diff)+0.1,0.1),
        ifelse(diff > 0, abs(diff)+0.1,0.1)
)

qdaVSlda <- wilcox.test(wilc_1_2[,1],wilc_1_2[,2], alternative = "two.sided",paired = TRUE)
qdaVSlda2 <- wilcox.test(wilc_1_2[,2],wilc_1_2[,1], alternative = "two.sided",paired = TRUE)

Rplus <-qdaVSlda$statistic
Rminus <- qdaVSlda2$statistic
p_value <- qdaVSlda$p.value

Rplus
Rminus
p_value

#Hay un 0.3 de confianza de que sean distintos

#Perform a multiple comparison using Friedman. 

friedman_result <- friedman.test(as.matrix(students[,2:3]))
friedman_result

#confianza de que sean distintos 1-0.65 =0.35

#Using Holm see if there is a winning algorithm

groups <- rep(1:dim(students[,2:3])[2], each=dim(students)[1])
pairwise.wilcox.test(as.matrix(students[,2:3]), groups, p.adjust= "holm", paired = TRUE)

#confianza de que sean distintos 0.35