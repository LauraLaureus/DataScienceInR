## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(width=60)
knitr::opts_chunk$set(comment = "", warning = FALSE, message = FALSE, echo = TRUE, tidy = TRUE, size="small")

## ------------------------------------------------------------------------
subclus <- read.table("subclus.txt", sep=",")
colnames(subclus) <- c("Att1", "Att2", "Class")

circle <- read.table("circle.txt", sep = ",")
colnames(circle) <- c("Att1", "Att2", "Class")

## ------------------------------------------------------------------------
#subcluss
plot(subclus$Att1, subclus$Att2, main="Subcluss")
points(subclus[subclus$Class==0,1],subclus[subclus$Class==0,2],col="red")
points(subclus[subclus$Class==1,1],subclus[subclus$Class==1,2],col="blue")

## ------------------------------------------------------------------------
plot(circle$Att1, circle$Att2, main="circle")
points(circle[circle$Class==0,1],circle[circle$Class==0,2],col="red")
points(circle[circle$Class==1,1],circle[circle$Class==1,2],col="blue")

## ------------------------------------------------------------------------
getIR = function(classData){
nClass0 <- sum(classData == 0)
nClass1 <- sum(classData == 1)
IR <- nClass1 / nClass0
IR 
}

IR_subcluss = getIR(subclus$Class)
IR_circle = getIR(circle$Class)

IR_subcluss
IR_circle

## ------------------------------------------------------------------------
#subcluss
pos <- (1:dim(subclus)[1])[subclus$Class==0]
neg <- (1:dim(subclus)[1])[subclus$Class==1]

SubclussCVperm_pos <- matrix(sample(pos,length(pos)), ncol=5, byrow=T)
SubclussCVperm_neg <- matrix(sample(neg,length(neg)), ncol=5, byrow=T)

SubclussCVperm <- rbind(SubclussCVperm_pos, SubclussCVperm_neg)

#circle

pos <- (1:dim(circle)[1])[circle$Class==0]
neg <- (1:dim(circle)[1])[circle$Class==1]

circleCVperm_pos <- matrix(sample(pos,length(pos)), ncol=5, byrow=T)
circleCVperm_neg <- matrix(sample(neg,length(neg)), ncol=5, byrow=T)

circleCVperm <- rbind(circleCVperm_pos, circleCVperm_neg)


## ------------------------------------------------------------------------
library(class)
knn.pred = NULL
for( i in 1:5){
  predictions <- knn(subclus[-SubclussCVperm[,i], -3], subclus[SubclussCVperm[,i], -3], subclus[-SubclussCVperm[,i], 3], k = 3)
  knn.pred <- c(knn.pred, predictions)
}
acc <- sum((subclus$Class[as.vector(SubclussCVperm)] == 0 & knn.pred == 1) 
           | (subclus$Class[as.vector(SubclussCVperm)] == 1 & knn.pred == 2)) / (sum(subclus$Class == 0) + sum(subclus$Class == 1))
tpr <- sum(subclus$Class[as.vector(SubclussCVperm)] == 0 & knn.pred == 1) / sum(subclus$Class == 0)
tnr <- sum(subclus$Class[as.vector(SubclussCVperm)] == 1 & knn.pred == 2) / sum(subclus$Class == 1)
gmean <- sqrt(tpr * tnr)
gmean

## ------------------------------------------------------------------------
knn.pred = NULL
for( i in 1:5){
  predictions <- knn(circle[-circleCVperm[,i], -3], circle[circleCVperm[,i], -3], circle[-circleCVperm[,i], 3], k = 3)
  knn.pred <- c(knn.pred, predictions)
}
acc <- sum((circle$Class[as.vector(circleCVperm)] == 0 & knn.pred == 1) 
           | (circle$Class[as.vector(circleCVperm)] == 1 & knn.pred == 2)) / (sum(circle$Class == 0) + sum(circle$Class == 1))
tpr <- sum(circle$Class[as.vector(circleCVperm)] == 0 & knn.pred == 1) / sum(circle$Class == 0)
tnr <- sum(circle$Class[as.vector(circleCVperm)] == 1 & knn.pred == 2) / sum(circle$Class == 1)
gmean <- sqrt(tpr * tnr)
gmean

## ------------------------------------------------------------------------
knn.pred = NULL
for( i in 1:5){
  
  train <- subclus[-SubclussCVperm[,i], -3]
  classes.train <- subclus[-SubclussCVperm[,i], 3] 
  test  <- subclus[SubclussCVperm[,i], -3]
  
  # randomly oversample the minority class (class 0)
  minority.indices <- (1:dim(train)[1])[classes.train == 0]
  to.add <- dim(train)[1] - 2 * length(minority.indices)
  duplicate <- sample(minority.indices, to.add, replace = T)
  for( j in 1:length(duplicate)){
    train <- rbind(train, train[duplicate[j],])
    classes.train <- c(classes.train, 0)
  }  
  
  # use the modified training set to make predictions
  predictions <-  knn(train, test, classes.train, k = 3)
  knn.pred <- c(knn.pred, predictions)
}
tpr.ROS <- sum(subclus$Class[as.vector(SubclussCVperm)] == 0 & knn.pred == 1) / sum(subclus$Class == 0)
tnr.ROS <- sum(subclus$Class[as.vector(SubclussCVperm)] == 1 & knn.pred == 2) / sum(subclus$Class == 1)
gmean.ROS <- sqrt(tpr.ROS * tnr.ROS)
gmean.ROS

## ------------------------------------------------------------------------
knn.pred = NULL
for( i in 1:5){
  
  train <- circle[-circleCVperm[,i], -3]
  classes.train <- circle[-circleCVperm[,i], 3] 
  test  <- circle[circleCVperm[,i], -3]
  
  # randomly oversample the minority class (class 0)
  minority.indices <- (1:dim(train)[1])[classes.train == 0]
  to.add <- dim(train)[1] - 2 * length(minority.indices)
  duplicate <- sample(minority.indices, to.add, replace = T)
  for( j in 1:length(duplicate)){
    train <- rbind(train, train[duplicate[j],])
    classes.train <- c(classes.train, 0)
  }  
  
  # use the modified training set to make predictions
  predictions <-  knn(train, test, classes.train, k = 3)
  knn.pred <- c(knn.pred, predictions)
}
tpr.ROS <- sum(circle$Class[as.vector(circleCVperm)] == 0 & knn.pred == 1) / sum(circle$Class == 0)
tnr.ROS <- sum(circle$Class[as.vector(circleCVperm)] == 1 & knn.pred == 2) / sum(circle$Class == 1)
gmean.ROS <- sqrt(tpr.ROS * tnr.ROS)
gmean.ROS

## ------------------------------------------------------------------------
knn.pred = NULL
for( i in 1:5){
  
  train <- subclus[-SubclussCVperm[,i], -3]
  classes.train <- subclus[-SubclussCVperm[,i], 3] 
  test  <- subclus[SubclussCVperm[,i], -3]
  
  # randomly undersample the minority class (class 1)
  majority.indices <- (1:dim(train)[1])[classes.train == 1]
  to.remove <- 2* length(majority.indices) - dim(train)[1]
  remove <- sample(majority.indices, to.remove, replace = F)
  train <- train[-remove,] 
  classes.train <- classes.train[-remove]
  
  # use the modified training set to make predictions
  predictions <-  knn(train, test, classes.train, k = 3)
  knn.pred <- c(knn.pred, predictions)
}
tpr.RUS <- sum(subclus$Class[as.vector(SubclussCVperm)] == 0 & knn.pred == 1) / sum(subclus$Class == 0)
tnr.RUS <- sum(subclus$Class[as.vector(SubclussCVperm)] == 1 & knn.pred == 2) / sum(subclus$Class == 1)
gmean.RUS <- sqrt(tpr.RUS * tnr.RUS)
gmean.RUS

## ------------------------------------------------------------------------
knn.pred = NULL
for( i in 1:5){
  
  train <- circle[-circleCVperm[,i], -3]
  classes.train <- circle[-circleCVperm[,i], 3] 
  test  <- circle[circleCVperm[,i], -3]
  
  # randomly undersample the minority class (class 1)
  majority.indices <- (1:dim(train)[1])[classes.train == 1]
  to.remove <- 2* length(majority.indices) - dim(train)[1]
  remove <- sample(majority.indices, to.remove, replace = F)
  train <- train[-remove,] 
  classes.train <- classes.train[-remove]
  
  # use the modified training set to make predictions
  predictions <-  knn(train, test, classes.train, k = 3)
  knn.pred <- c(knn.pred, predictions)
}
tpr.RUS <- sum(circle$Class[as.vector(circleCVperm)] == 0 & knn.pred == 1) / sum(circle$Class == 0)
tnr.RUS <- sum(circle$Class[as.vector(circleCVperm)] == 1 & knn.pred == 2) / sum(circle$Class == 1)
gmean.RUS <- sqrt(tpr.RUS * tnr.RUS)
gmean.RUS

## ------------------------------------------------------------------------
distance <- function(i, j, data){
  sum <- 0
  for(f in 1:dim(data)[2]){
    if(is.factor(data[,f])){ # nominal feature
      if(data[i,f] != data[j,f]){
        sum <- sum + 1
      }
    } else {
      sum <- sum + (data[i,f] - data[j,f]) * (data[i,f] - data[j,f])
    }
  }
  sum <- sqrt(sum)
  return(sum)
}

## ------------------------------------------------------------------------
getNeighbours = function(x,minorityIndices,train){
  minority = train[minorityIndices,]
  distances = NULL
  for(i in 1:(dim(minority)[1])){
    distances = c(distances, distance(x,rownames(minority[i,]),train))
  }
  indexInMinority = which(distances %in% sort(distances)[1:5])
  return(minority[indexInMinority,])
}
 

## ------------------------------------------------------------------------
syntheticInstance = function(x,nearestNeighbours){
  chosenNeighbourIndex = ceiling(runif(1)*5)
  chosenNeighbour = nearestNeighbours[chosenNeighbourIndex,]
  result =  NULL
  for(i in 1:length(x)){
    if(is.factor(x[i])){
      result = c(result, ifelse(runif(1) > 0.5, x[i],chosenNeighbour[i]))
    }
    else{
      increment = abs(as.numeric(x[i])-as.numeric(chosenNeighbour[i]))*runif(1)
      minimum = min(as.numeric(x[i]),as.numeric(chosenNeighbour[i]))
      result = c(result,minimum+increment)
    }
  }
  return(result)
}


## ------------------------------------------------------------------------

smote = function(data,labelIndex){
  minorityIndexes = which(data[,labelIndex] == 0)
  
  for(ir in 1:ceiling(getIR(data[,labelIndex]))){
    for(i in 1:length(minorityIndexes)){
      nnI = getNeighbours(minorityIndexes[i],minorityIndexes[-i],data)
      siI = syntheticInstance(data[minorityIndexes[i],],nnI)
      data = rbind(data,siI)
    }
  }
  
  return(data)
}

## ------------------------------------------------------------------------
data2 = smote(subclus,3)
plot(data2$Att1, data2$Att2, main="Subcluss2")
points(data2[data2$Class==0,1],data2[data2$Class==0,2],col="red")
points(data2[data2$Class==1,1],data2[data2$Class==1,2],col="blue")

## ------------------------------------------------------------------------
pos <- (1:dim(data2)[1])[data2$Class==0]
neg <- (1:dim(data2)[1])[data2$Class==1]

d2CVperm_pos <- matrix(sample(pos,length(pos)), ncol=5, byrow=T)
d2CVperm_neg <- matrix(sample(neg,length(neg)), ncol=5, byrow=T)

d2CVperm <- rbind(SubclussCVperm_pos, SubclussCVperm_neg)


knn.pred = NULL
for( i in 1:5){
  predictions <- knn(data2[-d2CVperm[,i], -3], data2[d2CVperm[,i], -3], data2[-d2CVperm[,i], 3], k = 3)
  knn.pred <- c(knn.pred, predictions)
}
acc <- sum((data2$Class[as.vector(d2CVperm)] == 0 & knn.pred == 1) 
           | (data2$Class[as.vector(d2CVperm)] == 1 & knn.pred == 2)) / (sum(data2$Class == 0) + sum(data2$Class == 1))
tpr <- sum(data2$Class[as.vector(d2CVperm)] == 0 & knn.pred == 1) / sum(data2$Class == 0)
tnr <- sum(data2$Class[as.vector(d2CVperm)] == 1 & knn.pred == 2) / sum(data2$Class == 1)
gmean <- sqrt(tpr * tnr)
gmean

## ------------------------------------------------------------------------
circle2 = smote(circle,3)
plot(circle2$Att1, circle2$Att2, main="circle2s")
points(circle2[circle2$Class==0,1],circle2[circle2$Class==0,2],col="red")
points(circle2[circle2$Class==1,1],circle2[circle2$Class==1,2],col="blue")

## ------------------------------------------------------------------------
pos <- (1:dim(circle2)[1])[circle2$Class==0]
neg <- (1:dim(circle2)[1])[circle2$Class==1]

circle2CVperm_pos <- matrix(sample(pos,length(pos)), ncol=5, byrow=T)
circle2CVperm_neg <- matrix(sample(neg,length(neg)), ncol=5, byrow=T)

circle2CVperm <- rbind(circle2CVperm_pos, circle2CVperm_neg)

knn.pred = NULL
for( i in 1:5){
  predictions <- knn(circle2[-circle2CVperm[,i], -3], circle2[circle2CVperm[,i], -3], circle2[-circle2CVperm[,i], 3], k = 3)
  knn.pred <- c(knn.pred, predictions)
}
acc <- sum((circle2$Class[as.vector(circle2CVperm)] == 0 & knn.pred == 1) 
           | (circle2$Class[as.vector(circle2CVperm)] == 1 & knn.pred == 2)) / (sum(circle2$Class == 0) + sum(circle2$Class == 1))
tpr <- sum(circle2$Class[as.vector(circle2CVperm)] == 0 & knn.pred == 1) / sum(circle2$Class == 0)
tnr <- sum(circle2$Class[as.vector(circle2CVperm)] == 1 & knn.pred == 2) / sum(circle2$Class == 1)
gmean <- sqrt(tpr * tnr)
gmean

