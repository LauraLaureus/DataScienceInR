# import the CSV file
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)

#names(wbcd)<- c("ID","Diagnosis","radius","texture","perimeter","area","smoothness","compactness","concavity","concave points","symmetry","fractal dimension")

# examine the structure of the wbcd data frame
str(wbcd)

# drop the id feature
wbcd <- wbcd[,-1]

# table of diagnosis
table(wbcd$diagnosis)
# recode diagnosis as a factor
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))

# table or proportions with more informative labels
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

# summarize three numeric features
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#normalize data
wbcd_n <- as.data.frame(lapply(wbcd[,2:31], normalize))

summary(wbcd_n$area_mean)

#visualize data
plot(wbcd[,2:5])
plot(wbcd_n[,1:4], col=wbcd[,1])

#check correlation
cor(wbcd[,2:5])

# create training and test data
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

# create labels for training and test data
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

# load the "class" library
library(class)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=21)

## Evaluating model performance ----
table(wbcd_test_pred,wbcd_test_labels)


##USING CARET
require("caret")
knnModel <- train(x = wbcd[1:469,-1], y = wbcd[1:469,1], method = "knn", preProc = c("center","scale"))
knnModel                                                                                     

knnFit <- train(wbcd_train, wbcd_train_labels, method="knn", metric="Accuracy", tuneGrid = data.frame(.k=1:15))
knnFit

knnPred <- predict(knnModel, newdata = wbcd_test)
knnPred

postResample(pred = knnPred, obs = testData$y)

#Exercise: Try with different k choices and do a quick comparison
knnFit <- train(wbcd_train, wbcd_train_labels, method="knn", metric="Accuracy", tuneGrid = data.frame(.k=1:35))
plot(knnFit,main ="Presición vs k", ylab= "Presición", xlab="K(vecinos)", pch= 20)


