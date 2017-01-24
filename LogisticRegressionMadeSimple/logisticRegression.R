file <- "inserta el nombre aquí"

require("foreign")
dataFrame <- read.spss(file)

logisticRegression <-glm(varSalida ~ ., family ="binomial", data = dataFrame)

summary(logisticRegression)