#install.packages("arules")

library(arules)
data() #Check installed databases 
data("AdultUCI")
dim(AdultUCI)
AdultUCI[1:2,]

#Preprocess - delete redundant features
AdultUCI[["fnlwgt"]] = NULL
AdultUCI[["education-num"]] = NULL

#Cut information to transform them into labels, since algorithms are prepare for categorial variables

AdultUCI[[ "age"]] = ordered( cut( AdultUCI[[ "age"]], c(0,25,45,65,100) ) ,
                              labels= c ("Young", "Middle-aged", "Senior", "Old"))

AdultUCI[[ "hours-per-week"]] = ordered( cut( AdultUCI[[ "hours-per-week"]], c(0,25,40,60,168) ) , labels= c("Part-time", "Full-time", "Over-time", "Workaholic"))

AdultUCI[[ "capital-gain"]] = ordered( cut( AdultUCI[[ "capital-gain"]], c(-Inf,0,median(AdultUCI[[ "capital-gain"]][AdultUCI[[ "capital-gain"]]>0]), Inf) ) , labels= c("None", "Low", "High"))

AdultUCI[[ "capital-loss"]] = ordered( cut( AdultUCI[[ "capital-loss"]], c(-Inf,0, median(AdultUCI[[ "capital-loss"]][AdultUCI[[ "capital-loss"]]>0]), Inf) ) , labels= c("None", "Low", "High"))

#Change type form dataframe to transaction
Adult<-as(AdultUCI, "transactions")
Adult

summary(Adult)

itemFrequencyPlot(Adult, support = 0.1, cex.names=0.8)

iAdult<-apriori(Adult, parameter = list(support = 0.1, target="frequent"))
iAdult<-sort(iAdult, by="support") # Los ordenamosporel valor del soporte
inspect(head(iAdult, n=10)) # Inspeccionamoslos 10 primeros

size(iAdult)
barplot(table(size(iAdult)), xlab="itemsetsize", ylab="count")
inspect(iAdult[size(iAdult)==1])

#Get maximal frequent set
imaxAdult<-iAdult[is.maximal(iAdult)]
inspect(head(sort(imaxAdult, by="support")))

#Get closed frequent set
icloAdult<-iAdult[is.closed(iAdult)]
inspect(head(sort(icloAdult, by="support")))

barplot( c(frequent=length(iAdult), closed=length(icloAdult), maximal=length(imaxAdult)), ylab="count", xlab="itemsets")

#Get rules from apriori method
rules <-apriori(Adult, parameter = list(support = 0.1, confidence = 0.8, minlen= 2))
summary(rules)
inspect(head(rules))
quality(head(rules))

#Sort by confidence
rulesSorted= sort(rules, by = "confidence")
inspect(head(rulesSorted))

#Subsetting ruless
rulesRaceWhite<-subset(rules, subset = lhs %in% "race=White" & lift > 1.2)
inspect(head(rulesRaceWhite))

#Remove redundant rules 
subsetMatrix<-is.subset(rulesSorted, rulesSorted)
subsetMatrix[lower.tri(subsetMatrix, diag=T)] <-NA
redundant <-colSums(subsetMatrix, na.rm=T) >= 1
rulesPruned<-rulesSorted[!redundant] # remove redundant rules
inspect(head(rulesPruned))

#Interesting Measures
mInteres<-interestMeasure(rulesPruned, measure=c("hyperConfidence", "leverage" ,"phi", "gini"), transactions=Adult)
quality(rulesPruned) <-cbind(quality(rulesPruned), mInteres)
inspect(head(sort(rulesPruned, by="phi")))

#Visualize
install.packages("arulesViz")
library (arulesViz)
plot(rulesPruned)
plot(rulesPruned[1:6], method="graph", control=list(type="items"))
try: plot(rulesPruned, method="grouped", interactive=TRUE)
plot(rulesPruned[1:6], , method="paracoord", control=list(reorder=TRUE))

write(rulesPruned, file="reglas.csv", sep = ",", col.names=NA)
 
install.packages("pmml")
library(pmml)
write.PMML(rulesPruned, file="reglas.pmml")
reglasPMML= read.PMML("reglas.pmml")
