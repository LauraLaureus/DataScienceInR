data.frame(names(subsetTrainImputed),c(names(test4),NA))

test4Levels = c()
for (c in 1:dim(test4)[2]){
  if(is.factor(test4[,c])){
    test4Levels = c(test4Levels, length(levels(test4[,c])))
  }else
    test4Levels = c(test4Levels,NA)
}
test4Levels

subSetImputedLevels = c()
for (c in 1:dim(subsetTrainImputed)[2]){
  if(is.factor(subsetTrainImputed[,c])){
    subSetImputedLevels = c(subSetImputedLevels, length(levels(subsetTrainImputed[,c])))
  }else
    subSetImputedLevels = c(subSetImputedLevels,NA)
}
subSetImputedLevels

comparison = data.frame(subSetImputedLevels,c(test4Levels,NA))
#Variable número 11 es la culpable de todos tus males

