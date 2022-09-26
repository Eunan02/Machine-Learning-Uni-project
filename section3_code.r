## @knitr Pr3.1
set.seed(42)
library(class)
library(rlang)
library(knitr)
library(randomForest)
library("rstudioapi")
library(MASS)
library(ggplot2)
library(caret)
setwd(dirname(getActiveDocumentContext()$path))
getwd()


#3.1

allFeats<-read.delim(file ="all_features.csv" ,header = F)
View(allFeats)
allFeats <- allFeats[sample(nrow(allFeats)),]
allFeats$folds <- cut(seq(1,nrow(allFeats)),breaks=kfoldsk,labels=FALSE)
allFeats$folds
train_control<-trainControl(method = "cv",number = 5,search = 'grid')
tune_grid <-expand.grid( .mtry=c(2,4,6,8))
tune_grid
x=25
results<-matrix(,ncol = 4,byrow = T)




model<-train(V1 ~V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18,data=allFeats,method="rf",trControl=train_control,   tuneGrid=tune_grid,ntree=x)
print(model)
results<-rbind(results,model$results$Accuracy)
plot((model),main=paste("random forest for ",x," Trees"),xlab="number of predictors")
for (i in 1:7)
{
  x<-x+50
  print(x)
  model<-train(V1 ~V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18,data=allFeats,method="rf",trControl=train_control,   tuneGrid=tune_grid,ntree=x)
  print(model)
  print(plot((model),main=paste("random forest for ",x," Trees"),xlab="number of predictors"))
  results<-rbind(results,model$results$Accuracy)
  
}
colnames(results)<-c(2,4,6,8)
rownames(results)<-c('',25,75,125,175,225,275,325,375)
results
## @knitr Pr3.2
#3.2 8 for 125
accs<-c()
for (i in 1:15)
{
  tune_grid <-expand.grid( .mtry=8)
  model<-train(V1 ~V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18,data=allFeats,method="rf",trControl=train_control,   tuneGrid=tune_grid,ntree=125)
  print(model)
accs<-append(accs,model$results$Accuracy)
  #accs<-c(accs+model$results$Accuracy)
}
print(accs)
accMean<-mean(accs)
accSd<-sd(accs)
accMean
accSd
t.test(accs,mu = 1/13, alternative = "two.sided")
