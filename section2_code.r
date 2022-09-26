## @knitr Pr2.1
set.seed(42)
library(class)
library(rlang)
library(knitr)
library("rstudioapi")
library(MASS)
library(ggplot2)
library(caret)
setwd(dirname(getActiveDocumentContext()$path))
getwd()

csvF<-read.csv(file ="40293751_features.csv" ,header = TRUE)
#kNeighbourMat<-Matrix(csvF[1:8,3])

kNeighbourMat=csvF[csvF$label %in% c('a','j','sad','smiley','xclaim'),]
kNeighbourMat$dummy.class[kNeighbourMat$label %in% c('a','j')]<-"letter"
kNeighbourMat$dummy.class[kNeighbourMat$label=="sad"]<-"sad"
kNeighbourMat$dummy.class[kNeighbourMat$label=="smiley"]<-"smiley"
kNeighbourMat$dummy.class[kNeighbourMat$label=="xclaim"]<-"xclaim"
train.X = cbind(kNeighbourMat$no_neigh_right,kNeighbourMat$aspect_ratio,kNeighbourMat$no_neigh_above,kNeighbourMat$connected_areas)
View(train.X)
ks = c(1,3,5,7,9,11,13)
accuracies = c()
for (kk in ks){
 
  if (kk / 2 !=0)
 
    {
    print(kk)
    knn1=knn(train.X,train.X,kNeighbourMat$dummy.class,k=kk)
  print(table(knn1,kNeighbourMat$dummy.class))
  accuracies = cbind(accuracies, mean(knn1==kNeighbourMat$dummy.class))
  }
}
accuracies

## @knitr Pr2.2
#2.2
kfoldsk=5
kNeighbourMat=kNeighbourMat

kNeighbourMat<-kNeighbourMat[sample(nrow(kNeighbourMat)),]
kNeighbourMat$folds<- cut(seq(1,nrow(kNeighbourMat)),breaks=kfoldsk,labels=FALSE)
View(kNeighbourMat)
kNeighbourMat<-kNeighbourMat[sample(nrow(kNeighbourMat)),]
kNeighbourMat$folds <- cut(seq(1,nrow(kNeighbourMat)),breaks=kfoldsk,labels=FALSE)
train_control <- trainControl(method="cv", number=kfoldsk)

tune_grid <- expand.grid(k = ((1:7)*2) -1)
tune_grid

# train the model
model <- train(dummy.class~no_neigh_right+aspect_ratio+no_neigh_above+connected_areas, data=kNeighbourMat, 
               trControl=train_control, tuneGrid=tune_grid, method="knn")
# summarize results
print(model)

## @knitr Pr2.3
#2.3
tune_grid<- expand.grid(k = 13)#best value of k
newModel<-train(dummy.class~no_neigh_right+aspect_ratio+no_neigh_above+connected_areas, data=kNeighbourMat, 
                trControl=train_control, tuneGrid=tune_grid, method="knn")
knnPredict<-predict(newModel,newdata=kNeighbourMat)
ActualData <- as.factor(kNeighbourMat[["dummy.class"]])
cm=confusionMatrix(knnPredict,ActualData)
cm
#2.4
x1<-model$results$k
y1<-model$results$Accuracy
y2<-accuracies

ks<-c(1,1/3,1/5,1/7,1/9,1/11,1/13)
plot(ks,y1,type="o",ylim=c(0,1),col="blue",ylab="accuracy rate",xlab="1/k")
lines(ks,type="o",y2,col="red")
legend(x = "bottomleft",          
       legend = c("training set", "cv classification set"),lty = c(1, 1),col=c("red","blue"))