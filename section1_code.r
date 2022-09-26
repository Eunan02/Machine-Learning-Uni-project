## @knitr Pr1.1
set.seed(42)
library(rlang)
library(knitr)
library("rstudioapi")
library(MASS)
library(ggplot2)
library(caret)

search()
setwd(dirname(getActiveDocumentContext()$path))
getwd()

csvF<-read.csv(file ="40293751_features.csv" ,header = TRUE)

  #Add dummy value 1 for letters, so it discriminates between letters and non letters
  csvF$dummy.letters<-0
  print(csvF$dummy.letters)
  csvF[1:80,19]<-1
  
  #Shuffle the rows, so the first 80% of the shuffled data could be training data making it random
  #and last 20% test data
  features_shuffled<-csvF[sample(nrow(csvF)),]
  
  #first 80% will be used as training data, last 20% test data
  trainingdata=features_shuffled[1:112,]
  testData=features_shuffled[113:140,]
  View(features_shuffled)
  plt <- ggplot(trainingdata, aes(x=nr_pix, fill=as.factor(dummy.letters))) +
    geom_histogram(binwidth=.2, alpha=.5, position='identity')
  plot(plt)
  
  #1.1
  glmFit<-glm(dummy.letters ~ nr_pix+aspect_ratio, 
              data = trainingdata, 
              family = 'binomial') 
  
  print(summary(glmFit))
  
 
  
  dataframematrix <- matrix(nrow=28,ncol=2)
  dataframematrix[,1] <- testData$nr_pix
  dataframematrix[,2] <- testData$aspect_ratio
  dataframematrix
  newdata <-as.data.frame(dataframematrix)
  colnames(newdata) = c("nr_pix","aspect_ratio")
  predicted = predict(glmFit,newdata,type="response")
  print(predicted)
  testData[["predicted_val"]] = predict(glmFit, testData, type="response")
  testData[["predicted_class"]] = 0
  testData[["predicted_class"]][testData[["predicted_val"]] > 0.5] = 1

  
  nrow(testData[correct_items,])/nrow(testData)
  PredictedData <- as.factor(testData[["predicted_class"]])
  PredictedData
  ActualData <- as.factor(testData[["dummy.letters"]])
  ActualData
  confMatrix <- confusionMatrix(PredictedData,ActualData)
  confMatrix
  confMatrix <- confusionMatrix(PredictedData,ActualData,mode = "prec_recall")
  confMatrix
 
  correct_items
  
#1.2
  ## @knitr Pr1.2
  kfoldsk = 5
  



ggplot(csvF, aes(x=nr_pix, y=aspect_ratio, color=dummy.letters)) +
  geom_point(alpha=.5, position='identity')


View(csvF)


train_control<-trainControl(method="cv",number=kfoldsk,savePredictions = T, classProbs = TRUE)
csvF$is.letter <- "no"
csvF$is.letter
csvF$is.letter[csvF$dummy.letters == 1] <- "yes"
csvF$is.letter



model<-train(is.letter~nr_pix+aspect_ratio, data = csvF,trControl=train_control,method="glm",family = "binomial")
print(model)

model$results
summary(model)
mean(model$pred$pred==model$pred$obs)

cm = confusionMatrix(table(model$pred$yes >= 0.5,
                           model$pred$obs == "yes")) 
cm
cm = confusionMatrix(table(model$pred$yes >= 0.5,
                           model$pred$obs == "yes"),mode = "prec_recall") 
cm
#1.3
library(MLeval)
res <- evalm(model)
res$roc
