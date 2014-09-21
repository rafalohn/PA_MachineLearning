library(caret)
library(plyr)
library(randomForest)

#testing<-read.csv("pml-testing.csv")
data<-read.csv("pml-training.csv")

columnsToRemove<-c('X','user_name','raw_timestamp_part_1','raw_timestamp_part_2','cvtd_timestamp')
data <- data[,!(names(data) %in% columnsToRemove)]
remove(columnsToRemove)
columnCountNA<-colSums(is.na(data))
data <- data[,columnCountNA< 10]
remove(columnCountNA)
columnCountEmpty<-colSums(data=="")+colSums(data=="#DIV/0!")
data <- data[,columnCountEmpty< 10]
remove(columnCountEmpty)

#inTrain<-createDataPartition(data$classe,p=0.75,list=FALSE)
#training<-data[inTrain,]
#testing<-data[-inTrain,]

#modFitRpart<-train(classe ~.,method='rpart',data=training)
#fancyRpartPlot(modFitRpart$finalModel)
#prediction<-predict(modFitRpart,newdata=testing)
#sum(prediction==testing$classe)/dim(testing)[1]

#modFitRandomForest <- randomForest(classe ~ ., data=training)
#randomForestPrediction<-predict(modFitRandomForest,newdata=testing)
#sum(randomForestPrediction==testing$classe)/dim(testing)[1]

#modFitGmb<-train(classe ~., method="gbm",data=training,verbose=FALSE)
#gmbPrediction<-predict(modFitGmb,newdata=testing)
#sum(gmbPrediction==testing$classe)/dim(testing)[1]

###############
k = 5
data$id <- sample(1:k, nrow(data), replace = TRUE)
list <- 1:k
##################################RPart
predictionTotal <- data.frame()
testTotal <- data.frame()

for (i in 1:k){  
  training <- subset(data, id %in% list[-i])
  testCurrent <- subset(data, id %in% c(i))
  model <-train(classe ~.,method='rpart',data=training)
  predictionCurrent <- as.data.frame(predict(model, testCurrent[,-55]))
  predictionTotal <- rbind(predictionTotal, predictionCurrent)
  testTotal <- rbind(testTotal, as.data.frame(testCurrent[,55]))
}
result <- cbind(predictionTotal, testTotal[, 1])
names(result) <- c("Predicted", "Actual")
rPart<-sum(result$Actual==result$Predicted)/dim(result)[1]
############################################
##################################Random Forest
predictionTotal <- data.frame()
testTotal <- data.frame()

for (i in 1:k){  
  training <- subset(data, id %in% list[-i])
  testCurrent <- subset(data, id %in% c(i))
  model <- randomForest(training$classe ~ ., data = training)
  predictionCurrent <- as.data.frame(predict(model, testCurrent[,-55]))
  predictionTotal <- rbind(predictionTotal, temp)
  testTotal <- rbind(testTotal, as.data.frame(testCurrent[,55]))
}
result <- cbind(predictionTotal, testTotal[, 1])
names(result) <- c("Predicted", "Actual")
randomForest<-sum(result$Actual==result$Predicted)/dim(result)[1]
randomForest
############################################
##################################GBM
predictionTotal <- data.frame()
testTotal <- data.frame()

for (i in 1:k){  
  training <- subset(data, id %in% list[-i])
  testCurrent <- subset(data, id %in% c(i))
  model <-train(classe ~., method="gbm",data=training,verbose=FALSE)
  predictionCurrent <- as.data.frame(predict(model, testCurrent[,-55]))
  predictionTotal <- rbind(predictionTotal, temp)
  testTotal <- rbind(testTotal, as.data.frame(testCurrent[,55]))
}
result <- cbind(predictionTotal, testTotal[, 1])
names(result) <- c("Predicted", "Actual")
randomForest<-sum(result$Actual==result$Predicted)/dim(result)[1]
randomForest
############################################