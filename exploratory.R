library(caret)
library(rattle)

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

set.seed(12541)
inTrain<-createDataPartition(data$classe,p=0.75,list=FALSE)
training<-data[inTrain,]
testing<-data[-inTrain,]

modFitRpart<-train(classe ~.,method='rpart',data=training)
fancyRpartPlot(modFitRpart$finalModel)
prediction<-predict(modFitRpart,newdata=testing)
sum(prediction==testing$classe)/dim(testing)[1]
##0.57

library(randomForest)
modFitRandomForest <- randomForest(classe ~ .,   data=training)
print(modFitRandomForest) # view results 
importance(modFitRandomForest) # importance of each predictor
randomForestPrediction<-predict(modFitRandomForest,newdata=testing)
sum(randomForestPrediction==testing$classe)/dim(testing)[1]

modFitGmb<-train(classe ~., method="gbm",data=training,verbose=FALSE)
print(modFitGmb)
gmbPrediction<-predict(modFitGmb,newdata=testing)
sum(gmbPrediction==testing$classe)/dim(testing)[1]