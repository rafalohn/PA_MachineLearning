library(caret)
library(plyr)
library(randomForest)

testing<-read.csv("pml-testing.csv")
training<-read.csv("pml-training.csv")

columnsToRemove<-c('X','user_name','raw_timestamp_part_1','raw_timestamp_part_2','cvtd_timestamp','new_window')
training <- training[,!(names(training) %in% columnsToRemove)]
testing <- testing[,!(names(testing) %in% columnsToRemove)]
remove(columnsToRemove)

columnCountNA<-colSums(is.na(training))
training <- training[,columnCountNA< 10]
testing <- testing[,columnCountNA< 10]
remove(columnCountNA)

columnCountEmpty<-colSums(training=="")+colSums(training=="#DIV/0!")
training <- training[,columnCountEmpty< 10]
testing <- testing[,columnCountEmpty< 10]
remove(columnCountEmpty)

model <- randomForest(classe ~ ., data=training)
prediction<-predict(model, newdata=testing[-55])
answers<- as.character(prediction)  
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answers)