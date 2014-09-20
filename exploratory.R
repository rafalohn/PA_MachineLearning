library(caret)
library(rattle)

testing<-read.csv("pml-testing.csv")
training<-read.csv("pml-training.csv")

columnsToRemove<-c('X','user_name','raw_timestamp_part_1','raw_timestamp_part_2','cvtd_timestamp')
cleanTraining <- training[,!(names(training) %in% columnsToRemove)]
remove(training)
remove(columnsToRemove)

columnCountNA<-colSums(is.na(cleanTraining))
cleanTraining <- cleanTraining[,columnCountNA< 10]
remove(columnCountNA)

columnCountEmpty<-colSums(cleanTraining=="")+colSums(cleanTraining=="#DIV/0!")
cleanTraining <- cleanTraining[,columnCountEmpty< 10]
remove(columnCountEmpty)


modFit<-train(classe ~.,method='rpart',data=cleanTraining)

fancyRpartPlot(modFit$finalModel)