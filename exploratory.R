library(caret)
testing<-read.csv("pml-testing.csv")
training<-read.csv("pml-training.csv")
columnNAs<-colSums(is.na(training))
cleanTraining <- training[,columnNAs< 19000]
columnEmpty<-colSums(cleanTraining=="")+colSums(cleanTraining=="#DIV/0!")
cleanTraining <- cleanTraining[,columnEmpty< 19000]
cleanTraining <- cleanTraining[,!(names(cleanTraining) %in% 'cvtd_timestamp')]