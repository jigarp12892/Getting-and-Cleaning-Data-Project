makeTidyData <- function()
{
  
'Step 1 : setting data folder as working directory'
  
   setwd("C:/Users/Jigar Patel/Documents/UCI HAR Dataset")
   
'Step 2 : reading dataset,subject IDs and activity IDs for training and test data' 
   
  TestSet<-read.table("./test/X_test.txt",stringsAsFactors = FALSE)
  TestActivityID<-read.table("./test/y_test.txt",stringsAsFactors = FALSE) 
  TestSubject<-read.table("./test/subject_test.txt",stringsAsFactors = FALSE)

  TrainingSet<-read.table("./train/X_train.txt",stringsAsFactors = FALSE)
  TrainingActivityID<-read.table("./train/y_train.txt",stringsAsFactors = FALSE)
  TrainingSubject<-read.table("./train/subject_train.txt",stringsAsFactors = FALSE)
 


'Step 3 : Binding columns of test columns'

  AllTestData<-cbind(TestSubject,TestActivityID,TestSet)
  
'Step 4 : Binding columns of training columns'
  
  AllTrainingData<-cbind(TrainingSubject,TrainingActivityID,TrainingSet)

'Step 5 : Binding rows to merge all 563 cols of test and training'

AllData<-rbind(AllTestData,AllTrainingData)
  
'Step 6: add appropriate variable names'

  VariablesData <- read.table("./features.txt",stringsAsFactors = FALSE)
  colnames(AllData)<-c("Subject ID","Activity ID",VariablesData$V2)  
  
'Step 7 : Finding columns that contain "mean" or "std"'

  Extractcol <- grep(".mean.|.std.",VariablesData$V2,ignore.case = TRUE)
  
  FinalColumnNumbers <- c(1,2,(Extractcol+2))
  
'Step 8: Creating DataSet with Subject ID, Activity ID and measurements that contain MEAN or STD'

  ReducedData <- AllData[,FinalColumnNumbers]
  
'Step 9 Giving descriptive activity label names'

activityNames<-read.table("./activity_labels.txt", stringsAsFactors = FALSE)
colnames(activityNames)<-c("Activity number", "Activity ID")
install.packages("qdap")  
library("qdap")
ReducedData$`Activity ID`<-lookup(ReducedData$`Activity ID`,activityNames)

'Step 10 : Creating an independent tidy dataset'

tidyData <-ReducedData %>% group_by(`Subject ID`,`Activity ID`) %>% summarise_all(funs(mean))

write.table(tidyData,file = "./tidyData")
}

