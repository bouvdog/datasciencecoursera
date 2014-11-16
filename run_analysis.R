# Getting and Cleaning Data Course Project
#
# Kevin Newsom
# 10/26/2014

run_analysis<-function()
{
  setwd("E:/datascience")
  trainingData<-read.table("GCD/train/X_train.txt")
  trainingSubjects<-read.table("GCD/train/subject_train.txt")
  trainingActivities<-read.table("GCD/train/y_train.txt")
  
  testingData<-read.table("GCD/test/X_test.txt")
  testingSubjects<-read.table("GCD/test/subject_test.txt")
  testingActivities<-read.table("GCD/test/y_test.txt")
  
  columnHeaders<-read.table("GCD/features.txt")
  
  columnHeaders<-read.table("GCD/features.txt")
  columnHeaders<-columnHeaders[,2]
  
  # Covert to more readable variable names
  # Appropriately labels the data set with descriptive variable names.
  columnHeaders<-gsub("^t", "Time.", columnHeaders)
  columnHeaders<-gsub("^f","FFT.", columnHeaders)
  columnHeaders<-gsub("-", ".", columnHeaders)
  columnHeaders<-gsub("\\(\\)","", columnHeaders)
  columnHeaders<-gsub("mean", "Mean", columnHeaders)
  columnHeaders<-gsub("std", "Std", columnHeaders)
 
  activityLabels<-read.table("GCD/activity_labels.txt", colClasses="character")
  
  # Add column names on the data
  colnames(trainingData)<-columnHeaders
  colnames(testingData)<-columnHeaders
  
  # Extracts only the measurements on the mean and standard deviation 
  # for each measurement
  trainingData<-trainingData[,grepl("Mean", names(trainingData)) | 
                               grepl("Std", names(trainingData))]
  
  testingData<-testingData[,grepl("Mean", names(testingData)) | 
                               grepl("Std", names(testingData))]
  
  # Add the subject id number and an appropriate column header
  trainingData$Subjects<-trainingSubjects[,1]
  testingData$Subjects<-testingSubjects[,1]
  
  # Want names of activites not key numbers for activities
  # Uses descriptive activity names to name the activities in the data set
  activities<-trainingActivities[,1]
  trainingNamedActivities <- vector("character", length=nrow(trainingActivities))
  j<-1
  for (i in activities)
  {
    activityName<-activityLabels[i,2]
    trainingNamedActivities[j]<-activityName
    j<-j+1
  }
  
  # Add the activity names to the data frame
  trainingData$Activities<-trainingNamedActivities
  
  # Want names of activites not key numbers for activities
  activities<-testingActivities[,1]
  testNamedActivities <- vector("character", length=nrow(testingActivities))
  j<-1
  for (i in activities)
  {
    activityName<-activityLabels[i,2]
    testNamedActivities[j]<-activityName
    j<-j+1
  }
  
  # Add the activity names to the data frame
  testingData$Activities<-testNamedActivities
  
  # Merges the training and the test sets to create one data set.
  merged<-rbind(testingData, trainingData)
  
  # From the data set in step 4, creates a second, independent tidy 
  # data set with the average of each variable for each activity and each subject.
  tidyDataWithAveragedVariables = aggregate(merged[,names(merged) != c("Subjects", "Activities")],
                          by=list(Activities=merged$Activities,
                                  Subjects = merged$Subjects),
                          mean);
  
  write.table(tidyDataWithAveragedVariables, './tidyData.txt',row.names=FALSE,sep=',');
}