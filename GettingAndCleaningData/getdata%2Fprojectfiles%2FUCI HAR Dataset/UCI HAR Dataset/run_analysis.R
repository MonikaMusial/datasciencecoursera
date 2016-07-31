library(reshape2)
library(plyr)
library(dplyr)

run_analysis <- function() {
  
  #  reading test and training files
  #
  setwd("C:/coursera/getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/test")
  subjectTest <- read.table("subject_test.txt", header = FALSE)
  labelsTest <- read.table("y_test.txt", header = FALSE)
  testSet <- read.table("X_test.txt", header = FALSE)
  
  setwd("C:/coursera/getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/train")
  subjectTrain <- read.table("subject_train.txt", header = FALSE)
  labelsTrain <- read.table("y_train.txt", header = FALSE)
  trainSet <- read.table("X_train.txt", header = FALSE)
  
  setwd("C:/coursera/getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/")
  features<-read.table("features.txt", header = FALSE)
  activityLabels<-read.table("activity_labels.txt", header = FALSE)
  
  #  merging train and test files
  #
  set<-rbind(trainSet,testSet)
  labelsSet<-rbind(labelsTrain,labelsTest)
  subjectSet<-rbind(subjectTrain,subjectTest)
  
  # adding descriptive activity names to name the activities in the data set
  #
  labelsSet$id  <- 1:nrow(labelsSet)
  act<-merge(activityLabels,labelsSet, by.x = "V1",by.y = "V1")
  act<-act[order(act$id), ]
  colnames(set)<-features$V2
  finalSet<-plyr::mutate(set,Activity=act$V2)
  
  # Extracts only the measurements on the mean and standard deviation for each measurement
  #
  indexes<-grep("std|mean|Mean",features$V2)
  indexes<-c(indexes,ncol(finalSet))
  finalSet<-finalSet[,indexes]
  # Tidy data set:
  finalSet
  
  # Creating tidy data set with the average of each variable for each activity and each subject
  #
  columns<-colnames(finalSet[,1:(ncol(finalSet)-1)])
  finalSet[] <- lapply(finalSet[], as.numeric)
  summarisedSet<-plyr::mutate(finalSet,Subject=subjectSet$V1)
  summarisedSet<-ddply(summarisedSet, c("Activity","Subject"), function(x) colMeans(x[columns]))
  # Tidy data set with the average of each variable for each activity and each subject
  summarisedSet
  
}

