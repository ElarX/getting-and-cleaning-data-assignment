#################################################

##Getting and Cleaning Data - Course Project 
##Author: Leo Rozenfeld
##2016-01-30


#Notes:
#
#This script assumes that the contents of "UCI HAR Dataset" were downloaded, and unzipped, and that
#
#THE SCRIPT LINE #32 HAS TO BE MODIFIED to reflect the location of the data (i.e. where "UCCI HAR Dataset" root folder is)
#
#and is placed into the working directory. The dataset .zip file can be downloaded from
#https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
#
#Steps:
#0. Load required packages, load data
#1. Merges the training and the test sets to create one data set.
#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#3. Uses descriptive activity names to name the activities in the data set
#4. Appropriately labels the data set with descriptive variable names. 
#5. From the data set in step 4, creates a second, independent tidy data set with the 
#   average of each variable for each activity and each subject.
#
#Version 1.0.1 Last Updated 2016-01-31
#
#

#0. Load required packages, set working directory, load data
library(dplyr)
setwd("C:\\Users\\Leo\\Documents\\getting-and-cleaning-data-assignment\\UCI HAR Dataset")


## get required data into the program. Both for the test and train subjects
dataset1 <- read.table("./test/X_test.txt", header=FALSE, colClasses = "numeric")
subjects1<-read.table("./test/subject_test.txt", colClasses="numeric")
activityID1<-read.table("./test/y_test.txt", colClasses="numeric")

dataset2 <- read.table("./train/X_train.txt", header=FALSE, colClasses = "numeric")
subjects2<-read.table("./train/subject_train.txt", colClasses="numeric")
activityID2<-read.table("./train/y_train.txt", colClasses="numeric")

#1. Merges the training and the test sets to create one data set.

##Join the rows and columns together of the corresponding segments
dataset <-bind_cols(
    bind_rows(subjects1, subjects2), 
    bind_rows(activityID1, activityID2),
    bind_rows(dataset1, dataset2))

rm(dataset1, dataset2, subjects1, subjects2, activityID1, activityID2)

#2. Extracts only the measurements on the mean and standard deviation for each measurement. 

#read in the labels of the columns
columnNames <-read.table("features.txt", header=FALSE)

#add the leading the SubjectID and ActivityID column name
columnNames<-c("subjectID","activityID", as.character(columnNames$V2))

#convert the names into unique names- otherwise run into problem with dplyr select function
columnNames <-make.names(columnNames, unique=TRUE)
names(dataset) <- columnNames
rm(columnNames)


#select the columns containing either std or mean, and append subjects and activityID
activitySensorData <- select(dataset, subjectID, activityID, matches("(std|mean)\\.\\.(\\.[XYZ])?$"))
rm(dataset)


#3. Uses descriptive activity names to name the activities in the data set

#rename activities from activityID to meaningful activity names using a lookup table

#Make lookup table from activity_labels
lutDF <-read.table("activity_labels.txt", colClasses="character")
lut <- setNames(tolower(lutDF$V2), lutDF$V1)

activitySensorData$activityID<- as.factor(activitySensorData$activityID)     #convert to factors first
activitySensorData$activityID <-lut[activitySensorData$activityID]           #use these factors in a lookup table to convert to meaningful descriptors
activitySensorData<-rename(activitySensorData, activityType = activityID)    #Rename activityID into activityType
rm(lutDF, lut)


#4. Appropriately labels the data set with descriptive variable names. 

#remove punctuation from column Names
names(activitySensorData)<- gsub("[[:punct:]]","", names(activitySensorData))

# - clean up some other names
nameCol <-names(activitySensorData)

nameCol<-gsub("^t","time",nameCol)
nameCol<-gsub("^f","freq",nameCol)
nameCol<-gsub("[Ss]td","StdDev",nameCol)
nameCol<-gsub("mean","Mean",nameCol)
nameCol<-gsub("[Aa]cc","Accel",nameCol)
names(activitySensorData)<- nameCol
rm(nameCol)

#5. From the data set in step 4, creates a second, independent tidy data set with the 
#   average of each variable for each activity and each subject.

#do the grouping stuff using dplyr and create the second data set
activitySensorDataSummary <-{group_by(activitySensorData, activityType, subjectID) %>%
        summarize_each(funs(mean))
}
