#################################################

##Getting and Cleaning Data - Course Project 
##Author: Leo Rozenfeld
##2016-01-30


#Notes:
#
#This script assumes that the contents of "UCI HAR Dataset" are unzipped into a folder named "UCI HAR Dataset"
#and placed into the root directory 
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
#Version 1.0. Last Updated 2016-01-30
#
#

#0. Load required packages, load data
library(dplyr)


## get required data into the program. Both for the test and train subjects
dataset1 <- read.table("./UCI HAR Dataset/test/X_test.txt", header=FALSE, colClasses = "numeric")
subjects1<-read.table("./UCI HAR Dataset/test/subject_test.txt", colClasses="numeric")
activityID1<-read.table("./UCI HAR Dataset/test/y_test.txt", colClasses="numeric")

dataset2 <- read.table("./UCI HAR Dataset/train/X_train.txt", header=FALSE, colClasses = "numeric")
subjects2<-read.table("./UCI HAR Dataset/train/subject_train.txt", colClasses="numeric")
activityID2<-read.table("./UCI HAR Dataset/train/y_train.txt", colClasses="numeric")

#1. Merges the training and the test sets to create one data set.

##Join the rows and columns together of the corresponding segments
dataset <-bind_cols(
          bind_rows(subjects1, subjects2), 
          bind_rows(activityID1, activityID2),
          bind_rows(dataset1, dataset2))

rm(dataset1, dataset2, subjects1, subjects2, activityID1, activityID2)

#2. Extracts only the measurements on the mean and standard deviation for each measurement. 

#read in the labels of the columns
columnNames <-read.table("./UCI HAR Dataset/features.txt", header=FALSE)

#add the leading the SubjectID and ActivityID column name
columnNames<-c("subjectID","activityID", as.character(columnNames$V2))

#convert the names into unique names- otherwise run into problem with dplyr select function
columnNames <-make.names(columnNames, unique=TRUE)
names(dataset) <- columnNames
rm(columnNames)


#select the columns containing either std or mean, and append subjects and activityID
tidyDataset <- select(dataset, subjectID, activityID, matches("(std|mean)\\.\\.(\\.[XYZ])?$"))
rm(dataset)


#3. Uses descriptive activity names to name the activities in the data set

#rename activities from activityID to meaningful activity names using a lookup table

#Make lookup table from activity_labels
lutDF <-read.table("./UCI HAR Dataset/activity_labels.txt", colClasses="character")
lut <- setNames(tolower(lutDF$V2), lutDF$V1)

tidyDataset$activityID<- as.factor(tidyDataset$activityID)     #convert to factors first
tidyDataset$activityID <-lut[tidyDataset$activityID]           #use these factors in a lookup table to convert to meaningful descriptors
tidyDataset<-rename(tidyDataset, activityType = activityID)    #Rename activityID into activityType
rm(lutDF, lut)


#4. Appropriately labels the data set with descriptive variable names. 

#remove punctuation from column Names
names(tidyDataset)<- gsub("[[:punct:]]","", names(tidyDataset))

# - clean up some other names
nameCol <-names(tidyDataset)

nameCol<-gsub("^t","time",nameCol)
nameCol<-gsub("^f","freq",nameCol)
nameCol<-gsub("[Ss]td","StdDev",nameCol)
nameCol<-gsub("mean","Mean",nameCol)
nameCol<-gsub("[Aa]cc","Accel",nameCol)
names(tidyDataset)<- nameCol
rm(nameCol)

#5. From the data set in step 4, creates a second, independent tidy data set with the 
#   average of each variable for each activity and each subject.

#do the grouping stuff using dplyr and create the second data set
tidyDataSummary <-{group_by(tidyDataset, activityType, subjectID) %>%
        summarize_each(funs(mean))
}
