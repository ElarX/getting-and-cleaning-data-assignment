##Getting and Cleanining Data Assignment - course project

##Author
Leo Rozenfeld


### Overview
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. 


### Scripts in the project
run_analysis.R

###Notes about the script
Please ensure that you have downloaded the source data from 
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
and extracted it into a the working directory.

If you wish to simply use the script on another directory, please modify line 32 



### Project Summary
The following is a summary description of the project instructions

The script carries out the following:

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive activity names. 
5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

### Output of Scripts
2 variables are stored in memory:
data.frame activitySensorData (10299 x 68)
data.frame activitySensorDataSummary (180 x 68)

### Additional Information
Additional information about the variables, data and transformations in the CodeBook.MD file.

