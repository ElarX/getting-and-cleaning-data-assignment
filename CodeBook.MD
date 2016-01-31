##Getting and Cleanining Data Assignment - course project

##Author
Leo Rozenfeld

###Description
This codebook describes the variables, the data,
and any transformations or work that you performed to clean up the data for the project

###Source Data
A complete description of the data set can be obtained here:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

###Data Set information - from the README file provided with the dataset
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

###Original Dataset attributes
Original Data in the dataset provided for each record:

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

##Data Analysis Description


###1. Merges the training and the test sets to create one data set.
Observations data was extracted from the following files. 10299 observations total.

./test/X_test.txt
./test/subject_test.txt
.test/y_test.txt

./train/X_train.txt
./train/subject_train.txt
./y_train.txt

Column information data was extracted from the following file
features.txt

Look up table to convert activityID into activity type was extracted from
./activity_labels.txt


###2. Extracts only the measurements on the mean and standard deviation for each measurement. 
Used dplyr package to select columns whose names ended with mean or std (with or without an XYZ)

###3. Uses descriptive activity names to name the activities in the data set
Converted ActivityID into meaningful factors using a lookup table made from activity_labels.txt  

###4. Appropriately labels the data set with descriptive variable names. 
Used gsub function on column names to clean up some labels. 
t prefix was converted into time
f prefix was converted into freq
std -> StdDev
mean->Mean
Acc->Accel

Resultant tidy dataset is stored in memory as "activitySensorData", a data.frame of 10299obs x 68 variables

###The following variables are present in the activitySensorData Dataset
 [1] "subjectID"                     
 [2] "activityType"                  
 [3] "timeBodyAccelMeanX"            
 [4] "timeBodyAccelMeanY"            
 [5] "timeBodyAccelMeanZ"            
 [6] "timeBodyAccelStdDevX"          
 [7] "timeBodyAccelStdDevY"          
 [8] "timeBodyAccelStdDevZ"          
 [9] "timeGravityAccelMeanX"         
[10] "timeGravityAccelMeanY"         
[11] "timeGravityAccelMeanZ"         
[12] "timeGravityAccelStdDevX"       
[13] "timeGravityAccelStdDevY"       
[14] "timeGravityAccelStdDevZ"       
[15] "timeBodyAccelJerkMeanX"        
[16] "timeBodyAccelJerkMeanY"        
[17] "timeBodyAccelJerkMeanZ"        
[18] "timeBodyAccelJerkStdDevX"      
[19] "timeBodyAccelJerkStdDevY"      
[20] "timeBodyAccelJerkStdDevZ"      
[21] "timeBodyGyroMeanX"             
[22] "timeBodyGyroMeanY"             
[23] "timeBodyGyroMeanZ"             
[24] "timeBodyGyroStdDevX"           
[25] "timeBodyGyroStdDevY"           
[26] "timeBodyGyroStdDevZ"           
[27] "timeBodyGyroJerkMeanX"         
[28] "timeBodyGyroJerkMeanY"         
[29] "timeBodyGyroJerkMeanZ"         
[30] "timeBodyGyroJerkStdDevX"       
[31] "timeBodyGyroJerkStdDevY"       
[32] "timeBodyGyroJerkStdDevZ"       
[33] "timeBodyAccelMagMean"          
[34] "timeBodyAccelMagStdDev"        
[35] "timeGravityAccelMagMean"       
[36] "timeGravityAccelMagStdDev"     
[37] "timeBodyAccelJerkMagMean"      
[38] "timeBodyAccelJerkMagStdDev"    
[39] "timeBodyGyroMagMean"           
[40] "timeBodyGyroMagStdDev"         
[41] "timeBodyGyroJerkMagMean"       
[42] "timeBodyGyroJerkMagStdDev"     
[43] "freqBodyAccelMeanX"            
[44] "freqBodyAccelMeanY"            
[45] "freqBodyAccelMeanZ"            
[46] "freqBodyAccelStdDevX"          
[47] "freqBodyAccelStdDevY"          
[48] "freqBodyAccelStdDevZ"          
[49] "freqBodyAccelJerkMeanX"        
[50] "freqBodyAccelJerkMeanY"        
[51] "freqBodyAccelJerkMeanZ"        
[52] "freqBodyAccelJerkStdDevX"      
[53] "freqBodyAccelJerkStdDevY"      
[54] "freqBodyAccelJerkStdDevZ"      
[55] "freqBodyGyroMeanX"             
[56] "freqBodyGyroMeanY"             
[57] "freqBodyGyroMeanZ"             
[58] "freqBodyGyroStdDevX"           
[59] "freqBodyGyroStdDevY"           
[60] "freqBodyGyroStdDevZ"           
[61] "freqBodyAccelMagMean"          
[62] "freqBodyAccelMagStdDev"        
[63] "freqBodyBodyAccelJerkMagMean"  
[64] "freqBodyBodyAccelJerkMagStdDev"
[65] "freqBodyBodyGyroMagMean"       
[66] "freqBodyBodyGyroMagStdDev"     
[67] "freqBodyBodyGyroJerkMagMean"   
[68] "freqBodyBodyGyroJerkMagStdDev" 

Further details about the labels can be interpreted from the features_info.txt file


###5. From the data set in step 4, creates a second, independent tidy data set with the 
   average of each variable for each activity and each subject.
dplyr package group_by function was used to group data from activitySensorData dataset by type of activity, followed by subjectID. 
Corresponding means of all sensory data

Resultant tidy data set is stored in memory as ActivitySensorDataSummary, a data.frame of 180obs x 68 variables
It contains the same variables as activitySensorData dataset described above.
