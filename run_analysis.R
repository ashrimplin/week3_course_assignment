#load the dplyr library for analysis 
library(dplyr)

#Create unique data frames for each of the files to be used for the analysis
x_test <- read.table("X_test.txt",header = FALSE)
x_train <- read.table("X_train.txt",header = FALSE)
y_test <- read.table("Y_test.txt",header = FALSE)
y_train <- read.table("Y_train.txt",header = FALSE)
subject_test <- read.table("subject_test.txt",header = FALSE)
subject_train <- read.table("subject_train.txt",header = FALSE)
features <- read.table("features.txt",header = FALSE)

#Extracts only the measurements on the mean and standard deviation for each measurement. 
x_test <- select(x_test, 1,	2,	3,	4,	5,	6,	41,	42,	43,	44,	45,	46,	81,	82,	83,	84,	85,	86,	121,	122,	123,	124,	125,	126,	161,	162,	163,	164,	165,	166,	201,	202,	214,	215,	227,	228,	240,	241,	253,	254,	266,	267,	268,	269,	270,	271,	294,	295,	296,	345,	346,	347,	348,	349,	350,	373,	374,	375,	424,	425,	426,	427,	428,	429,	452,	453,	454,	503,	504,	513,	516,	517,	526,	529,	530,	539,	542,	543,	552)
x_train <- select(x_train, 1,	2,	3,	4,	5,	6,	41,	42,	43,	44,	45,	46,	81,	82,	83,	84,	85,	86,	121,	122,	123,	124,	125,	126,	161,	162,	163,	164,	165,	166,	201,	202,	214,	215,	227,	228,	240,	241,	253,	254,	266,	267,	268,	269,	270,	271,	294,	295,	296,	345,	346,	347,	348,	349,	350,	373,	374,	375,	424,	425,	426,	427,	428,	429,	452,	453,	454,	503,	504,	513,	516,	517,	526,	529,	530,	539,	542,	543,	552)

#Rename 'person' and 'activity' headers
subject_test <- rename(subject_test, person = V1)
subject_train <- rename(subject_train, person = V1)
y_test <- rename(y_test, activity = V1)
y_train <- rename(y_train, activity = V1)

# Merge the test data sets together
test_combined <- cbind(subject_test,y_test,x_test)

# Merge the train data sets together
train_combined <- cbind(subject_train,y_train,x_train)

# Merges the training and the test sets to create one data set.
complete_dataframe <- rbind(test_combined, train_combined)

# labeling the data set with descriptive variable names. 
complete_dataframe <- rename(complete_dataframe, tBodyAcc_mean_X = V1,	tBodyAcc_mean_Y = V2,	tBodyAcc_mean_Z = V3,	tBodyAcc_std_X = V4,	tBodyAcc_std_Y = V5,	tBodyAcc_std_Z = V6,	tGravityAcc_mean_X = V41,	tGravityAcc_mean_Y = V42,	tGravityAcc_mean_Z = V43,	tGravityAcc_std_X = V44,	tGravityAcc_std_Y = V45,	tGravityAcc_std_Z = V46,	tBodyAccJerk_mean_X = V81,	tBodyAccJerk_mean_Y = V82,	tBodyAccJerk_mean_Z = V83,	tBodyAccJerk_std_X = V84,	tBodyAccJerk_std_Y = V85,	tBodyAccJerk_std_Z = V86,	tBodyGyro_mean_X = V121,	tBodyGyro_mean_Y = V122,	tBodyGyro_mean_Z = V123,	tBodyGyro_std_X = V124,	tBodyGyro_std_Y = V125,	tBodyGyro_std_Z = V126,	tBodyGyroJerk_mean_X = V161,	tBodyGyroJerk_mean_Y = V162,	tBodyGyroJerk_mean_Z = V163,	tBodyGyroJerk_std_X = V164,	tBodyGyroJerk_std_Y = V165,	tBodyGyroJerk_std_Z = V166,	tBodyAccMag_mean = V201,	tBodyAccMag_std = V202,	tGravityAccMag_mean = V214,	tGravityAccMag_std = V215,	tBodyAccJerkMag_mean = V227,	tBodyAccJerkMag_std = V228,	tBodyGyroMag_mean = V240,	tBodyGyroMag_std = V241,	tBodyGyroJerkMag_mean = V253,	tBodyGyroJerkMag_std = V254,	fBodyAcc_mean_X = V266,	fBodyAcc_mean_Y = V267,	fBodyAcc_mean_Z = V268,	fBodyAcc_std_X = V269,	fBodyAcc_std_Y = V270,	fBodyAcc_std_Z = V271,	fBodyAcc_meanFreq_X = V294,	fBodyAcc_meanFreq_Y = V295,	fBodyAcc_meanFreq_Z = V296,	fBodyAccJerk_mean_X = V345,	fBodyAccJerk_mean_Y = V346,	fBodyAccJerk_mean_Z = V347,	fBodyAccJerk_std_X = V348,	fBodyAccJerk_std_Y = V349,	fBodyAccJerk_std_Z = V350,	fBodyAccJerk_meanFreq_X = V373,	fBodyAccJerk_meanFreq_Y = V374,	fBodyAccJerk_meanFreq_Z = V375,	fBodyGyro_mean_X = V424,	fBodyGyro_mean_Y = V425,	fBodyGyro_mean_Z = V426,	fBodyGyro_std_X = V427,	fBodyGyro_std_Y = V428,	fBodyGyro_std_Z = V429,	fBodyGyro_meanFreq_X = V452,	fBodyGyro_meanFreq_Y = V453,	fBodyGyro_meanFreq_Z = V454,	fBodyAccMag_mean = V503,	fBodyAccMag_std = V504,	fBodyAccMag_meanFreq = V513,	fBodyBodyAccJerkMag_mean = V516,	fBodyBodyAccJerkMag_std = V517,	fBodyBodyAccJerkMag_meanFreq = V526,	fBodyBodyGyroMag_mean = V529,	fBodyBodyGyroMag_std = V530,	fBodyBodyGyroMag_meanFreq = V539,	fBodyBodyGyroJerkMag_mean = V542,	fBodyBodyGyroJerkMag_std = V543,	fBodyBodyGyroJerkMag_meanFreq = V552)

# independent tidy data set with the average of each variable for each activity and each subject.
grouping <- group_by(complete_dataframe,activity,subject)
complete_dataframe_summarized = summarize(test,mean(tBodyAcc_mean_X),	mean(tBodyAcc_mean_Y),	mean(tBodyAcc_mean_Z),	mean(tBodyAcc_std_X),	mean(tBodyAcc_std_Y),	mean(tBodyAcc_std_Z),	mean(tGravityAcc_mean_X),	mean(tGravityAcc_mean_Y),	mean(tGravityAcc_mean_Z),	mean(tGravityAcc_std_X),	mean(tGravityAcc_std_Y),	mean(tGravityAcc_std_Z),	mean(tBodyAccJerk_mean_X),	mean(tBodyAccJerk_mean_Y),	mean(tBodyAccJerk_mean_Z),	mean(tBodyAccJerk_std_X),	mean(tBodyAccJerk_std_Y),	mean(tBodyAccJerk_std_Z),	mean(tBodyGyro_mean_X),	mean(tBodyGyro_mean_Y),	mean(tBodyGyro_mean_Z),	mean(tBodyGyro_std_X),	mean(tBodyGyro_std_Y),	mean(tBodyGyro_std_Z),	mean(tBodyGyroJerk_mean_X),	mean(tBodyGyroJerk_mean_Y),	mean(tBodyGyroJerk_mean_Z),	mean(tBodyGyroJerk_std_X),	mean(tBodyGyroJerk_std_Y),	mean(tBodyGyroJerk_std_Z),	mean(tBodyAccMag_mean),	mean(tBodyAccMag_std),	mean(tGravityAccMag_mean),	mean(tGravityAccMag_std),	mean(tBodyAccJerkMag_mean),	mean(tBodyAccJerkMag_std),	mean(tBodyGyroMag_mean),	mean(tBodyGyroMag_std),	mean(tBodyGyroJerkMag_mean),	mean(tBodyGyroJerkMag_std),	mean(fBodyAcc_mean_X),	mean(fBodyAcc_mean_Y),	mean(fBodyAcc_mean_Z),	mean(fBodyAcc_std_X),	mean(fBodyAcc_std_Y),	mean(fBodyAcc_std_Z),	mean(fBodyAcc_meanFreq_X),	mean(fBodyAcc_meanFreq_Y),	mean(fBodyAcc_meanFreq_Z),	mean(fBodyAccJerk_mean_X),	mean(fBodyAccJerk_mean_Y),	mean(fBodyAccJerk_mean_Z),	mean(fBodyAccJerk_std_X),	mean(fBodyAccJerk_std_Y),	mean(fBodyAccJerk_std_Z),	mean(fBodyAccJerk_meanFreq_X),	mean(fBodyAccJerk_meanFreq_Y),	mean(fBodyAccJerk_meanFreq_Z),	mean(fBodyGyro_mean_X),	mean(fBodyGyro_mean_Y),	mean(fBodyGyro_mean_Z),	mean(fBodyGyro_std_X),	mean(fBodyGyro_std_Y),	mean(fBodyGyro_std_Z),	mean(fBodyGyro_meanFreq_X),	mean(fBodyGyro_meanFreq_Y),	mean(fBodyGyro_meanFreq_Z),	mean(fBodyAccMag_mean),	mean(fBodyAccMag_std),	mean(fBodyAccMag_meanFreq),	mean(fBodyBodyAccJerkMag_mean),	mean(fBodyBodyAccJerkMag_std),	mean(fBodyBodyAccJerkMag_meanFreq),	mean(fBodyBodyGyroMag_mean),	mean(fBodyBodyGyroMag_std),	mean(fBodyBodyGyroMag_meanFreq),	mean(fBodyBodyGyroJerkMag_mean),	mean(fBodyBodyGyroJerkMag_std),	mean(fBodyBodyGyroJerkMag_meanFreq))

# create export of the tidy data set
write.table(sum, file = "samsung_avg_var_by_activity_subject.txt", row.names = FALSE)
