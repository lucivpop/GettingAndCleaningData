# Code Book

## Data source

The dataset is derived from the "Human Activity Recognition Using Smartphones Data Set" at: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

## Subject and Activity Fields 

* `subject` - The participant ("subject") ID
* `activity` - The label of the activity performed when the corresponding measurements were taken

## Extracted Feature Fields (only those that contained mean and sd)

* `tBodyAcc-mean()-X` 
* `tBodyAcc-mean()-Y` 
* `tBodyAcc-mean()-Z` 
* `tBodyAcc-std()-X` 
* `tBodyAcc-std()-Y` 
* `tBodyAcc-std()-Z` 
* `tGravityAcc-mean()-X` 
* `tGravityAcc-mean()-Y` 
* `tGravityAcc-mean()-Z` 
* `tGravityAcc-std()-X` 
* `tGravityAcc-std()-Y` 
* `tGravityAcc-std()-Z` 
* `tBodyAccJerk-mean()-X` 
* `tBodyAccJerk-mean()-Y` 
* `tBodyAccJerk-mean()-Z` 
* `tBodyAccJerk-std()-X` 
* `tBodyAccJerk-std()-Y` 
* `tBodyAccJerk-std()-Z` 
* `tBodyGyro-mean()-X` 
* `tBodyGyro-mean()-Y` 
* `tBodyGyro-mean()-Z` 
* `tBodyGyro-std()-X` 
* `tBodyGyro-std()-Y` 
* `tBodyGyro-std()-Z` 
* `tBodyGyroJerk-mean()-X` 
* `tBodyGyroJerk-mean()-Y` 
* `tBodyGyroJerk-mean()-Z` 
* `tBodyGyroJerk-std()-X` 
* `tBodyGyroJerk-std()-Y` 
* `tBodyGyroJerk-std()-Z` 
* `tBodyAccMag-mean()` 
* `tBodyAccMag-std()` 
* `tGravityAccMag-mean()` 
* `tGravityAccMag-std()` 
* `tBodyAccJerkMag-mean()` 
* `tBodyAccJerkMag-std()` 
* `tBodyGyroMag-mean()` 
* `tBodyGyroMag-std()` 
* `tBodyGyroJerkMag-mean()` 
* `tBodyGyroJerkMag-std()` 
* `fBodyAcc-mean()-X` 
* `fBodyAcc-mean()-Y` 
* `fBodyAcc-mean()-Z` 
* `fBodyAcc-std()-X` 
* `fBodyAcc-std()-Y` 
* `fBodyAcc-std()-Z` 
* `fBodyAccJerk-mean()-X` 
* `fBodyAccJerk-mean()-Y` 
* `fBodyAccJerk-mean()-Z` 
* `fBodyAccJerk-std()-X` 
* `fBodyAccJerk-std()-Y` 
* `fBodyAccJerk-std()-Z` 
* `fBodyGyro-mean()-X` 
* `fBodyGyro-mean()-Y` 
* `fBodyGyro-mean()-Z` 
* `fBodyGyro-std()-X` 
* `fBodyGyro-std()-Y` 
* `fBodyGyro-std()-Z` 
* `fBodyAccMag-mean()` 
* `fBodyAccMag-std()` 
* `fBodyBodyAccJerkMag-mean()` 
* `fBodyBodyAccJerkMag-std()` 
* `fBodyBodyGyroMag-mean()` 
* `fBodyBodyGyroMag-std()` 
* `fBodyBodyGyroJerkMag-mean()` 
* `fBodyBodyGyroJerkMag-std()`

## Activity Labels - were transformed tolower case and "_" removed

* `walking` (value `1`)
* `walkingupstairs` (value `2`)
* `walkingdownstairs` (value `3`)
* `sitting` (value `4`)
* `standing` (value `5`)
* `laying` (value `6`)

## Transformations performed

Extracted only the measurements on the mean and standard deviation for each measurement.

Changed numeric representation of activity with labels.

descriptive variable names were used to appropriately label the variables in the data set. 

Created a second, independent tidy data set (tidy.txt) with the average of each variable for each activity and each subject. 
