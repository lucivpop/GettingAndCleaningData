library(data.table)

# Download and unzip the Dataset.zip from Url
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, destfile = "Dataset.zip")
unzip("Dataset.zip", exdir = ".")

# Requirement 1: Merge test and training sets into one data set 
testDS <- read.table("./UCI HAR Dataset/test/X_test.txt",header=FALSE)
testDS.a <- read.table("./UCI HAR Dataset/test/y_test.txt",header=FALSE)
testDS.s <- read.table("./UCI HAR Dataset/test/subject_test.txt",header=FALSE)
trainDS <- read.table("./UCI HAR Dataset/train/X_train.txt",header=FALSE)
trainDS.a <- read.table("./UCI HAR Dataset/train/y_train.txt",header=FALSE)
trainDS.s <- read.table("./UCI HAR Dataset/train/subject_train.txt",header=FALSE)
# features set
X<-rbind(testDS,trainDS)
# activity set
Y<-rbind(testDS.a,trainDS.a)
# subject set
S<-rbind(testDS.s,trainDS.s)
# test 
#head(X)
#head(Y)
#head(S)

# Requierement 2: Extract only the measurements on the mean and standard deviation for each measurement
features <- read.table("./UCI HAR Dataset/features.txt")
indices_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, indices_of_good_features]
names(X) <- features[indices_of_good_features, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
# test
#names(X)  

# Requirement 3: Uses descriptive activity names to name the activities in the data set
activities <- read.table("./UCI HAR Dataset/activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity"
# test 
#Y
#str(Y)

# Requirement 4: Appropriately labels the data set with descriptive activity names
names(S) <- "subject"
cleaned <- cbind(S, Y, X)
write.table(cleaned, "cleanedDS.txt")
# test
#S
#str(S)

# Requirement 5: Creates a second, independent tidy data set with the average of each variable 
# for each activity and each subject.
uniqueSubjects = unique(S)[,1]
numSubjects = length(unique(S)[,1])
numActivities = length(activities[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
    for (a in 1:numActivities) {
        result[row, 1] = uniqueSubjects[s]
        result[row, 2] = activities[a, 2]
        tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
        result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
        row = row+1
    }
}
write.table(result, "tidy.txt")
# test
#str(result)
