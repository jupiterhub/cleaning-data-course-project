# Usage: 
# 1. Download https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 2. put run_analysis.R  into the extracted folder like so
## 	getdata-projectfiles-UCI HAR Dataset
## 		run_analysis.R
## 		UCI HAR Dataset
# 3. Set working directory so you can source R and the dataset
# 4. source("run_analysis.R")
# 5. Get cleanData.txt is generated on thesame folder as source
loadDependencies <- function() {
	# For renaming columns and preserving columns on original data
	library(plyr)	
}

# 'UCI HAR Dataset' data must be in same folder as run_analysis.R
# OUTPUT - loadWearableData()
# x <- loadWearableData()
# summary(x)
#                Length Class      Mode
# features         2    data.frame list
# activityLabels   2    data.frame list
# xTest          561    data.frame list
# yTest            1    data.frame list
# subjectTest      1    data.frame list
# xTrain         561    data.frame list
# yTrain           1    data.frame list
# subjectTrain     1    data.frame list
loadWearableData <- function() {
	data <- list()
	
	#load features
	data$features <- read.table("./UCI HAR Dataset/features.txt")
	data$activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt")

	# load test data
	data$xTest <- read.table("./UCI HAR Dataset/test/X_test.txt")
	data$yTest <- read.table("./UCI HAR Dataset/test/y_test.txt", col.names="activityId")
	data$subjectTest <- read.table("./UCI HAR Dataset/test/subject_test.txt", col.names="subject")

	# load train data
	data$xTrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
	data$yTrain <- read.table("./UCI HAR Dataset/train/y_train.txt")
	data$subjectTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")

	return(data)
}



# Returns combined list of train and test containing merged subjectTest, yTest, xTest
mergeTestAndTrainData <- function(wearableData) {
	mergedTestData <- cbind(rename(wearableData$subjectTest, c("V1"="subject")),
		rename(wearableData$yTest, c("V1"="activityId")),
		wearableData$xTest)

	mergedTrainData <- cbind(rename(wearableData$subjectTrain, c("V1"="subject")),
		rename(wearableData$yTrain, c("V1"="activityId")),
		wearableData$xTrain)

	return(rbind(mergedTestData, mergedTrainData))
}

# Only return observations with mean and std
meanAndStdColumnsOnly <- function(wearableData) {
	isMeanOrStd <- grepl("mean|std",wearableData$features$V2) 
	
	return(wearableData$mergedData[,isMeanOrStd])
}

# reformat the column to display activityname and subject on first columns
reorganizeColumns <- function(cleanData) {
	cleanData[,2] = NULL	
	cleanData[,3] = NULL	
	cleanData[,80] = NULL	

	return(cleanData)
}


####### Call functinos
loadDependencies()
wearableData <- loadWearableData()

options(warn=-1)
# 1. Merges the training and the test sets to create one data set.\
wearableData$mergedData <- mergeTestAndTrainData(wearableData)
message("1. Merged test and train data succesfully.")

# 2. Extracts only the measurements on the mean and standard deviation for each measurement
wearableData$mergedData <- meanAndStdColumnsOnly(wearableData)
message("2. Extracted mean/std measurements successfully.")

# 3. Uses descriptive activity names to name the activities in the data set
wearableData$mergedData  <- merge(wearableData$mergedData, wearableData$activityLabels, by.x="activityId", by.y="V1")
wearableData$mergedData  <- rename(wearableData$mergedData, c("V2.y"="activityName"))
message("3. Addded activity name on the data set succesfully.")

# 4. Appropriately labels the data set with descriptive variable names. 
variableNames <- lapply(wearableData$features[grepl("mean|std",wearableData$features$V2),]$V2, as.character)
colnames(wearableData$mergedData) <- c("subject","activityId", variableNames, "activityName")
message("4. Labeled variable names successfully.")

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
cleanData <- aggregate(wearableData$mergedData, by=list(activityName = wearableData$mergedData$activityName, subject = wearableData$mergedData$subject), mean)	
cleanData <- reorganizeColumns(cleanData)
write.table(cleanData, "cleanData.txt", sep="\t")
message("5. File cleanData.txt created successfully.")

options(warn=0)