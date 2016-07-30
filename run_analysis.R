## The R script, run_analysis.R that the following:
## 1. Merges the training and the test sets to create one data set;
## 2. Extracts only the measurements on the mean and standard deviation 
        ## for each measurement;
## 3. Uses descriptive activity names to name the activities in the data set;
## 4. Appropriately labels the data set with descriptive activity names;
## 5. Creates a second, independent tidy data set with the average of 
        ## each variable for each activity and each subject.


## Load the respective Librarys
if (!require("data.table")) {
  install.packages("data.table")
}

if (!require("dplyr")) {
  install.packages("dplyr")
}

library("data.table")
library("dplyr")


## Task 1: Merges the training and the test sets to create one data set.

## Load the associated Meta Data

FeatureNames <- read.table(file = "./UCI HAR Dataset/features.txt")
ActivityLabels <- read.table(file = "./UCI HAR Dataset/activity_labels.txt",
                header = FALSE)

## Load the Training Data Set
train_subject <- read.table(file = "./UCI HAR Dataset/train/subject_train.txt",
                            header = FALSE)
train_activity <- read.table(file = "./UCI HAR Dataset/train/y_train.txt",
                             header = FALSE)
train_features <- read.table(file = "./UCI HAR Dataset/train/X_train.txt",
                             header = FALSE)

## Load the Testing Data Set
test_subject <- read.table(file = "./UCI HAR Dataset/test/subject_test.txt",
                           header = FALSE)
test_activity <- read.table(file = "./UCI HAR Dataset/test/y_test.txt",
                            header = FALSE)
test_features <- read.table(file = "./UCI HAR Dataset/test/X_test.txt",
                            header = FALSE)


## Merge the Training and Testing Data Set 
Subject <- rbind(train_subject,test_subject)
Activity <- rbind(train_activity,test_activity)
Features <- rbind(train_features,test_features)

## Name the Columns 
colnames(Features) <- t(FeatureNames[2])
colnames(Activity) <- "Activity"
colnames(Subject) <- "Subject"

## Create a complete Data Set
CompleteData <- cbind(Features,Activity,Subject)



## Task 2: Extracts only the measurements on the mean and standard deviation 

colMeanStd <- grep(".*Mean.*|.*Std.*",x = names(CompleteData), ignore.case=TRUE)
filteredCols <- c(colMeanStd,562:563)
extractedData <- CompleteData[,filteredCols]



## Task 3: Uses descriptive activity names to name the activities in the dataset
extractedData$Activity <- as.character(extractedData$Activity)
for(i in 1:6) {
        extractedData$Activity[extractedData$Activity == i] <- 
                as.character(ActivityLabels[,2])
}
extractedData$Activity <- as.factor(extractedData$Activity)


## Task 4: labels the data set with descriptive activity names;
names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), 
                           ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), 
                           ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), 
                           ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))



## Task 5: Creates a tidy data set with the average of each variable 
extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

tidydata <- aggregate(. ~Subject+Activity,extractedData,mean) 
tidydata <- tidydata[order(tidydata$Subject,tidydata$Activity),]
write.table(x = tidydata,file = "./tidydata.txt",row.names = FALSE)