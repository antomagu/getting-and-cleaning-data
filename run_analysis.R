#Getting and Cleaning Data Project John Hopkins Coursera
#Author: Antonio

# Download the dataset

getdata_projectfiles_UCI_HAR_Dataset <- read_csv("C:/Users/anton/Downloads/getdata_projectfiles_UCI HAR Dataset.zip")

## Reading files of Test

#Load the Data

X_test <- read_csv("C:/Users/anton/Downloads/test/X_test.txt")
y_test <- read_csv("C:/Users/anton/Downloads/test/y_test.txt")
subject_test <- read.table("C:/Users/anton/Downloads/test/subject_test.txt", quote="\"", comment.char="")

## Reading files of Train

X_train <- read_csv("C:/Users/anton/Downloads/train/X_train.txt")
y_train <- read_csv("C:/Users/anton/Downloads/train/y_train.txt")
subject_train <- read.table("C:/Users/anton/Downloads/train/subject_train.txt", quote="\"", comment.char="")

## Reading feature

features <- read.table("C:/Users/anton/Downloads/features.txt", quote="\"", comment.char="")

## Reading activity_labels

activity_labels <- read.table("C:/Users/anton/Downloads/activity_labels.txt", quote="\"", comment.char="")

## Assigning variable names
colnames(X_train) <- features[,2]
colnames(y_train) <- "activityID"
colnames(subject_train) <- "subjectID"

colnames(X_test) <- features[,2]
colnames(y_test) <- "activityID"
colnames(subject_test) <- "subjectID"

colnames(activity_labels) <- c('activityId','activityType')

# 1. Merge the training and test datasets
# 1.1 Merge datasets
train <- cbind(y_train, subject_train, X_train)
test <- cbind(y_test,subject_test, X_test)
df<- rbind(train, test)

# 2 Extracts only the measurements on the mean and standard deviation for each measurement.
colNames <- colnames(df)

#Create vector for defining ID, mean, and sd
mean_and_std <- (grepl("activityID", colNames) |
                   grepl("subjectID", colNames) |
                   
                   #SHOULD NOT BE ACCEPTED AS A NEW SUBMISSION
                 grepl("mean..", colNames) |
                   grepl("std...", colNames)
)

setForMeanAndStd <- df[ , mean_and_std == TRUE]

# 3 Uses descriptive activity names to name the activities in the data set
setWithActivityNames <- merge(setForMeanAndStd, df, activity_labels 
                              by='activityId',
                              all.x=TRUE)
# 4 Appropriately labels the data set with descriptive variable names
# see 1.3, 2.2, 2.3

# 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


        tidySet <- aggregate(. ~subjectID + activityID, setWithActivityNames, mean)
        tidySet <- tidySet[order(tidySet$subjectID, tidySet$activityID), ]
        
        # 5.2 Writing second tidy data set into a txt file
        write.table(tidySet, "tidySet.txt", row.names = FALSE)
        
write.table(setWithActivityNames, "tidySet.txt", row.names = FALSE)
