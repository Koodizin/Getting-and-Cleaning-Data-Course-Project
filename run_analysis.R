#Install Packages
install.packages("data.table")
install.packages("dplyr")

#Running the libraries
library(data.table)
library(dplyr)


#Reading the data

feature_Names <- read.table("UCI HAR Dataset/features.txt")
activity_Labels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

subject_Train <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activity_Train <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
features_Train <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

subject_Test <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activity_Test <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
features_Test <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)


#Part 1 - Merges the training and the test sets to create one data set.

subject <- rbind(subject_Train, subject_Test)
activity <- rbind(activity_Train, activity_Test)
features <- rbind(features_Train, features_Test)


colnames(features) <- t(feature_Names[2])
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
all_data <- cbind(features,activity,subject)

#Part 2 - Extracts only the measurements on the mean and standard deviation for each measurement. 

columnsMean_STD <- grep(".*Mean.*|.*Std.*", names(all_data), ignore.case=TRUE)
required_Columns <- c(columnsMean_STD, 562, 563)
dim(all_data)

extract_data <- all_data[,required_Columns]
dim(extract_data)


#Part 3 - Uses descriptive activity names to name the activities in the data set

extract_data$Activity <- as.character(extract_data$Activity)
for (i in 1:6){
  extract_data$Activity[extract_data$Activity == i] <- as.character(activityLabels[i,2])
}

extract_data$Activity <- as.factor(extract_data$Activity)

#Part 4 - Appropriately labels the data set with descriptive variable names. 

names(extract_data)

names(extract_data)<-gsub("Acc", "Accelerometer", names(extract_data))
names(extract_data)<-gsub("Gyro", "Gyroscope", names(extract_data))
names(extract_data)<-gsub("BodyBody", "Body", names(extract_data))
names(extract_data)<-gsub("Mag", "Magnitude", names(extract_data))
names(extract_data)<-gsub("^t", "Time", names(extract_data))
names(extract_data)<-gsub("^f", "Frequency", names(extract_data))
names(extract_data)<-gsub("tBody", "TimeBody", names(extract_data))
names(extract_data)<-gsub("-mean()", "Mean", names(extract_data), ignore.case = TRUE)
names(extract_data)<-gsub("-std()", "STD", names(extract_data), ignore.case = TRUE)
names(extract_data)<-gsub("-freq()", "Frequency", names(extract_data), ignore.case = TRUE)
names(extract_data)<-gsub("angle", "Angle", names(extract_data))
names(extract_data)<-gsub("gravity", "Gravity", names(extract_data))

names(extract_data)


#Part 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

extract_data$Subject <- as.factor(extract_data$Subject)
extract_data <- data.table(extract_data)

tidyData <- aggregate(. ~Subject + Activity, extract_data, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)
