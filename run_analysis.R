## project 1 Lesson4- Getting and Cleaning data

## data description:
## http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
## project data:
## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

## This script creates one R script called run_analysis.R 
## that does the following:

## 1 Merge the training and the test sets to create one data set.
## 2 Extracts only the measurements on the mean and standard deviation 
##   for each measurement.
## 3 Uses descriptive activity names to name the activities in the data set
## 4 Appropriately label the data set with descriptive variable names.
## 5 From the data set in step 4, creates a second, independent tidy data set 
##   with the average of each variable for each activity and each subject.
##   #####   #####   #####   #####   #####   #####   #####   #####   #####

# these are likely already in your environment
library(dplyr)
library(plyr)

# execute from working directory containing the UCI HAR dataset
print("Load Test data into R: x, y, and subject files")
x_test<- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test<- read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test<- read.table("./UCI HAR Dataset/test/subject_test.txt")

print("Load Train data into R: x, y and subject files")
x_train<- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train<- read.table("./UCI HAR Dataset/train/y_train.txt")
subject_train<- read.table("./UCI HAR Dataset/train/subject_train.txt")

print("## 1 Combine like files (x and x, y ans y, etc) in Test and Train repos")
x_data <- rbind(x_train, x_test)
y_data <- rbind(y_train, y_test)
subject_data<- rbind(subject_train, subject_test)

#get features and activity labels
features_data <- read.table("./UCI HAR Dataset/features.txt")
activity_labels<- read.table("./UCI HAR Dataset/activity_labels.txt", col.names = c("Code", "Activity"))

print("## 2 CLEAN mean and std values from merged data")
get_mean_std<- features_data[grep("mean\\(\\)|std\\(\\)", features_data[,2]),]
x_data <- x_data[,get_mean_std[,1]]

# CLEAN -add/remove col, construct table that contains mean and std
print("## 3 Use descriptive names for the activities in the data set")
y_data$Code <- y_data[,1] #add column 'Code'
y_data <-subset(y_data, select = -V1) #remove column V1

##CLEAN -add activity_labels to y_data$Code column
y_data<- join(y_data, activity_labels) 

print("## 4 : Use descriptive labels on x_data columns")
##CLEAN -col names in x_data, subject_data 
colnames(x_data)<- features_data[get_mean_std[,1],2]
colnames(subject_data)<- "Subject"
activity <- y_data[,-1]      #create labels from y_data

print("## 5: Create table with data in #4 so that mean values have subject and label") 
total<- cbind(x_data, subject_data, activity)
mean_data <- total%>%
        group_by(Subject, activity)%>%
        summarise_all(list(mean)) # grouping to get mean values

write.table(mean_data,"Get_clean.txt", row.names = FALSE)
print("Output file, 'Get_clean.txt': 219K, 181 observations")
