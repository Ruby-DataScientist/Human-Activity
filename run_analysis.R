install.packages("dplyr")
library(dplyr)

filename<-"getdata_projectfiles_UCI HAR Dataset"

## If the file already exists
if(!file.exists(filename)){
    fileURL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileURL, filename, method="curl")
}

##Unzip file
if(!file.exists(filename)){
    unzip(filename)
}
## Read Features and activities 

features <-read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))


## Read Training Data

subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject_no")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

## Read Testing Data

subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject_no")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")



##1. Merges the training and the test sets to create one data set.
subject<-rbind(subject_train, subject_test)
X<-rbind(x_train, x_test)
Y<-rbind(y_train, y_test)
merged_data<-cbind(subject, X,Y)


## 2.Extracts only the measurements on the mean and standard deviation for each measurement.

final_clean_data <- merged_data %>% select(subject_no, code, contains("mean"), contains("std"))


##3. Uses descriptive activity names to name the activities in the data set
## This gives names of activities : walking_downstairs , laying etc...

final_clean_data$code <- activity_labels[final_clean_data$code, 2]

##4. Appropriately labels the data set with descriptive variable names.
##Purpose of the following code is to make column names more descriptive and understandable

names(final_clean_data)[2] = "activity"
names(final_clean_data)<-gsub("-mean()", "Mean", names(final_clean_data), ignore.case = TRUE)
names(final_clean_data)<-gsub("-std()", "STD", names(final_clean_data), ignore.case = TRUE)
names(final_clean_data)<-gsub("Acc", "Accelerometer", names(final_clean_data))
names(final_clean_data)<-gsub("Gyro", "Gyroscope", names(final_clean_data))
names(final_clean_data)<-gsub("BodyBody", "Body", names(final_clean_data))
names(final_clean_data)<-gsub("Mag", "Magnitude", names(final_clean_data))
names(final_clean_data)<-gsub("^t", "Time", names(final_clean_data))
names(final_clean_data)<-gsub("^f", "Frequency", names(final_clean_data), ignore.case = TRUE)
names(final_clean_data)<-gsub("tBody", "BodyTime", names(final_clean_data))
names(final_clean_data)<-gsub("-freq()", "Frequency", names(final_clean_data))
names(final_clean_data)<-gsub("angle", "Angle", names(final_clean_data))
names(final_clean_data)<-gsub("gravity", "Gravity", names(final_clean_data))


Final_Independent_Data <- final_clean_data %>%
    group_by(activity, subject_no) %>%
    summarise_all(funs(mean))

##Checking final data
str(Final_Independent_Data)
head(Final_Independent_Data)
tail(Final_Independent_Data)

write.table(Final_Independent_Data, "TidyDataSet.txt", row.name=FALSE)

