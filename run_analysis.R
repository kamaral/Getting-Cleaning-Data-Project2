run_analysis <- function()
 
  
  library(data.table)
library(dplyr)


  #Load Files  
  features <- read.table("features.txt", header = FALSE)
  activity_labels <- read.table("activity_labels.txt", header = FALSE)

#assign column names to activity_labels
colnames(activity_labels)  = c('activityId','activityType');

#Load test files
  subject_test <- read.table("subject_test.txt", header = FALSE)
  y_test <- read.table("y_test.txt", header = FALSE)
x_test <- read.table("X_test.txt", header = FALSE)

#Load training files
  subject_train <- read.table("subject_train.txt", header = FALSE)
  y_train <- read.table("y_train.txt",  header = FALSE)
x_train <- read.table("X_train.txt", header = FALSE)
  
  
  #Create test and train data
  subject_y_x_test <- cbind(subject_test, y_test, x_test)
  subject_y_x_train <- cbind(subject_train, y_train, x_train)

#Merges the column names, tests, and training sets to create one data set training and the test sets to create one data set.
columnNames <- c("Subject", "activityId", t(features[2]) )

dataset <- rbind(columnNames, subject_y_x_test, subject_y_x_train)


#Extracts only the measurements on the mean and standard deviation for each measurement. 
logicalVector = (grepl("Subject",colnames) | grepl("activityId",colnames) | grepl("-mean..",colnames) & !grepl("-meanFreq..",colnames) & !grepl("mean..-",colnames) | grepl("-std..",colnames) & !grepl("-std()..-",colnames))
dataset = dataset[logicalVector==TRUE]

#Uses descriptive activity names to name the activities in the data set
dataset = merge(dataset,activity_labels,by='activityId',all.x=TRUE);


#Appropriately labels the data set with descriptive variable names. 
names(dataset) <- gsub("Acc", "Accelerator", names(dataset))
names(dataset) <- gsub("Mag", "Magnitude", names(dataset))
names(dataset) <- gsub("Gyro", "Gyroscope", names(dataset))
names(dataset) <- gsub("^t", "time", names(dataset))
names(dataset) <- gsub("^f", "frequency", names(dataset))
dataset$Subject[dataset$Subject == 1] <- "Participant 1"
dataset$Subject[dataset$Subject == 2] <- "Participant 2"
dataset$Subject[dataset$Subject == 3] <- "Participant 3"
dataset$Subject[dataset$Subject == 4] <- "Participant 4"
dataset$Subject[dataset$Subject == 5] <- "Participant 5"
dataset$Subject[dataset$Subject == 6] <- "Participant 6"
dataset$Subject[dataset$Subject == 7] <- "Participant 7"
dataset$Subject[dataset$Subject == 8] <- "Participant 8"
dataset$Subject[dataset$Subject == 9] <- "Participant 9"
dataset$Subject[dataset$Subject == 10] <- "Participant 10"
dataset$Subject[dataset$Subject == 11] <- "Participant 11"
dataset$Subject[dataset$Subject == 12] <- "Participant 12"
dataset$Subject[dataset$Subject == 13] <- "Participant 13"
dataset$Subject[dataset$Subject == 14] <- "Participant 14"
dataset$Subject[dataset$Subject == 15] <- "Participant 15"
dataset$Subject[dataset$Subject == 16] <- "Participant 16"
dataset$Subject[dataset$Subject == 17] <- "Participant 17"
dataset$Subject[dataset$Subject == 18] <- "Participant 18"
dataset$Subject[dataset$Subject == 19] <- "Participant 19"
dataset$Subject[dataset$Subject == 20] <- "Participant 20"
dataset$Subject[dataset$Subject == 21] <- "Participant 21"
dataset$Subject[dataset$Subject == 22] <- "Participant 22"
dataset$Subject[dataset$Subject == 23] <- "Participant 23"
dataset$Subject[dataset$Subject == 24] <- "Participant 24"
dataset$Subject[dataset$Subject == 25] <- "Participant 25"
dataset$Subject[dataset$Subject == 26] <- "Participant 26"
dataset$Subject[dataset$Subject == 27] <- "Participant 27"
dataset$Subject[dataset$Subject == 28] <- "Participant 28"
dataset$Subject[dataset$Subject == 29] <- "Participant 29"
dataset$Subject[dataset$Subject == 30] <- "Participant 30"


#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# q5<- dataset[,]
tidydata <- dataset[, lapply(.SD, mean), by = 'Subject, activityId']
write.table(tidydata, file = "tidydata.txt", row.names = FALSE)