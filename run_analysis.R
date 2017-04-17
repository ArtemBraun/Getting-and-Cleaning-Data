# launching "dplyr" package

library(dplyr)

# downloading data file
Url_data <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
File_data <- "UCI HAR Dataset.zip"
if (!file.exists(File_data)) {
  download.file(Url_data, File_data, mode = "wb")
}

# extracting zip file
data_name <- "UCI HAR Dataset"
if (!file.exists(data_name)) {
  unzip(File_data)
}

# reading file with activity names
activities <- read.table(file.path(data_name, "activity_labels.txt"))
colnames(activities) <- c("activity_Number", "activity_Name")

# reading file with features
features <- read.table(file.path(data_name, "features.txt"), as.is = TRUE)

# reading files with training data
Activities_train <- read.table(file.path(data_name, "train", "y_train.txt"))
Values_train <- read.table(file.path(data_name, "train", "X_train.txt"))
Subjects_train <- read.table(file.path(data_name, "train", "subject_train.txt"))

# reading files with test data
Activities_test <- read.table(file.path(data_name, "test", "y_test.txt"))
Values_test <- read.table(file.path(data_name, "test", "X_test.txt"))
Subjects_test <- read.table(file.path(data_name, "test", "subject_test.txt"))



# Taks 1 - Merge the training and the test sets to create one data set

# merging into a single data table
Merged_table <- rbind(cbind(Subjects_train, Values_train, Activities_train),
  		       cbind(Subjects_test, Values_test, Activities_test)
)

# naming columns
colnames(Merged_table) <- c("subject", features[, 2], "activity")



# Task 2 - Extract only the measurements on the mean and standard deviation for each measurement


Columns_with_mean_std <- grepl("subject|activity|mean|std", colnames(Merged_table))
Merged_table <- Merged_table[, Columns_with_mean_std]



# Task 3 - Use descriptive activity names to name the activities in the data set

Merged_table$activity <- factor(Merged_table$activity, levels = activities[, 1], labels = activities[, 2])


# Task 4 - Appropriately label the data set with descriptive variable names


# getting column names
Merged_tableCols <- colnames(Merged_table)

# tidying column names
Merged_tableCols <- gsub("[\\(\\)-]", "", Merged_tableCols)
Merged_tableCols <- gsub("^f", "frequencyDomain", Merged_tableCols)
Merged_tableCols <- gsub("^t", "timeDomain", Merged_tableCols)
Merged_tableCols <- gsub("Acc", "Accelerometer", Merged_tableCols)
Merged_tableCols <- gsub("Gyro", "Gyroscope", Merged_tableCols)
Merged_tableCols <- gsub("Mag", "Magnitude", Merged_tableCols)
Merged_tableCols <- gsub("Freq", "Frequency", Merged_tableCols)
Merged_tableCols <- gsub("mean", "Mean", Merged_tableCols)
Merged_tableCols <- gsub("std", "StandardDeviation", Merged_tableCols)
Merged_tableCols <- gsub("BodyBody", "Body", Merged_tableCols)

# applying new labels to columns
colnames(Merged_table) <- Merged_tableCols



# Task 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject


Merged_tableMeans <- Merged_table %>% 
				  group_by(subject, activity) %>%
							      summarise_each(funs(mean))

write.table(Merged_tableMeans, "tidy_data.txt", row.names = FALSE, quote = FALSE)