## 1. Merges the training and the test sets to create one data set.  

directory <- "./UCI HAR Dataset"
activity_labels <- read.table(paste(directory, "/activity_labels.txt", sep=""))

# train: labels and data

subject_train <- read.table(paste(directory, "/train/subject_train.txt", sep=""))
y_train <- read.table(paste(directory, "/train/y_train.txt", sep=""))
train_data <- read.table(paste(directory, "/train/X_train.txt", sep=""))

# test: labels and data

subject_test <- read.table(paste(directory, "/test/subject_test.txt", sep=""))
y_test <- read.table(paste(directory, "/test/y_test.txt", sep=""))
test_data <- read.table(paste(directory, "/test/X_test.txt", sep=""))

## 3. Uses descriptive activity names to name the activities in the data set.  

# merge activity labels: y train and y test

y_train_labels <- merge(y_train, activity_labels, by="V1")
y_test_labels <- merge(y_test, activity_labels, by="V1")

# merge the training and testing data and the respective labels

train_set <- cbind(subject_train, y_train_labels, train_data)
test_set <- cbind(subject_test, y_test_labels, test_data)

# finally, concatenate train and test sets, to complete task 1

dataset <- rbind(train_set, test_set)

## 4. Appropriately labels the data set with descriptive variable names.  

features_data <- read.table(paste(directory, "/features.txt", sep=""))
colnames(dataset) <- c("Subject", "ActivityId", "Activity", as.vector(features_data[,2]))

## 2. Extracts only the measurements on the mean and standard deviation 
##    for each measurement.  

# subset only the rows where the name contains the words mean or std
target_columns <- subset(features_data,  grepl("(mean\\(\\)|std\\(\\))", features_data$V2))

# extract the first 3 columns plus means and stds

extracted_dataset <- dataset[,c(1, 2, 3, as.vector(target_columns$V1)+3)]

## format to proper column names

colnames_ds <- colnames(extracted_dataset)
colnames_ds <- gsub("-mean()", "Mean", colnames_ds, fixed=TRUE)
colnames_ds <- gsub("-std()", "Std", colnames_ds, fixed=TRUE)
colnames_ds <- gsub("BodyBody", "Body", colnames_ds, fixed=TRUE) # BodyBody ???
colnames(extracted_dataset) <- colnames_ds


## 5. Creates a second, independent tidy data set 
##    with the average of each variable for each activity 
##    and each subject.  

## melt the data

library(reshape2)
melted_data <- melt(extracted_dataset, id=c("Subject", "ActivityId", "Activity"))

# cast the dataset back to the tidy_data format

tidy_dataset <- dcast(melted_data, formula = Subject + ActivityId + Activity ~ variable, mean)

## write the output into a file

write.table(tidy_dataset, "./result.txt", sep="\t", row.names=FALSE)

