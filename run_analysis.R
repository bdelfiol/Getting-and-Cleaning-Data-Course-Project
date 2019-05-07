
getwd()
setwd("C:/Users/beatr/Desktop/CourseraR/3_Cleaning Data/UCI HAR Dataset")

#1 Merges the training and the test sets to create one data set.
#2 Extracts only the measurements on the mean and standard deviation for each measurement.
#3 Uses descriptive activity names to name the activities in the data set
#4 Appropriately labels the data set with descriptive variable names.
#5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

urL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
unzip(zipfile=url, exdir="C:/Users/beatr/Desktop/CourseraR/3_Cleaning Data/UCI HAR Dataset")
list.files("C:/Users/beatr/Desktop/CourseraR/3_Cleaning Data/UCI HAR Dataset")
#[1] "activity_labels.txt" "features.txt"        "features_info.txt"   "README.txt"         
#[5] "test"                "train"

#Data from "test"

list.files("C:/Users/beatr/Desktop/CourseraR/3_Cleaning Data/UCI HAR Dataset/test")
#[1] "Inertial Signals" "subject_test.txt" "X_test.txt"       "y_test.txt"

pathX_test = file.path("C:/Users/beatr/Desktop/CourseraR/3_Cleaning Data/UCI HAR Dataset/test/X_test.txt")
X_test <- read.table(pathX_test,header = FALSE)
str(X_test) #checking
#View(X_test)

pathy_test = file.path("C:/Users/beatr/Desktop/CourseraR/3_Cleaning Data/UCI HAR Dataset/test/y_test.txt")
y_test = read.table(pathy_test,header = FALSE)
str(y_test) #checking
#View(y_test)

pathsubject_test = file.path("C:/Users/beatr/Desktop/CourseraR/3_Cleaning Data/UCI HAR Dataset/test/subject_test.txt")
subject_test = read.table(pathsubject_test,header = FALSE)
str(subject_test) #checking
#View(y_test)

#Data from "training"
list.files("C:/Users/beatr/Desktop/CourseraR/3_Cleaning Data/UCI HAR Dataset/train")
#[1] "Inertial Signals"  "subject_train.txt" "X_train.txt"       "y_train.txt"

pathX_train = file.path("C:/Users/beatr/Desktop/CourseraR/3_Cleaning Data/UCI HAR Dataset/train/X_train.txt")
X_train <- read.table(pathX_train, header = FALSE)
str(X_train) #checking
#View(X_train)

pathy_train = file.path("C:/Users/beatr/Desktop/CourseraR/3_Cleaning Data/UCI HAR Dataset/train/y_train.txt")
y_train = read.table(pathy_train,header = FALSE)
str(y_train) #checking
#View(y_train)

pathsubject_train = file.path("C:/Users/beatr/Desktop/CourseraR/3_Cleaning Data/UCI HAR Dataset/train/subject_train.txt")
subject_train = read.table(pathsubject_train,header = FALSE)
str(subject_train) #checking
#View(y_test)

#Features (names for "test" e "training") - vector with names of the variables X
pathy_features = file.path("C:/Users/beatr/Desktop/CourseraR/3_Cleaning Data/UCI HAR Dataset/features.txt")
features = read.table(pathy_features,header = FALSE)
str(features) #checking
#View(features)

#Activity labels vector
pathactivity_labels = file.path("C:/Users/beatr/Desktop/CourseraR/3_Cleaning Data/UCI HAR Dataset/activity_labels.txt")
activityLabels = read.table(pathactivity_labels, header = FALSE)
str(activityLabels) #checking
#View(activityLabels)

#giving names to the columns of dataframe "X_test" - Features
colnames(X_test) = features[,2]
str(X_test) #checking
#View(X_test)

#giving name to the column of dataframe "y_test"
colnames(y_test) = "activityId" 
str(y_test) #checking
#View(y_test)

#giving names to the column of dataframe "subject_test"
colnames(subject_test) = "subjectId" 
str(subject_test) #checking

#giving names to the columns of dataframe "X_train" - Features
colnames(X_train) = features[,2] 
str(X_train) #checking
#View(X_train)

#giving name to the column of dataframe "y_test"
colnames(y_train) = "activityId" 
str(y_train) #checking
#View(y_train)

#giving names to the column of dataframe "subject_train"
colnames(subject_train) = "subjectId" 
str(subject_train) #checking

#giving names to the columns of dataframe "activityLabels"
colnames(activityLabels) <- c("activityId", "activityType") 
str(activityLabels) #checking
#View(activityLabels)

#1 Merges the "training" and the "test" sets to create one data set.

#1.1 combine by columns files front "test" dataframe "y_test", "X_test" and "subject_test" 
mrg_test = cbind(y_test, subject_test, X_test)
head(mrg_test)
str(mrg_test) #checking
#View(mrg_test)

#1.2 combine by columns files front "training" dataframe "y_train", "X_train" and "subject_train"
mrg_train = cbind(y_train, subject_train, X_train)
head(mrg_train)
str(mrg_train) #checking
#View(mrg_train)

# 1.3 #combine by rows files "test" and "train" - file setAllInOne
setAllInOne = rbind(mrg_train, mrg_test) 
write.table(setAllInOne, "setAllInOne.txt", row.names = FALSE)

#2 Extracts only the measurements on the mean and standard deviation for each measurement.Also keepping "activityId", "subejectId"
colNames = colnames(setAllInOne) 
mean_std = (grepl("activityId", colNames) | grepl("subjectId", colNames) | grepl("mean..", colNames) | grepl("std..", colNames))
str(mean_std) #checking
head(mean_std)

setForMean_Std<- setAllInOne[, mean_std == TRUE]
str(setForMean_Std) #checking
head(setForMean_Std)

#3 Uses descriptive activity names to name the activities in the data set

setWithActivityName = merge(setForMean_Std, activityLabels, by="activityId", all.x=TRUE) #criou uma nova variável "Activitype"
names(setWithActivityName)
View(setWithActivityName)

#4 Appropriately labels the data set with descriptive variable names.
# it was done previusly
dataWithActivityName <- setWithActivityName
str(dataWithActivityName) #checking

#5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
finalTidy <- aggregate(.~subjectId + activityId, dataWithActivityName, mean)
finalTidy <- finalTidy[order(finalTidy$subjectId, finalTidy$activityId),]

write.table(finalTidy, "finalTidy.txt", row.names = FALSE)
str(finalTidy) #checking
View(finalTidy)
