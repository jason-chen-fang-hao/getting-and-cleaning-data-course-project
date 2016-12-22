#Get and merge the data
 #loading raw data sets
setwd("C:/Users/shujuan/Desktop/coursera/getting and cleaning data/week 3/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset")
library(plyr)
library(data.table)
subjectTrain = read.table('./train/subject_train.txt',header=FALSE)
xTrain = read.table('./train/x_train.txt',header=FALSE)
yTrain = read.table('./train/y_train.txt',header=FALSE)

subjectTest = read.table('./test/subject_test.txt',header=FALSE)
xTest = read.table('./test/x_test.txt',header=FALSE)
yTest = read.table('./test/y_test.txt',header=FALSE)

#Organizing and combining raw data sets into single one.
xDataSet <- rbind(xTrain, xTest)
yDataSet <- rbind(yTrain, yTest)
subjectDataSet <- rbind(subjectTrain, subjectTest)
dim(xDataSet)
## [1] 10299   561
dim(yDataSet)
## [1] 10299     1
dim(subjectDataSet)
## [1] 10299     1


#Extract only the measurements on the mean and standard deviation for each measurement.
xDataSet_mean_std <- xDataSet[, grep("-(mean|std)\\(\\)", read.table("features.txt")[, 2])]
names(xDataSet_mean_std) <- read.table("features.txt")[grep("-(mean|std)\\(\\)", read.table("features.txt")[, 2]), 2] 
View(xDataSet_mean_std)
dim(xDataSet_mean_std)

## [1] 10299    66


#Use descriptive activity names to name the activities in the data set.
yDataSet[, 1] <- read.table("activity_labels.txt")[yDataSet[, 1], 2]
names(yDataSet) <- "Activity"
View(yDataSet)

#Appropriately label the data set with descriptive activity names.

names(subjectDataSet) <- "Subject"
summary(subjectDataSet)

##     Subject     
##  Min.   : 1.00  
##  1st Qu.: 9.00  
##  Median :17.00  
##  Mean   :16.15  
##  3rd Qu.:24.00  
##  Max.   :30.00

# Organizing and combining all data sets into single one.

singleDataSet <- cbind(xDataSet_mean_std, yDataSet, subjectDataSet)

# Defining descriptive names for all variables.

names(singleDataSet) <- make.names(names(singleDataSet))
names(singleDataSet) <- gsub('Acc',"Acceleration",names(singleDataSet))
names(singleDataSet) <- gsub('GyroJerk',"AngularAcceleration",names(singleDataSet))
names(singleDataSet) <- gsub('Gyro',"AngularSpeed",names(singleDataSet))
names(singleDataSet) <- gsub('Mag',"Magnitude",names(singleDataSet))
names(singleDataSet) <- gsub('^t',"TimeDomain.",names(singleDataSet))
names(singleDataSet) <- gsub('^f',"FrequencyDomain.",names(singleDataSet))
names(singleDataSet) <- gsub('\\.mean',".Mean",names(singleDataSet))
names(singleDataSet) <- gsub('\\.std',".StandardDeviation",names(singleDataSet))
names(singleDataSet) <- gsub('Freq\\.',"Frequency.",names(singleDataSet))
names(singleDataSet) <- gsub('Freq$',"Frequency",names(singleDataSet))

View(singleDataSet)

#Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
names(singleDataSet)

##  [1] "TimeDomain.BodyAcceleration.Mean...X"                                    
##  [2] "TimeDomain.BodyAcceleration.Mean...Y"                                    
##  [3] "TimeDomain.BodyAcceleration.Mean...Z"                                    
##  [4] "TimeDomain.BodyAcceleration.StandardDeviation...X"                       
##  [5] "TimeDomain.BodyAcceleration.StandardDeviation...Y"                       
##  [6] "TimeDomain.BodyAcceleration.StandardDeviation...Z"                       
##  [7] "TimeDomain.GravityAcceleration.Mean...X"                                 
##  [8] "TimeDomain.GravityAcceleration.Mean...Y"                                 
##  [9] "TimeDomain.GravityAcceleration.Mean...Z"                                 
## [10] "TimeDomain.GravityAcceleration.StandardDeviation...X"                    
## [11] "TimeDomain.GravityAcceleration.StandardDeviation...Y"                    
## [12] "TimeDomain.GravityAcceleration.StandardDeviation...Z"                    
## [13] "TimeDomain.BodyAccelerationJerk.Mean...X"                                
## [14] "TimeDomain.BodyAccelerationJerk.Mean...Y"                                
## [15] "TimeDomain.BodyAccelerationJerk.Mean...Z"                                
## [16] "TimeDomain.BodyAccelerationJerk.StandardDeviation...X"                   
## [17] "TimeDomain.BodyAccelerationJerk.StandardDeviation...Y"                   
## [18] "TimeDomain.BodyAccelerationJerk.StandardDeviation...Z"                   
## [19] "TimeDomain.BodyAngularSpeed.Mean...X"                                    
## [20] "TimeDomain.BodyAngularSpeed.Mean...Y"                                    
## [21] "TimeDomain.BodyAngularSpeed.Mean...Z"                                    
## [22] "TimeDomain.BodyAngularSpeed.StandardDeviation...X"                       
library(reshape2)

filename <- "getdata_dataset.zip"

## Download and unzip the dataset:
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
  download.file(fileURL, filename, method="curl")
}  
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

# Load activity labels + features
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt")
activityLabels[,2] <- as.character(activityLabels[,2])
features <- read.table("UCI HAR Dataset/features.txt")
features[,2] <- as.character(features[,2])

# Extract only the data on mean and standard deviation
featuresWanted <- grep(".*mean.*|.*std.*", features[,2])
featuresWanted.names <- features[featuresWanted,2]
featuresWanted.names = gsub('-mean', 'Mean', featuresWanted.names)
featuresWanted.names = gsub('-std', 'Std', featuresWanted.names)
featuresWanted.names <- gsub('[-()]', '', featuresWanted.names)


# Load the datasets
train <- read.table("UCI HAR Dataset/train/X_train.txt")[featuresWanted]
trainActivities <- read.table("UCI HAR Dataset/train/Y_train.txt")
trainSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
train <- cbind(trainSubjects, trainActivities, train)

test <- read.table("UCI HAR Dataset/test/X_test.txt")[featuresWanted]
testActivities <- read.table("UCI HAR Dataset/test/Y_test.txt")
testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
test <- cbind(testSubjects, testActivities, test)

# merge datasets and add labels
allData <- rbind(train, test)
colnames(allData) <- c("subject", "activity", featuresWanted.names)

# turn activities & subjects into factors
allData$activity <- factor(allData$activity, levels = activityLabels[,1], labels = activityLabels[,2])
allData$subject <- as.factor(allData$subject)

allData.melted <- melt(allData, id = c("subject", "activity"))
allData.mean <- dcast(allData.melted, subject + activity ~ variable, mean)

write.table(allData.mean, "tidy.txt", row.names = FALSE, quote = FALSE)
