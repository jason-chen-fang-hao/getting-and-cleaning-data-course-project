# getting-and-cleaning-data-course-project
Getting and Cleaning Data Course Project
The R script, run_analysis.R, does the following:

Getting and merging the data sets by loading the raw dats sets and  then combining them into a single data set
Loads both the training and test datasets, keeping only those columns which reflect a mean or standard deviation by xData subset based on the logical vector to keep only desired columns, i.e. mean() and std()
Loads the activity and subject data for each dataset, and merges those columns with the dataset
Defining descriptive names for all variables.
Creates a tidy dataset that consists of the average value of each variable for each subject and eachactivity.
