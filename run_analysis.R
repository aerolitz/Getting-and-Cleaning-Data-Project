## The purpose of this project is to demonstrate your ability to collect, 
## work with, and clean a data set. The goal is to prepare tidy data that 
## can be used for later analysis. You will be graded by your peers on a 
## series of yes/no questions related to the project.
## You will be required to submit:
##   1) a tidy data set as described below
##   2) a link to a Github repository with your script for performing the
##      analysis
##   3) a code book that describes the variables, the data, and any 
##      transformations or work that you performed to clean up the data 
##      called CodeBook.md. 
## You should also include a README.md in the repo with your scripts. 
## This repo explains how all of the scripts work and how they are 
## connected.  

## One of the most exciting areas in all of data science right now is 
## wearable computing - see for example this article. 
## Companies like Fitbit, Nike, and Jawbone Up are racing to develop the 
## most advanced algorithms to attract new users. 
## The data linked to from the course website represent data collected 
## from the accelerometers from the Samsung Galaxy S smartphone. 
## A full description is available at the site where the data was 
## obtained: 
        
## http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

## Here are the data for the project: 
        
## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

## You should create one R script called run_analysis.R that does the 
## following:
## 1 - Merges the training and the test sets to create one data set.
## 2 - Extracts only the measurements on the mean and standard deviation
##     for each measurement. 
## 3 - Uses descriptive activity names to name the activities in the data
##     set.
## 4 - Appropriately labels the data set with descriptive variable names. 
## 5 - From the data set in step 4, creates a second, independent tidy 
##     data set with the average of each variable for each activity and 
##     each subject.



## 0-A - Loading libraries

# Checking if necessary libraries are in place
if (!require("data.table")) {
        install.packages("data.table")
}

if (!require("reshape2")) {
        install.packages("reshape2")
}

if (!require("plyr")) {
        install.packages("plyr")
}
# Loading libraries
require("data.table")
require("reshape2")
require("plyr")


## 0-B - Loading files
tabela_al <- read.table("~/R/getdata-012/UCI HAR Dataset/activity_labels.txt")
tabela_f <- read.table("~/R/getdata-012/UCI HAR Dataset/features.txt")
teste_x <- read.table("~/R/getdata-012/UCI HAR Dataset/test/X_test.txt")
teste_y <- read.table("~/R/getdata-012/UCI HAR Dataset/test/y_test.txt")
teste_s <- read.table("~/R/getdata-012/UCI HAR Dataset/test/subject_test.txt")
treino_x <- read.table("~/R/getdata-012/UCI HAR Dataset/train/X_train.txt")
treino_y <- read.table("~/R/getdata-012/UCI HAR Dataset/train/y_train.txt")
treino_s <- read.table("~/R/getdata-012/UCI HAR Dataset/train/subject_train.txt")


## 1 - Merges the training and the test sets to create one data set.
tabela_x <- rbind(teste_x, treino_x)
tabela_y <- rbind(teste_y, treino_y)
tabela_s <- rbind(teste_s, treino_s)


## 2 - Extracts only the measurements on the mean and standard deviation
##     for each measurement. 

# Selecting "mean" or "standard deviation" (std) only
features <- grepl("mean|std", tabela_f[, 2])
# Applying selection at "Table X"
tabela_x <- tabela_x[, features]



## 3 - Uses descriptive activity names to name the activities in the data
##     set.

# Removing "_" and changing to lower case the labels at "Table AL"
tabela_al[, 2] <- gsub("_", "", tolower(as.character(tabela_al[, 2])))
# Applying the labels in "Table AL" at "Table Y"
tabela_y[, 1] <- tabela_al[tabela_y[, 1], 2]



## 4 - Appropriately labels the data set with descriptive variable names.

# Loading features names into the columns labels a "Table X"
names(tabela_x) <- tolower(tabela_f[features, 2])
# Removing parentesis from column labels (just elegancy)
names(tabela_x) <- gsub("\\(|\\)", "", names(tabela_x))
# Renaming the column name to "activity" at "Table Y"
names(tabela_y) <- "activity"
# Renaming the column name to "subject" at "Table S"
names(tabela_s) <- "subject"
# Unify the 3 data tables into a single table
tabela <- cbind(tabela_s, tabela_y, tabela_x)


## 5 - From the data set in step 4, creates a second, independent tidy 
##     data set with the average of each variable for each activity and 
##     each subject.

# Reshaping the data set into rows, only keeping "Subject" and "Activity"
# as columns. The other variables are converted into single rows
listao <- melt(tabela, id = c("subject", "activity"))
# Sumarize data, getting the mean (average) of "value" column
tidy_data <- ddply(listao, c("subject", "activity", "variable"), summarise, mean = mean(value))
# Renaming columns
names(tidy_data) <- c("subject", "activity", "activity_labels", "average")
# Exporting table to file
write.table(tidy_data, file = "~/R/getdata-012/tidy_data.txt", row.name = FALSE)