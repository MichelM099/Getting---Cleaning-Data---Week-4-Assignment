#
#         Programming Assingment Week 4
# 
# The program flow follows the 5 specific steps detailed in Assignment 4 of the Coursera
# Getting and Cleaning Data course. Basically each step, listed below, is further documented
# in each programming step.
# 
# The initial data was obtained can be obtained from the follow url
#
#    https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#
# and downloaded using the download.file command.
#
# Further information concerning the purpose of the data is available from the url
#
#    http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

# This code uses the dplyr and strigr libraries.

# Define and load required libraries. dplyr allows for data manupulation expressed in a more 
# readable format. The stringr is a stringr library for doing string related operations that are
# expressed to be more user friendly.


# # raw un-processed file to download url
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip


# Load Libraries
library(dplyr)
library(stringr)

#
####################################################

#  Download the zip file, unzip and save to directory in current working directory
#  and erase the zip file.

#  Code to download the original dataset to the currently set workding directory in R session
#  Gets the currently selected working directory, names a file to download the zip content
#  Uses the unzip command and results in the creation of a folder entitled
#  UCI HAR Dataset in the currently selected working directory. The ZIP file is deleted once the
#  downloaded file is unzipped.
#
#  The remainder of the program assumes the selected working directory remains the same.

###################################################

tmpfile <- "/tmpfile.zip"
f_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
wd <- getwd()
wdf <- paste0(wd,tmpfile)
download.file(f_url, wdf)
unzip(wdf) 
file.remove(wdf)

#
###################################################

# Read-in all the files using read.table

##################################################

# Read the column names of the train and test data sets
col_names <- read.table(paste0(wd,"/UCI HAR Dataset/features.txt"), stringsAsFactors = FALSE)

# Read the descriptors to the different activities
activity_desc <- read.table(paste0(wd, "/UCI HAR Dataset/activity_labels.txt"))

# Read the test set data
train_set_subject_id <- read.table(paste0(wd,"/UCI HAR Dataset/train/subject_train.txt"))
train_set_obs <- read.table(paste0(wd,"/UCI HAR Dataset/train/X_train.txt"))
train_set_labels <- read.table(paste0(wd, "/UCI HAR Dataset/train/y_train.txt"))

# Read the train set data
test_set_subject_id <- read.table(paste0(wd,"/UCI HAR Dataset/test/subject_test.txt"))
test_set_obs <- read.table(paste0(wd, "/UCI HAR Dataset/test/X_test.txt"))
test_set_labels <- read.table(paste0(wd, "/UCI HAR Dataset/test/y_test.txt"))


#
###################################################

# Merge data frames to create one data set

##################################################

train_set <- cbind(train_set_subject_id,train_set_labels, train_set_obs)
test_set <- cbind(test_set_subject_id,test_set_labels,test_set_obs)
act_recon <- rbind(test_set,train_set)

#
###################################################

# Assign column names 

##################################################


# Assign names to col_names df

names(col_names)[1] <- "features_id"
names(col_names)[2] <- "features_name"

names(activity_desc)[1] <- "activity_id"
names(activity_desc)[2] <- "activity_name"

# Apply appropriate labels/variable names to the combine dataset
names(act_recon)[1] <- "subjectid"
names(act_recon)[2] <- "activity"
names(act_recon)[3:563] <- col_names$features_name
names(act_recon) <- make.unique(names(act_recon))

#  Properly name the activities with descriptive names

act_recon$act_desc <- activity_desc$activity_name[act_recon$activity]

#
###################################################

# Calculate the mean of all the observations by subject Id and the activity description. All the 
# variables with matches to "std" or "mean", with or without capitals were selected.

##################################################
#

tidyDataSet <- act_recon %>%
                select(matches("-std()|-mean()|subjectid|act_desc", ignore.case = TRUE)) %>%
                group_by(subjectid, act_desc) %>%
                summarise_at(vars(matches("-std|-mean", ignore.case = TRUE)), mean) %>%
                arrange(subjectid, act_desc)

#
###################################################

# Make the dataset tidy by removing commas, white spaces, paranthesis and seperators but leaving
# capital letters because they add clarity to the variable names. Save the file to a txt file.

##################################################
#


names(tidyDataSet) <- str_replace_all(names(tidyDataSet),"[:(),-]", "") # remove (), character


write.table(tidyDataSet, paste0( wd, "/tidyDataSet.txt"))

