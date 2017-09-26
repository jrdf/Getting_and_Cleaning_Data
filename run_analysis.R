##############################################
## Getting and Cleaning Data Course Project ##
##############################################

## Please refer to ReadMe.MD for detailed information.


## loading libraries

	library(dplyr)


## url for downloading data

	fileUrl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"


## create directory data in case it does not exist

	if (!file.exists("data")) {dir.create("data")}


## download files

	download.file(fileUrl, destfile="./data/Dataset.zip", method="auto")
	datedownloaded <- date()


## extract data

	unzip(zipfile="./data/Dataset.zip",exdir="./data")


## read different datasets

	## test data
	X_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
	y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
	subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

	## train data
	X_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
	y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
	subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")

	## features
	features <- read.table("./data/UCI HAR Dataset/features.txt")

	## all = test and train data together
      subject_all <- rbind(subject_test, subject_train)
 	X_all <- rbind(X_test, X_train)
      y_all <- rbind(y_test, y_train)


## name variables, merge datasets for test and train group

	names(subject_all) <- c("subject_id")
      y_all <- mutate(y_all, activity = ifelse(V1 == 1, "walking",
                                         ifelse(V1 == 2, "walking_upstairs",
                                          ifelse(V1 == 3, "walking_downstairs",
                                           ifelse(V1 == 4, "sitting",
                                            ifelse(V1 == 5, "standing",
                                             ifelse(V1 == 6, "laying", "unknown")))))))
      y_all <- select(y_all, activity)

	## dataset with all data for test and train subjects
      all_data <- cbind(subject_all, y_all, X_all, all=TRUE)


## select variables for mean and standard deviation(sd)

      m <- grepl("[Mm][Ee][Aa][Nn]", features$V2)      ### mean variables
      s <- grepl("[Ss][Tt][Dd]", features$V2)          ### sd variables
      a <- grepl("[Aa][Nn][Gg][Ll][Ee]", features$V2)  ### angle variables
      features <- cbind(features, m, s, a)
      features <- mutate(features, sel = ifelse((m == 1 | s == 1) & a == 0, 1, 0))  ### mean and sd (not angle) variables
      selection <- filter(features, sel == 1)
      selection_num <- as.numeric(selection$V1)  ### variable numbers
      selection_char <- paste0("V",selection_num)  ### variable numbers preceded by V
      selection_names <- as.character(selection$V2)  ### variable names  
	selection_names <- gsub("[()]","",selection_names)  ### exclude () from variable names
      selection_names <- gsub("[-]","_",selection_names)  ### change - to _ in variable names

      all_select <- select(all_data, "subject_id", "activity", selection_char)  ### select columns
      colnames(all_select) <- c("subject_id", "activity", selection_names)  ### name variables


## calculate mean per subject and activity

	numvar <- dim(all_select)[2]  ### number of variables in our selected data
      all_select_mean <- aggregate(all_select[, 3:numvar], list(subject_id=all_select$subject_id, activity=all_select$activity), mean)  ### calculate mean by subject and activity
	names(all_select_mean) <- sapply(names(all_select_mean), tolower)  ### convert variable names to lower case

## output

	write.table(all_select_mean, file = "tidy_data.txt", row.names=FALSE)
