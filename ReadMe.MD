## ReadMe for Getting and Cleaning Data Course Project

This ReadMe gives information about the steps performed and datasets generated for the Getting and Cleaning Data Course Project from Coursera.

The tasks for this Course Project are the following (copied from the Coursera website):
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.
You should create one R script called run_analysis.R that does the following. 
*1.Merges the training and the test sets to create one data set.
*2.Extracts only the measurements on the mean and standard deviation for each measurement. 
*3.Uses descriptive activity names to name the activities in the data set
*4.Appropriately labels the data set with descriptive variable names. 
*5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

An R script called run_analysis.R is provided that downloads the files for this Course Project from the internet ("http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip") and stores them into a data directory (that is created if not already existing) in the current working directory. After extracting, the files needed for further processing are read into R, merged and transformed as needed and finally, a tidy dataset (tidy_data.csv) is written to the working directory that includes the variables as described in CodeBook.MD.

The input files need to be merged as they are delivered in two separate sets - one for the training group and one for the test group. As a first step, information on subject ID, activity and the respective measurements/estimations were combined based on the information provided with the data. A selection on mean and standard deviation of the different variables was processed as a second step as only those variables should be chosen according to the Course Project definitions. Furthermore, the mean of those variables - by subject ID and activity - was calculated and written to the output file (i.e. tidy_data.csv). The result is a comma-separated values (CSV) file with 180 observations (6x30, for the 30 subjects and 6 activities each). For further information on the respective transformations and calculations, please also see the comments in run_analysis.R.
The tidy_data.csv can be read into R using the following code:
read.table("tidy_data.txt", header=TRUE)

In addition to this, there is a file called CodeBook.MD that lists the variables included in tidy_data.csv with their respective explanations.

For further information on the underlying data, please see the documentation provided with the data (cf. link above) or contact the respective web page: 
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones