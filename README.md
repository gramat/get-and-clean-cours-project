####The repo get-and-clean-cours-project contains
Coursera.org

Data Science Specialization

Getting & Cleaning Data course croject files

D.Gramatchikov

######Script run_analysis.R
######NEEDS:
The training and test data sets and descripting files extracted from 

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip.

Folder UCI HAR Dataset must be in the current work directory.


######DOES:
* Merges the training and the test sets to create one data set.
* Extracts only the measurements on the mean and standard deviation for each measurement. 
* Uses descriptive activity names to name the activities in the data set
* Appropriately labels the data set with descriptive activity names. 
* Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

**Preporatory steps are:**

Write the function getting pathnames for necessary data files

and make possible to test are there these files at all

(in UCI HAR Dataset folder or in train folder

or just in work directory)

    getDataPath <- function(dataDir, dataSubDir, dataFile){
      if(file.exists(file.path(dataDir,dataSubDir, dataFile))){
        return(file.path(dataDir,dataSubDir, dataFile))
      }else{
        if(file.exists(file.path(dataSubDir, dataFile))){
          return(file.path(dataSubDir, dataFile))
        }else{
          if(file.exists(file.path(dataFile))){
            return(file.path(dataFile))
          }
          else{
            return(NA)
          }
        }
      }
    }


define the folder names and data files variables


        dataDir <- file.path("UCI HAR Dataset")
        trainDir <- file.path("train")
        testDir <- file.path("test")
        dataFiles <- c("X_train.txt", "subject_train.txt", "y_train.txt", "X_test.txt", "subject_test.txt", "y_test.txt", "activity_labels.txt", "features.txt")
        dataPaths <- vector(mode="character", length=8)

test if files exist and if they do - get path to files

        for(i in c(1:3)){
            dataPaths[i] <- getDataPath(dataDir, trainDir, dataFiles[i])
        }
        for(i in c(4:6)){
            dataPaths[i] <- getDataPath(dataDir, testDir, dataFiles[i])
        }
        for(i in c(7:8)){
            dataPaths[i] <- getDataPath("", dataDir, dataFiles[i])
        }


if some files do not exist - stop script

        if(sum(is.na(dataPaths)) > 0){
            stop("Sorry, files: ", dataFiles[is.na(dataPaths)], " - not found. Script is stopped.")
  
        }

else, if all files are found

**getting train data**

        xTrain <- read.table(dataPaths[1])
        subjectTrain <- read.table(dataPaths[2], col.names=c("subject"))
        yTrain <- read.table(dataPaths[3], col.names=c("activity"))
        trainData <- cbind(subjectTrain, yTrain, xTrain)

**train data set is ready;**

remove subjectTrain, yTrain, xTrain (just for memory saving)

        rm(subjectTrain, yTrain, xTrain)

**getting test data**

        xTest <- read.table(dataPaths[4])
        subjectTest <- read.table(dataPaths[5], col.names=c("subject"))
        yTest <- read.table(dataPaths[6], col.names=c("activity"))
        testData <- cbind(subjectTest, yTest, xTest)
        
        
**test data set is ready;**

remove subjectTest, yTest, xTest

        rm(subjectTest, yTest, xTest)

**Combine the Data in samsungData set**

        samsungData <- rbind(trainData, testData)
        rm(testData, trainData)
        
**get the names of activities**

        actNames <- read.table(dataPaths[7])
        actNames <- as.vector(actNames[,2], mode="character")
        
**set activities data in data set as factor with activities names**

        samsungData$activity <- as.factor(samsungData$activity)
        levels(samsungData$activity) <- actNames;
        
**get the labels of data set variables**

        varNames <- read.table(dataPaths[8])
        varNames <- as.vector(varNames[,2], mode="character")
        
**transform labels**

        varNames <- gsub("[-(),]", ".", varNames)
        varNames <- gsub("[^0-9A-Za-z]+$", "", varNames) 
        varNames <- gsub("[^0-9A-Za-z]+", ".", varNames)
        
**get index of mean and std mesurements variables**

        varIndex <- sort(c(grep("mean[^0-9A-Za-z]|mean[^0-9A-Za-z]*$", varNames, ignore.case=F), grep("std", varNames, ignore.case=F)))

**define labels for selected columns**

        varNames <- varNames[varIndex]

**Extract only the measurements on the mean and standard deviation for each measurement.**

        varIndex <- varIndex+2
        samsungData <- samsungData[,c(1:2, varIndex)]

**set labels**

        colnames(samsungData) <- c(colnames(samsungData[,1:2]), varNames)
        
**create new data set - tidyData**

column names as in samsungData and 180 rows (30 subjects * 6 activities)

        tidyData <- samsungData[1:180, ]
        
**assign the cells the average of each variable for each activity and each subject.**
        for(i in c(1:30)){
          for(j in c(1:6)){
            rowNum <- 6*(i-1)+j
            tidyData[rowNum, 1] <- i
            tidyData[rowNum, 2] <- levels(samsungData$activity)[j]
            for(k in c(3:ncol(samsungData))){
                tidyData[rowNum, k] <- mean(samsungData[(samsungData$subject == i) & (samsungData$activity == levels(samsungData$activity)[j]),k])      
          }
         }
        }
        
**save data in a file**

        write.table(tidyData, "tidy_data.txt")

