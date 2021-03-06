#README

##Instructions

The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit:  

1) a tidy data set as described below,  
2) a link to a **Github** repository with your script for performing the analysis, and  
3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called **CodeBook.md**.  

You should also include a **README.md** in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.  

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:  

http://web.archive.org/web/20140803224107/http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones  

Here are the data for the project:  

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip  

You should create one R script called **run_analysis.R** that does the following.  

- Merges the training and the test sets to create one data set.  
- Extracts only the measurements on the mean and standard deviation for each measurement.  
- Uses descriptive activity names to name the activities in the data set.  
- Appropriately labels the data set with descriptive variable names.  
- Creates a second, independent tidy data set with the average of each variable for each activity and each subject.  

*Good luck!*



##Implementation

As this project is very specific, much tightened to the data spreaded in all those text files, I prefer not to use functions, but to write one single flat code that addresses each task at the proper time.  

The mandatory steps are identified with comments and the names of the variables are self explained, I hope so.  I know that this approach will generate one big chunk of code, but I think that this result is more easily readable.  

The script is aside this document and assumes that your working directory contains the data downloaded in one directory named "**UCI HAR Dataset**".  

The result will be written at the same directory and named "**result.txt**".   


```
## [1] "Thanks!"
```

##License

Use of this dataset in publications must be acknowledged by referencing the following publication [1]  

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012  

This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.  

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.  

##The Project

Human Activity Recognition Using Smartphones Dataset  
Version 1.0  

Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.  
Smartlab - Non Linear Complex Systems Laboratory  
DITEN - Università degli Studi di Genova.  
Via Opera Pia 11A, I-16145, Genoa, Italy.  
activityrecognition@smartlab.ws  
www.smartlab.ws  


The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.  

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details.  
