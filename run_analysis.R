## Clean Data Programming Course Project
## Jacek K. Urbanek
## January, 2015

## Load-in the data

test_data = read.table("UCI HAR Dataset/test/X_test.txt") # test data
train_data = read.table("UCI HAR Dataset/train/X_train.txt") # train data

test_subjects = read.table("UCI HAR Dataset/test/subject_test.txt") # test subjects
train_subjects = read.table("UCI HAR Dataset/train/subject_train.txt") # train subjects

test_labels = read.table("UCI HAR Dataset/test/y_test.txt") # test labels
train_labels = read.table("UCI HAR Dataset/train/y_train.txt") # train labels

features = read.table("UCI HAR Dataset/features.txt") # features
activity_labels = read.table("UCI HAR Dataset/activity_labels.txt") # activity labels

## Merge the data

total_data = rbind(test_data, train_data)
total_subjects = append(test_subjects[[1]],train_subjects[[1]])
total_labels = append(test_labels[[1]],train_labels[[1]])

## Subset the Data
## Get indices of mean

meanInd = grep("mean",features[,2])

## Get indices of STD

stdInd = grep("std",features[,2])

## Merge the indices

ind=append(meanInd,stdInd)

## Subset the Data

data_subset = total_data[,ind]

## Rename Variables

colnames(data_subset) = features[ind,2]

## Apply Activity Names

library(plyr)
proper_labels = mapvalues(as.factor(total_labels), from = as.character(activity_labels[[1]]), to = as.character(activity_labels[[2]]))

## Run analysis

s=split(data_subset,interaction(total_subjects,proper_labels))

a=names(s)
b=lapply(s,colMeans)

##### Loop to merge resulting data

cleanData=numeric() ## Empty vector for clean data results
SubjectID=factor() ## empty vector for subjects IDs
ActivityID=factor() ## empty vector for activity IDs

LV_subjects=levels(as.factor(total_subjects)) ## levels for subjects
LV_labels=levels(proper_labels) ## levels for labels
for (i in 1:length(LV_subjects) ) {
  for (j in 1:length(LV_labels) ) {
  
  unlistedData=unlist(b[i*j],use.names=F)
  cleanData=rbind(cleanData,unlistedData,deparse.level=0)
  SubjectID=rbind(SubjectID,LV_subjects[i])
  ActivityID=rbind(ActivityID,LV_labels[j])
  
}
}
cleanData=as.data.frame(cleanData)
colnames(cleanData) = features[ind,2]

cleanData$SubjectID=SubjectID
cleanData$ActivityID=ActivityID

## Save the results

write.table(cleanData,"cleanData.txt") 
