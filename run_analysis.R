##Read Data

#Directory where Data is located
path <- file.path("./", "UCI HAR Dataset")

#List of all files in data directory
list.files(path, recursive = TRUE)

#load data.table as it is faster than frame
library(data.table)

#Get Subject IDS
DT.subject.ID.Train <- fread(file.path(path, "train", "subject_train.txt"))
DT.subject.ID.Test <- fread(file.path(path, "test", "subject_test.txt"))

# Get activity labels 
DT.label.Train <- fread(file.path(path, "train", "Y_train.txt"))
DT.label.Test <- fread(file.path(path, "test", "Y_test.txt"))

#load train data into table
df <- read.table(file.path(path, "train", "X_train.txt"))
DT.train <- data.table(df)


#load test data into table
df <- read.table(file.path(path, "test", "X_test.txt"))   
DT.test <- data.table(df)

##Merge Data

#Binding the subject IDs:
DT.All.subject.IDs <- rbind(DT.subject.ID.Train, DT.subject.ID.Test)
#Apply subject IDs 
setnames(DT.All.subject.IDs, "V1", "subject")

#Binding Labels
DT.All.labels <- rbind(DT.label.Train, DT.label.Test)
#Apply labels
setnames(DT.All.labels, "V1", "activity.label")

# merging the train` and test data
DT.Train.and.Test <- rbind(DT.train , DT.test)

#merge the cols
DT.All <- cbind(DT.All.subject.IDs, DT.Train.and.Test)
DT.All <- cbind(DT.All, DT.All.labels)
dim(DT.All)

##Extracting the the measurements on the mean and standard deviation

#The features file contains names from which we can extract the the mean() and STD()

DT.features <- fread(file.path(path, "features.txt"))
setnames(DT.features, names(DT.features), c("feature.number", "feature.name"))
DT.features <- DT.features[grepl("mean\\(\\)|std\\(\\)", feature.name)]
dim(DT.features) 

#Macthing feature into DT.all

DT.features$feature.code <- DT.features[, paste0("V", feature.number)]
tail(DT.features)
DT.features$feature.code

#make subject and activity.label keys
setkey(DT.All, subject, activity.label)
# And append the `feature.code` to this spesifying which columns we want to extract
the.columns.we.want <- c(key(DT.All), DT.features$feature.code)
result <- DT.All[, the.columns.we.want, with=FALSE]
str(result)

##set Descriptive activty names

DT.activity.names <- fread(file.path(path, "activity_labels.txt"))
setnames(DT.activity.names, names(DT.activity.names), c("activity.label", "activity.name"))

##Label data set

DT <- merge(result, DT.activity.names, by = "activity.label", all.x = TRUE)
str(DT)


library(reshape2)
setkey(DT, subject, activity.label, activity.name)
DT <- data.table(melt(DT, key(DT), variable.name = "feature.code"))
DT <- merge(DT, DT.features[, list(feature.number, feature.code, feature.name)], by = "feature.code", 
            all.x = TRUE)

head(DT, n=10); tail(DT, n=10)


##Write write to second data set



### delete everything in the workspace and just leave DT
l = ls()
rm(list=l[l != "DT"])
rm(l)

#copy
dt <- DT

#make feature.name a factor: 

dt[ ,feature := factor(dt$feature.name)]

#Frequency
levels <- matrix(1:2, nrow=2)
logical <- matrix(c(grepl("^t", dt$feature), grepl("^f", dt$feature)), ncol = 2)
dt$Domain <- factor(logical %*% levels, labels = c("Time", "Freq"))


#Accelerometer or Gyroscope?
levels <- matrix(1:2, nrow=2)
logical <- matrix(c(grepl("Acc", dt$feature), grepl("Gyro", dt$feature)), ncol = 2)
dt$Instrument <- factor(logical %*% levels, labels = c("Accelerometer", "Gyroscope"))


#Acceleration
levels <- matrix(1:2, nrow=2)
logical <- matrix(c(grepl("BodyAcc", dt$feature), grepl("GravityAcc", dt$feature)), ncol = 2)
dt$Acceleration <- factor(logical %*% levels, labels = c(NA, "Body", "Gravity"))


#mean and std
logical <- matrix(c(grepl("mean()", dt$feature), grepl("std()", dt$feature)), ncol = 2)
dt$Statistic <- factor(logical %*% levels, labels = c("Mean", "SD"))

#Features on One category 
dt$Jerk <- factor( grepl("Jerk", dt$feature),labels = c(NA, "Jerk"))
dt$Magnitude <- factor(grepl("Mag", dt$feature), labels = c(NA, "Magnitude"))

# Axial variables
levels <- matrix(1:3, 3)
logical <- matrix(c(grepl("-X", dt$feature), grepl("-Y", dt$feature), grepl("-Z", dt$feature)), ncol=3)
dt$Axis <- factor(logical %*% levels, labels = c(NA, "X", "Y", "Z"))


#Create tidy data
dt[ ,activity :=  factor(dt$activity.name)]
setkey(dt, subject, activity, Acceleration, Domain, Instrument, 
       Jerk, Magnitude, Statistic, Axis)
TIDY <- dt[, list(count = .N, average = mean(value)), by = key(dt)]


key(TIDY)

#saveing
f <- file.path(".", "TIDY_HumanActivity.txt")
write.table(TIDY, f, quote = FALSE, sep = "\t", row.names = FALSE)



