# Set working directory
setwd("~/Coursera/3DataCleaning/Project")

# Install and load packages if required
if (!require("data.table")) {
        install.packages("data.table")
}

if (!require("reshape2")) {
        install.packages("reshape2")
}

require("data.table")
require("reshape2")

# Read the files

# Read the subject files
subject_test <- read.table("./test/subject_test.txt")
subject_train <- read.table("./train/subject_train.txt")

# Read the activity files
y_test <- read.table("./test/y_test.txt")
y_train <- read.table("./train/y_train.txt")

# read the data files
X_test <- read.table("./test/X_test.txt")
X_train <- read.table("./train/X_train.txt")

# Load labels for data files. 

features <- read.table("./features.txt")[,2]
names(X_test) = features
names(X_train) = features

##################################################################
# 1. Merges the training and the test sets to create one data set.
##################################################################

#Concatenate the data tables.
dtSubject <- rbind(subject_train, subject_test)
setnames(dtSubject, "V1", "subject")
dtActivity <- rbind(y_train, y_test)
setnames(dtActivity, "V1", "activityNum")
dt <- rbind(X_train, X_test)

#Merge columns.
dtSubject <- cbind(dtSubject, dtActivity)
dt <- cbind(dtSubject, dt)


############################################################################################
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
############################################################################################
dtMeanSt <- dt[,grepl("mean|std|subject|activityNum", names(dt))]

###########################################################################
# 3. Uses descriptive activity names to name the activities in the data set
###########################################################################
# Load: activity labels
activity_labels <- read.table("activity_labels.txt")
names(activity_labels) = c("activityNum", "activityName")

# Inclusion of activity label merging
dtLabeled <- merge(dtMeanSt, activity_labels)
# Reorder the columns
dtLabeled<- dtLabeled[ ,c(1:2, 82, 3:81)]

##############################################################
# 4. Appropriately labels the data set with descriptive names.
##############################################################

# Remove parentheses
names(dtLabeled) <- gsub('\\(|\\)',"",names(dtLabeled), perl = TRUE)
# Make syntactically valid names
names(dtLabeled) <- make.names(names(dtLabeled))
# Make clearer names
names(dtLabeled) <- gsub('Acc',"Acceleration",names(dtLabeled))
names(dtLabeled) <- gsub('GyroJerk',"AngularAcceleration",names(dtLabeled))
names(dtLabeled) <- gsub('Gyro',"AngularSpeed",names(dtLabeled))
names(dtLabeled) <- gsub('Mag',"Magnitude",names(dtLabeled))
names(dtLabeled) <- gsub('^t',"Time.",names(dtLabeled)) # everything starts with t
names(dtLabeled) <- gsub('^f',"Frequency.",names(dtLabeled)) # everything starts with f
names(dtLabeled) <- gsub('\\.mean',".Mean",names(dtLabeled))
names(dtLabeled) <- gsub('\\.std',".StandardDeviation",names(dtLabeled))
names(dtLabeled) <- gsub('MeanFreq\\.',"Mean",names(dtLabeled))


######################################################################################################################
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
######################################################################################################################

dt_melt <- melt(dtLabeled, id = c("subject", "activityNum", "activityName"))
dt_tidy <- dcast(dt_melt, subject + activityNum + activityName ~ variable, mean)
write.table(dt_tidy, file = "./tidy_data.txt")
