run_analysis <- function()
{
  library(reshape2); ## Functions in this library will be used to reshape the dataset later on 
  ##Read in the 3 data files in the test directory
  xtest = read.table('./UCI HAR Dataset/test/X_test.txt', header = FALSE, stringsAsFactors = F, fill = T)
  ytest = read.table('./UCI HAR Dataset/test/y_test.txt', header = FALSE, stringsAsFactors = F, fill = T, col.names = c("Activity_ID"))
  subtest = read.table('./UCI HAR Dataset/test/subject_test.txt', header = FALSE, stringsAsFactors = F, fill = T, col.names = c("Subject_ID"))
  ##Read in the 3 data files in the train directory
  xtrain = read.table('./UCI HAR Dataset/train/X_train.txt', header = FALSE, stringsAsFactors = F, fill = T);
  ytrain = read.table('./UCI HAR Dataset/train/y_train.txt', header = FALSE, stringsAsFactors = F, fill = T, col.names = c("Activity_ID"));
  subtrain = read.table('./UCI HAR Dataset/train/subject_train.txt', header = FALSE, stringsAsFactors = F, fill = T, col.names = c("Subject_ID"));
  
  ##Read in the features to be used to identify the columns later
  features = read.table('./UCI HAR Dataset/features.txt', header = FALSE, stringsAsFactors = F, fill = T);
  ##Read in the activity labels to be used to describe the activities
  activity = read.table('./UCI HAR Dataset/activity_labels.txt', header = FALSE, stringsAsFactors = F, fill = T, col.names = c("ID","Description"));  
  ##assign colnames to the test data set
  colnames(xtest) <- features[,2];
  ##assign colnames to the train data set
  colnames(xtrain) <- features[,2];
  ##append the subject information and activity id's to the main test data set 
  test_data <- cbind(subtest,ytest,xtest);
  ##append the subject information and activity id's to the main train data set 
  train_data <- cbind(subtrain,ytrain,xtrain);
  
  ##combine the test & train data sets
  all_data <- rbind(test_data, train_data);
  
  ##Attach the Activity labels based on the corresponding activity ID 
  merged_data <- merge(all_data,activity,by.x="Activity_ID",by.y="ID",all=TRUE);
  
  ##Find the columns that measure the mean and standard deviation for each measurement
  colmeansstd <- grep("mean|std",names(merged_data));
  
  ##Extracts only the measurements on the mean and standard deviation for each measurement
  mean_std_data <- merged_data[,c(1,2,564,colmeansstd)];
  
  ##Melt the data to allow for casting
  meltData <- melt(mean_std_data, id=c(1:3), measure.vars=c(4:82));
  ##Cast the average of each variable for each activity and each subject
  tidy_data <- dcast(meltData, Subject_ID + Description ~ variable,mean);
  
  ##rename the columns for easier readability
  names(tidy_data)<- sub("()","",names(tidy_data),fixed=TRUE);
  
  
  #write theoutput to file
  write.table(tidy_data, file="tidy_data.txt",row.names=FALSE);

}
  
