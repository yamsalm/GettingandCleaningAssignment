library(readr)
library(stringr)
library(dplyr)
library(tidyr)
## will load a data using the paths 
createSet <-function(x,y,subject){
  ## loadData
  X_test <- read_fwf(x,fwf_widths(rep( c(16),561)))
  ##load Users
  users<-read_delim(subject,",",col_names = FALSE)
  names(users)<- c("UserId")
  
  ##load  activty
  activ<-read_delim(y,",",col_names = FALSE)
  names(activ)<- c("ActivityId")
  activ<-mutate(activ,Activity= factor(ActivityId,labels = c ("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING") ))
  
  ##load and filter field Names
  featuresName <- read_delim("UCI HAR Dataset/features.txt",  " ", escape_double = FALSE, col_names = FALSE,     trim_ws = TRUE)
  
  names( X_test) <- featuresName[[2]]
  names<- featuresName[[2]]
  
  meanAndStdNames <- names[(str_detect(names, "mean") & !str_detect(names, "meanFreq"))|str_detect(names, "std") ]
  ##filter fields by name
  meanAndStdData<-X_test[, meanAndStdNames]
  ## join users data and Activety data
  dataWithUserAndActiv<-cbind(users,activ,meanAndStdData)
  dataWithUserAndActiv
}

## run create set funcytion on test and train
testSet<-createSet("UCI HAR Dataset/test/X_test.txt","UCI HAR Dataset/test/y_test.txt","UCI HAR Dataset/test/subject_test.txt")

trainSet<-createSet("UCI HAR Dataset/train/X_train.txt","UCI HAR Dataset/train/y_train.txt","UCI HAR Dataset/train/subject_train.txt")
## union set
fullSet<-rbind(testSet,trainSet)
## unpivot Data by activty
tidySet<- fullSet %>%
     gather(MeasuremenName, MeasuremenValue, -UserId,-ActivityId,-Activity) 
## get the mean val for each activty for each  Measuremen
g<-group_by(tidySet,Activity,MeasuremenName)
meanSet<- summarise(g,MeanValue = mean(MeasuremenValue))
write.table(meanSet,"meanSetTable")