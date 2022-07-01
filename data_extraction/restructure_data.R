
#############                                                     #############

#### This code restructures the data, where the current folder structure is:
## E:/WinSCP files/participant folders/digital features/date_time.csv

#   participant folders (e.g. af0f5743-597c-4098-a1a8-dbfca3981455)/
#   digital features    (e.g. connect_fitbit_sleep_classic)

# to the following structure:
# E:/digital features/concatenated features from ALL participants.csv

#   digital features     (e.g. connect_fitbit_sleep_classic)
#   concatenated features (e.g. master_connect_fitbit_sleep_classic.csv)


#############                                                     #############


library(tidyverse)

setwd("F:/")  # check that this is the correct path to the external hard drive.

DESIRED_FEATURE="questionnaire_qids" # change features here


#creates a list of all the participants
participants<- list.files(path="./WinSCP files", pattern=NULL, all.files=FALSE, 
           full.names=TRUE)


## Loop for extraction
for(x in 1:length(participants))
{
  #creates a list of all the subfolders within the participants - so the features
  features<-list.files(path=participants[x], pattern="", all.files=FALSE,
                           full.names=TRUE)
  
  #generates folders where the features will be stored
  folder_names <- list.files(path=participants[x], pattern="", all.files=FALSE,
                             full.names=FALSE)
  
  
  #Loop through the feature folders
  for (y in 1:length(features))
  {
    #but only if folder name exists
    if (folder_names[y]==DESIRED_FEATURE)
    {
      #creates the list for the name of the file (i.e. the date/time of the date point)
      features_timestamp <- list.files(path=features[y], pattern="csv.gz", all.files=FALSE,
                                           full.names=FALSE) %>%
        str_replace(".csv.gz","")
      
      #creates a list of the files themselves
      features_files<- list.files(path=features[y], pattern="csv.gz", all.files=FALSE,
                                      full.names=TRUE)
      
      #combines all the csv files, including the name as a variable
      a <- Map(cbind, lapply(features_files, read_csv), timestamp = features_timestamp)
      features_df <- Reduce(full_join,a) %>% distinct()
      
      #creates a folder in which to store these features
      dir.create(file.path('./', DESIRED_FEATURE), showWarnings = FALSE)
      #creates a file of concatenated csv files for all participants and stores here
      write_csv(features_df,paste("./",DESIRED_FEATURE,'/',folder_names[y],"_sub",x,".csv",sep=''))
      #removes the unwanted lists from R memory
      rm(features_df)
    }
  }
  
}
  
  
# now create master file with all the subfolders from all participants together
master_file<- list.files(path=paste('./',DESIRED_FEATURE,sep=''), pattern=NULL, all.files=FALSE,
                        full.names=TRUE)%>% 
  lapply(read_csv) %>% 
  bind_rows %>%distinct

#and create a csv file for it
write_csv(master_file,paste("./feature folders/","master_",DESIRED_FEATURE,".csv",sep=''))

# delete the files that are now joined
junk <- list.files(path=paste('./',DESIRED_FEATURE,sep=''),  pattern="sub",full.names = TRUE)
file.remove(junk,showWarnings = FALSE)
