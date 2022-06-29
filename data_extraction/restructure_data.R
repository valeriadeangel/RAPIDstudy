
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


library(dplyr)
library(readr)


list(sub_dir_list)

setwd("E:/")

DESIRED_FEATURE="connect_fitbit_sleep_classic" # change features here

#creates a list of all the participants
parent_lst<- list.files(path="./WinSCP files", pattern=NULL, all.files=FALSE, 
           full.names=TRUE)


## Basic
for(x in 1:length(parent_lst))
{
#creates a list of all the subfolders within the participants - so the features
  sub_dir_list<-list.files(path=parent_lst[x], pattern="", all.files=FALSE,
                           full.names=TRUE)
  
  #generates folders where the features will be stored
  folder_names <- list.files(path=parent_lst[x], pattern="", all.files=FALSE,
                             full.names=FALSE)
  
  #Loop through the feature folders
  for (y in 1:length(sub_dir_list))
  {
    #but only if folder name exists
    if (folder_names[y]==DESIRED_FEATURE)
      {
    sub_dir_list_df <- list.files(path=sub_dir_list[y], pattern="csv.gz", all.files=FALSE,
                             full.names=TRUE)%>%
      
      #read the cvs files and bind them all rowwise
      lapply(read_csv) %>% 
      bind_rows %>%distinct
    dir.create(file.path('./', DESIRED_FEATURE), showWarnings = FALSE)

    #create a folder for each participant 
    write_csv(sub_dir_list_df,paste("./",DESIRED_FEATURE,'/',folder_names[y],"_sub",x,".csv",sep=''))
    #remove the lists of subfolders from R memory
    rm(sub_dir_list_df)
    }
  }
  
}
# now create master file with all the subfolders from all partcipants together
master_file<- list.files(path=paste('./',DESIRED_FEATURE,sep=''), pattern=NULL, all.files=FALSE,
                        full.names=TRUE)%>% 
  lapply(read_csv) %>% 
  bind_rows %>%distinct
#and create a csv file for it
write_csv(master_file,paste("./",DESIRED_FEATURE,'/',"master_",DESIRED_FEATURE,".csv",sep=''))

# delete the files that are now joined
junk <- list.files(path=paste('./',DESIRED_FEATURE,sep=''),  pattern="sub",full.names = TRUE)
file.remove(junk,showWarnings = FALSE)




