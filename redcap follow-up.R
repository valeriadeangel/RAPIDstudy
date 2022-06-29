library(plyr)
library(dplyr)
library(tidyverse)
library(reshape2)
library(data.table)
library(broom)
library(readr)
library(readxl)
library(stringr)

#documents needed:
# 1. followup_redcap (1)
# 2. followup redcap (3)
# 3. follow up_redcap (4)
# 4. 2. demographic table (3)
# 5. 3. Redcap active data (5)



#create the follow-up dataset
followup <- read_excel("~/R/followup_redcap.xlsx")  #this is REDcap follow up data availability (have participants replied to questionnaires each week?)

#names(followup)
#head(followup[,1:3])

#clean variable recap_event_name
event_name <- followup$redcap_event_name %>%        #creates a new variable without "_arm_1"
  str_remove_all("_arm_1")
followup <- followup %>%                            #which you then need to add to the dataframe - replacing redcap_event_name
  mutate(redcap_event_name = event_name)
followup <- followup %>%                            #delete 2 columns that are not clinical data
  filter(redcap_event_name != "2_month_progress")
followup <- followup %>%
  filter(redcap_event_name != "your_treatment_and")

#unique(followup$redcap_event_name)


tx_dataset <- read_excel("~/R/2. Demographic Table.xlsx",   #dataset of treatment dates
                         sheet = 3, range = "A1:M67", 
                         col_types = c("text", "numeric", "date", 
                                       "date", "numeric", "date", "numeric", 
                                       "date", "numeric", "numeric", "numeric", 
                                       "numeric", "text"))

#ls(tx_dataset)
#(tx_dataset$`Treatment start date`)-(tx_dataset$`Date of assessment`)  # check that date of assessment always precedes tx start dates 



# Create tx status data frame with ALL participants WIDE -----------------------------------------------
# WIDE format treatment status

P14 <- c(rep(1,5), rep(2,23), rep(3, 12), rep(NA,0))
P15 <- c(rep(1,2), rep(2,21), rep(3, 9), rep(NA,8))
P16 <- c(rep(1,2), rep(2,16), rep(3, 13), rep(NA,9))
P17 <- c(rep(1,3), rep(2,24), rep(3, 4), rep(NA,9))
P18 <- c(rep(1,31), rep(2,0), rep(3, 0), rep(NA,9)) 
P19 <- c(rep(1,1), rep(2,10), rep(3, 17), rep(NA,12))
P20 <- c(rep(1,4), rep(2,16), rep(3, 7), rep(NA,13))
P21 <- c(rep(1,10), rep(2,22), rep(3, 6), rep(NA,2))
P22 <- c(rep(1,13), rep(2,23), rep(3, 4), rep(NA,0))
P23 <- c(rep(1,3), rep(2,4), rep(3, 31), rep(NA,2))
P24 <- c(rep(1,2), rep(2,13), rep(3, 12), rep(NA,13))
P25 <- c(rep(1,2), rep(2,7), rep(3, 31), rep(NA,0))
P26 <- c(rep(1,1), rep(2,6), rep(3, 13), rep(NA,20))
P27 <- c(rep(1,4), rep(2,14), rep(3, 14), rep(NA,8))
P28 <- c(rep(1,6), rep(2,18), rep(3, 16), rep(NA,0))
P29 <- c(rep(1,3), rep(2,14), rep(3, 16), rep(NA,7))
P30 <- c(rep(1,28), rep("NA", 12))
P31 <- c(rep(1,37), rep("NA", 3))
P32 <- c(rep(1,2), rep(2,18), rep(3, 16), rep(NA,4))
P33 <- c(rep(1,5), rep(2,7), rep(3, 1), rep(NA,27))
P34 <- c(rep(1,6), rep(2,0), rep(3, 29), rep(NA,5))
P35 <- c(rep(1,12), rep(2,11), rep(3, 13), rep(NA,4))
P36 <- c(rep(1,4), rep(2,20), rep(3, 15), rep(NA,1))
P37 <- c(rep(1,2), rep(2,0), rep(3, 30), rep(NA,8))
P38 <- c(rep(1,5), rep(2,15), rep(3, 15), rep(NA,5))
P39 <- c(rep(1,3), rep(2,7), rep(3, 18), rep(NA,12))
P40 <- c(rep(1,8), rep(2,11), rep(3, 12), rep(NA,9))
P41 <- c(rep(1,1), rep(2,11), rep(3, 17), rep(NA,11))
P42 <- c(rep(1,2), rep(2,12), rep(3, 12), rep(NA,14))
P43 <- c(rep(1,2), rep(2,13), rep(3, 12), rep(NA,13))
P44 <- c(rep(1,7), rep(2,10), rep(3, 15), rep(NA,8))
P45 <- c(rep(1,30), rep("NA", 10))
P46 <- c(rep(1,3), rep(2,6), rep(3, 22), rep(NA,9))
P47 <- c(rep(1,30), rep("NA", 10))
P48 <- c(rep(1,3), rep(2,6), rep(3, 19), rep(NA,12))
P49 <- c(rep(1,19), rep(2,8), rep(3, 13), rep(NA,0))
P50 <- c(rep(1,7), rep(2,6), rep(3, 14), rep(NA,13))
P51 <- c(rep(1,2), rep(2,19), rep(3, 15), rep(NA,4))
P52 <- c(rep(1,3), rep(2,26), rep(3, 6), rep(NA,5))
P53 <- c(rep(1,4), rep(2,0), rep(3, 18), rep(NA,18))
P54 <- c(rep(1,2), rep(2,11), rep(3, 14), rep(NA,13))
P55 <- c(rep(1,1), rep(2,5), rep(3, 13), rep(NA,21))
P56 <- c(rep(1,2), rep(2,5), rep(3, 15), rep(NA,18))
P57 <- c(rep(1,1), rep(2,6), rep(3, 21), rep(NA,12))
P58 <- c(rep(1,22), rep(2, 18))
P59 <- c(rep(1,6), rep(2,6), rep(3, 14), rep(NA,14))
P60 <- c(rep(1,8), rep(2,7), rep(3, 15), rep(NA,10))
P61 <- c(rep(1,2), rep(2,6), rep(3, 14), rep(NA,18))
P62 <- c(rep(1,15), rep("NA", 25))
P63 <- c(rep(1,26), rep("NA", 14))
P64 <- c(rep(1,2), rep(2,7), rep(3, 13), rep(NA,18))
P65 <- c(rep(1,2), rep(2,9), rep(3, 23), rep(NA,6))
P66 <- c(rep(1,1), rep(2,4), rep(3, 15), rep(NA,20))
P67 <- c(rep(1,7), rep(2,14), rep(3, 13), rep(NA,6))
P68 <- c(rep(1,2), rep(2,7), rep(3, 14), rep(NA,17))
P69 <- c(rep(1,1), rep(2,5), rep(3, 15), rep(NA,19))
P70 <- c(rep(1,8), rep(2,14), rep(3, 14), rep(NA,4))
P71 <- c(rep(1,3), rep(2,7), rep(3, 15), rep(NA,15))
P72 <- c(rep(1,6), rep(2,6), rep(3, 14), rep(NA,14))
P73 <- c(rep(1,5), rep(2,23), rep(3, 9), rep(NA,3))
P74 <- c(rep(1,16), rep(2,5), rep(3, 15), rep(NA,4))
P75 <- c(rep(1,2), rep(2,6), rep(3, 18), rep(NA,14))
P76 <- c(rep(1,18), rep(2,14), rep(3, 4), rep(NA,4))
P77 <- c(rep(1,1), rep(2,6), rep(3, 10), rep(NA,23))
P78 <- c(rep(1,2), rep(2,8), rep(3, 13), rep(NA,17))
P79 <- c(rep(1,1), rep(2,5), rep(3, 14), rep(NA,20))

tx_status <- cbind(P14, 
                   P15, 
                   P16, 
                   P17, 
                   P18,
                   P19, 
                   P20, 
                   P21, 
                   P22, 
                   P23, 
                   P24, 
                   P25, 
                   P26, 
                   P27, 
                   P28, 
                   P29, 
                   P30,
                   P31,
                   P32, 
                   P33, 
                   P34,
                   P35, 
                   P36, 
                   P37,
                   P38, 
                   P39, 
                   P40, 
                   P41, 
                   P42, 
                   P43, 
                   P44, 
                    P45, 
                   P46, 
                    P47, 
                   P48, 
                   P49, 
                   P50, 
                   P51, 
                   P52, 
                    P53, 
                   P54, 
                   P55, 
                   P56, 
                   P57, 
                    P58, 
                   P59, 
                   P60, 
                   P61, 
                    P62, 
                    P63, 
                   P64, 
                   P65, 
                   P66, 
                   P67, 
                   P68, 
                   P69, 
                   P70, 
                   P71, 
                   P72, 
                   P73, 
                   P74, 
                   P75, 
                   P76, 
                   P77, 
                   P78, 
                   P79)

tx_status <- as.data.frame(tx_status)                                                      

# export to excel and transpose columns and rows
# write.csv(tx_status,"~/R/tx statuswide.csv", row.names = FALSE)   ##this becomes followup_redcap.xlsx, sheet (3)


# Create tx status data frame with excluded participants WIDE --------------------------------------------
P14 <- c(rep(1,5), rep(2,23), rep(3, 12), rep(NA,0))
P15 <- c(rep(1,2), rep(2,21), rep(3, 9), rep(NA,8))
P16 <- c(rep(1,2), rep(2,16), rep(3, 13), rep(NA,9))
P17 <- c(rep(1,3), rep(2,24), rep(3, 4), rep(NA,9))
# P18 <- c(rep(1,4), rep(2,1), rep(3, 26), rep(NA,9))
P19 <- c(rep(1,1), rep(2,10), rep(3, 17), rep(NA,12))
P20 <- c(rep(1,4), rep(2,16), rep(3, 7), rep(NA,13))
P21 <- c(rep(1,10), rep(2,22), rep(3, 6), rep(NA,2))
P22 <- c(rep(1,13), rep(2,23), rep(3, 4), rep(NA,0))
P23 <- c(rep(1,3), rep(2,4), rep(3, 31), rep(NA,2))
P24 <- c(rep(1,2), rep(2,13), rep(3, 12), rep(NA,13))
P25 <- c(rep(1,2), rep(2,7), rep(3, 31), rep(NA,0))
P26 <- c(rep(1,1), rep(2,6), rep(3, 13), rep(NA,20))
P27 <- c(rep(1,4), rep(2,14), rep(3, 14), rep(NA,8))
P28 <- c(rep(1,6), rep(2,18), rep(3, 16), rep(NA,0))
P29 <- c(rep(1,3), rep(2,14), rep(3, 16), rep(NA,7))
# P30 <- c(rep("NA", 40))
# P31 <- c(rep("NA", 40))
P32 <- c(rep(1,2), rep(2,18), rep(3, 16), rep(NA,4))
P33 <- c(rep(1,5), rep(2,7), rep(3, 1), rep(NA,27))
# P34 <- c(rep(1,6), rep(2,0), rep(3, 29), rep(NA,5))
P35 <- c(rep(1,12), rep(2,11), rep(3, 13), rep(NA,4))
P36 <- c(rep(1,4), rep(2,20), rep(3, 15), rep(NA,1))
# P37 <- c(rep(1,2), rep(2,0), rep(3, 30), rep(NA,8))
P38 <- c(rep(1,5), rep(2,15), rep(3, 15), rep(NA,5))
P39 <- c(rep(1,3), rep(2,7), rep(3, 18), rep(NA,12))
P40 <- c(rep(1,8), rep(2,11), rep(3, 12), rep(NA,9))
P41 <- c(rep(1,1), rep(2,11), rep(3, 17), rep(NA,11))
P42 <- c(rep(1,2), rep(2,12), rep(3, 12), rep(NA,14))
P43 <- c(rep(1,2), rep(2,13), rep(3, 12), rep(NA,13))
P44 <- c(rep(1,7), rep(2,10), rep(3, 15), rep(NA,8))
# P45 <- c(rep("NA", 40))
P46 <- c(rep(1,3), rep(2,6), rep(3, 22), rep(NA,9))
# P47 <- c(rep("NA", 40))
P48 <- c(rep(1,3), rep(2,6), rep(3, 19), rep(NA,12))
P49 <- c(rep(1,19), rep(2,8), rep(3, 13), rep(NA,0))
P50 <- c(rep(1,7), rep(2,6), rep(3, 14), rep(NA,13))
P51 <- c(rep(1,2), rep(2,19), rep(3, 15), rep(NA,4))
P52 <- c(rep(1,3), rep(2,26), rep(3, 6), rep(NA,5))
# P53 <- c(rep(1,4), rep(2,0), rep(3, 18), rep(NA,18))
P54 <- c(rep(1,2), rep(2,11), rep(3, 14), rep(NA,13))
P55 <- c(rep(1,1), rep(2,5), rep(3, 13), rep(NA,21))
P56 <- c(rep(1,2), rep(2,5), rep(3, 15), rep(NA,18))
P57 <- c(rep(1,1), rep(2,6), rep(3, 21), rep(NA,12))
# P58 <- c(rep(1,22), rep(2, 18))
P59 <- c(rep(1,6), rep(2,6), rep(3, 14), rep(NA,14))
P60 <- c(rep(1,8), rep(2,7), rep(3, 15), rep(NA,10))
P61 <- c(rep(1,2), rep(2,6), rep(3, 14), rep(NA,18))
# P62 <- c(rep("NA", 40))
# P63 <- c(rep("NA", 40))
P64 <- c(rep(1,2), rep(2,7), rep(3, 13), rep(NA,18))
P65 <- c(rep(1,2), rep(2,9), rep(3, 23), rep(NA,6))
P66 <- c(rep(1,1), rep(2,4), rep(3, 15), rep(NA,20))
P67 <- c(rep(1,7), rep(2,14), rep(3, 13), rep(NA,6))
P68 <- c(rep(1,2), rep(2,7), rep(3, 14), rep(NA,17))
P69 <- c(rep(1,1), rep(2,5), rep(3, 15), rep(NA,19))
P70 <- c(rep(1,8), rep(2,14), rep(3, 14), rep(NA,4))
P71 <- c(rep(1,3), rep(2,7), rep(3, 15), rep(NA,15))
P72 <- c(rep(1,6), rep(2,6), rep(3, 14), rep(NA,14))
P73 <- c(rep(1,5), rep(2,23), rep(3, 9), rep(NA,3))
P74 <- c(rep(1,16), rep(2,5), rep(3, 15), rep(NA,4))
P75 <- c(rep(1,2), rep(2,6), rep(3, 18), rep(NA,14))
P76 <- c(rep(1,18), rep(2,14), rep(3, 4), rep(NA,4))
P77 <- c(rep(1,1), rep(2,6), rep(3, 10), rep(NA,23))
P78 <- c(rep(1,2), rep(2,8), rep(3, 13), rep(NA,17))
P79 <- c(rep(1,1), rep(2,5), rep(3, 14), rep(NA,20))

tx_status_exc <- cbind(P14,
                   P15,
                   P16,
                   P17,
                   # P18,
                   P19,
                   P20,
                   P21,
                   P22,
                   P23,
                   P24,
                   P25,
                   P26,
                   P27,
                   P28,
                   P29,
                   # P30,
                   # P31,
                   P32,
                   P33,
                   # P34,
                   P35,
                   P36,
                   # P37,
                   P38,
                   P39,
                   P40,
                   P41,
                   P42,
                   P43,
                   P44,
                   # P45,
                   P46,
                   # P47,
                   P48,
                   P49,
                   P50,
                   P51,
                   P52,
                   # P53,
                   P54,
                   P55,
                   P56,
                   P57,
                   # P58,
                   P59,
                   P60,
                   P61,
                   # P62,
                   # P63,
                   P64,
                   P65,
                   P66,
                   P67,
                   P68,
                   P69,
                   P70,
                   P71,
                   P72,
                   P73,
                   P74,
                   P75,
                   P76,
                   P77,
                   P78,
                   P79)

tx_status_exc <- as.data.frame(tx_status_exc)

### export to excel and transpose columns and rows
# write.csv(tx_status_exc,"~/R/tx statuswide.csv", row.names = FALSE)  # this was converted into followup_redcap.xlsx, sheet 4

# merge tx status df with availabiltiy df - excluded participants ---------------------------------

#import treatment status df
followup_wide_exc <- read_excel("~/R/followup_redcap.xlsx",
                            sheet = 4)                      #sheet 4 has the following Ps excluded due to NA tx status (30,31,37,45,47,62,63)
fu_long_exc <- melt(data = followup_wide_exc,               # switch to long format
                id.vars = "record_id",
                variable.name = "redcap_event_name",
                value.name = "tx_status")


record_id_clean <- fu_long_exc$record_id %>%
  str_remove_all("P")                                       # 1. remove the P (creates a string called record_id)
fu_long_exc <- mutate(fu_long_exc, record_id = record_id_clean) #2. and then add back into the df


X3_REDCap <- read_excel("~/R/3. REDCap active data (LongBook1_REDCap).xlsx",  
                        sheet = 5)                          #import data availability for each week
redcap_subset <- X3_REDCap %>%
  select(record_id, redcap_event_name, perc.complete, data_present)

# View(redcap_subset %>%
#   group_by(record_id) %>%
#   count(nrow(n)))


fu_redcap_exc <- merge(redcap_subset, fu_long_exc)           #merge both datasets

fu_redcap_exc <- fu_redcap_exc %>%                           # convert Ended by to factor (categorical) with 3 levels
  mutate(tx_status = factor(as.character(tx_status),
                            levels = c(1, 2, 3),
                            labels = c("pre_tx", "tx", "post_tx")))

View(fu_redcap_exc)

# merge tx status df with availability df - ALL Participants-------------------------------

#import treatment status df
followup_wide <- read_excel("~/R/followup_redcap.xlsx",
                                sheet = 3)
fu_long <- melt(data = followup_wide,               # switch to long format
                id.vars = "record_id",
                variable.name = "redcap_event_name",
                value.name = "tx_status")

record_id_clean <- fu_long$record_id %>%
  str_remove_all("P")                                   # 1. remove the P (creates a string called record_id)
fu_long <- mutate(fu_long, record_id = record_id_clean) #2. and then add back into the df


X3_REDCap <- read_excel("~/R/3. REDCap active data (LongBook1_REDCap).xlsx",  
                        sheet = 5)                  #import data availability for each week
redcap_subset <- X3_REDCap %>%
  select(record_id, redcap_event_name, perc.complete, data_present)

#merge both datasets
fu_redcap <- merge(redcap_subset, fu_long)

fu_redcap <- fu_redcap %>%                           # convert Ended by to factor (categorical) with 3 levels
  mutate(tx_status = factor(as.character(tx_status),
                            levels = c(1, 2, 3),
                            labels = c("pre_tx", "tx", "post_tx")))

View(fu_redcap)
# select availability by Tx status (ALL Ps) ----------------------------------------

redcap_pretx <- fu_redcap %>%
  filter(tx_status == "pre_tx")

redcap_tx <- fu_redcap %>%
  filter(tx_status == "tx")

redcap_posttx <- fu_redcap %>%
  filter(tx_status == "post_tx")


prop_pretx <- redcap_pretx %>%
  group_by(redcap_event_name) %>%               #for each week
  mutate(week_avail = sum(data_present)) %>%    #calculate how many participants have data available
  mutate(total_week = n())  %>%                 #out of the number of people that week
  mutate(prop.available = (week_avail/total_week)*100) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

prop_tx <- redcap_tx %>%
  group_by(redcap_event_name) %>%               #for each week
  mutate(week_avail = sum(data_present)) %>%    #calculate how many participants have data available
  mutate(total_week = n())  %>%                 #out of the number of people that week
  mutate(prop.available = (week_avail/total_week)*100) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

prop_posttx <- redcap_posttx %>%
  group_by(redcap_event_name) %>%               #for each week
  mutate(week_avail = sum(data_present)) %>%    #calculate how many participants have data available
  mutate(total_week = n())  %>%                 #out of the number of people that week
  mutate(prop.available = (week_avail/total_week)*100) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

# write.csv(prop_pretx,"~/R/prop_pre.csv", row.names = FALSE)
# write.csv(prop_tx,"~/R/prop_tx.csv", row.names = FALSE)
# write.csv(prop_posttx,"~/R/prop_post.csv", row.names = FALSE)


# redcap availability by study end status  --------------------------------
subset_studyend <- X2_Demographic_Table %>%
  select(record_ID, `Ended by`)
names(subset_studyend)[names(subset_studyend) == "record_ID"] <- 'record_id'     # rename
names(subset_studyend)[names(subset_studyend) == "Ended by"] <- 'ended_by'       # rename

subset_studyend <- subset_studyend %>%                                   # convert Ended by to factor (categorical) with 3 levels
  mutate(ended_by = factor(as.character(ended_by),
                             levels = c("lost to follow up", "withdrawal", "completed"),
                             labels = c("lost to follow up", "withdrawal", "completed")))

#merge subsetted study end df with follow up data availability
fu_studyend <- merge(redcap_subset, subset_studyend)


# plots -------------------------------------------------------------------

#plot a mean line of data availability of average percentage complete by study end
#x axis is redcap_event_name, y is percentage/mean
by_studyend_week <- fu_studyend %>%
  group_by(redcap_event_name, ended_by) 
# %>%
# summarize(meancompletion = mean(perc.complete))

ggplot(by_studyend_week, aes(x=redcap_event_name, y=perc.complete, color=ended_by)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)



#plot with participant exclusions - N participants with available data by tx status
by_tx_week_exc <- fu_redcap_exc %>%
  group_by(redcap_event_name, tx_status) %>%
  drop_na() %>%
  mutate(week_avail = sum(data_present)) %>%    #calculate how many participants have data available
  mutate(total_week = n())  %>%                 #out of the number of people that week
  mutate(prop.available = (week_avail/total_week)*100) %>%
  mutate(is_outlier = total_week < 6) %>%       #to exclude weeks with < 5 people (1)
  filter(is_outlier == FALSE) %>%               #(2)
  summarise_if(is.numeric, mean, na.rm = TRUE)

ggplot(by_tx_week_exc, aes(x=redcap_event_name, y=prop.available, color=tx_status)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)




 
