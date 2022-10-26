##PHQ 9 analysis
library(agricolae)
library(lmerTest)
library(data.table)
library(corrplot)
library(glmnet)
library(caret)
library(groupdata2)
library(readxl)
library(multcomp)
library(tidyverse)
library(lubridate)
library(naniar)

select <- dplyr::select


#load datasets 
getwd()
setwd("C:/Users/k1754359/OneDrive - King's College London/PhD/6. Correlation Digital signals in Depression/R scripts")

# setwd("C:/Users/valer/Downloads")


redcap_full <- read_excel("REDcap full.xlsx")
IDmap <- read_excel("4. aRMT data.xlsx", sheet = "IDmap")


options(scipen=999)
# options(scipen=0) # to revert scientific notation


sleep <- fread("daily_sleep_feature.csv", data.table=F) %>%
  dplyr::mutate(date_str = lubridate::ymd(date_str))

bt <- fread("daily_bt_feature.csv", data.table=F) %>%
  dplyr::mutate(date_str = lubridate::ymd(date_str))

gps <- fread("daily_GPS_feature.csv", data.table=F) %>%
  dplyr::mutate(date_str = lubridate::ymd(date_str))



## Ends with phq_select <- outcome

# PHQ9 cleaning-----
#create a function to fix the incorrect scoring of scales.
rescore <- function(x, na.rm = FALSE) (x - 1)
phq_select <- redcap_full %>%
  dplyr::select(c(1, 2, contains("phq9"))) %>%
  filter(record_id > 13) %>%
  filter(str_detect(redcap_event_name, "week|enrolment")) %>% 
  #PHQ was coded incorrectly from 1 - 4 instead of 0 -3, so need to fix  
  mutate(across(phq9_1:phq9_10, rescore)) %>%
  mutate(total_phq = rowSums(across(phq9_1:phq9_9), na.rm = T)) %>%
  filter(!is.na(phq9_timestamp)) %>%    #select only one row per enrolment event
  # dichotomise phq9 based on clinical cut-off
  rename(redcap_event = redcap_event_name) %>% 
  mutate(phq_binary = ifelse(total_phq >9, 1, 0))
#change redcap_event with week number
redcap_event <- phq_select$redcap_event  %>%
  str_remove_all("_arm_1") %>%
  str_remove_all("week_") %>%
  str_replace("enrolment", "0")
phq_select$redcap_event <- as.numeric(redcap_event)
phq_select$survey_date <- as.Date(phq_select$phq9_timestamp)

#add p_id
phq <- merge(phq_select, IDmap[, c("participant_id", "record_id")]) %>%
  rename(p_id = participant_id)

# ADD features to PHQ (following Yuezhou's code from QIDS)
qids<-phq




win_size <- 7

# Sleep -----

# extract second-order features
sleep_feature <- colnames(sleep)[6:15]
second_feature <- c("_mean", "_std")
new_feature =c()
for (i in 1:length(sleep_feature)){
  for (j in 1:length(second_feature)){
    new_feature = append(new_feature,paste(sleep_feature[i],second_feature[j],sep = ""))
  }
}
pre_col_num <- ncol(qids)
qids$sleep_day <- NA
qids[, new_feature] <-NA

for (i in 1:nrow(qids)){
  print(i)
  p_id_select <- qids$p_id[i]
  date_select <- qids$survey_date[i]
  sleep_temp <- sleep[sleep$p_id==p_id_select,]
  sleep_temp <- sleep_temp[sleep_temp$date_str >= date_select - days(win_size) & sleep_temp$date_str < date_select,]
  sleep_temp <- na.omit(sleep_temp)
  qids$sleep_day[i] <- nrow(sleep_temp)
  if (nrow(sleep_temp) >= 2){
    for (j in 1:length(sleep_feature)){
      # all days
      qids[i, (j-1) *length(second_feature) + 1 + pre_col_num + 1] = mean(sleep_temp[,j+5], na.rm = TRUE)
      qids[i, (j-1) *length(second_feature) + 2 + pre_col_num + 1] = sd(sleep_temp[,j+5], na.rm = TRUE)
    }
  }
}
# BT features ----

bt_feature <- colnames(bt)[7:11]
new_feature =c()

for (i in 1:length(bt_feature)){
  for (j in 1:length(second_feature)){
    new_feature = append(new_feature,paste(bt_feature[i], second_feature[j],sep = ""))
  }
}

pre_col_num <- ncol(qids)
qids$bt_day <- NA
qids[, new_feature] <-NA

for (i in 1:nrow(qids)){
  print(i)
  p_id_select <- qids$p_id[i]
  date_select <- qids$survey_date[i]
  bt_temp <- bt[bt$p_id==p_id_select,]
  bt_temp <- bt_temp[bt_temp$date_str >= date_select - days(win_size) & bt_temp$date_str < date_select,]
  bt_temp <- bt_temp[!is.na(bt_temp$bt_available_day), ]
  qids$bt_day[i] <- sum(bt_temp$bt_available_day >=12)   #change here for missing data threshold
  bt_temp <- bt_temp[bt_temp$bt_available_day >=12,]     #change here for missing data threshold
  
  if (nrow(bt_temp)>=2){
    for (j in 1:length(bt_feature)){
      # all day
      qids[i, (j-1) *length(second_feature) + 1 + pre_col_num + 1] = mean(bt_temp[,j+6], na.rm = TRUE)
      qids[i, (j-1) *length(second_feature) + 2 + pre_col_num + 1] = sd(bt_temp[,j+6], na.rm = TRUE)
      
    }
    
  }
}

# GPS ----

gps_feature <- colnames(gps)[7:9]
new_feature =c()
for (i in 1:length(gps_feature)){
  for (j in 1:length(second_feature)){
    new_feature = append(new_feature,paste(gps_feature[i], second_feature[j],sep = ""))
  }
}
pre_col_num <- ncol(qids)
qids$gps_day <- NA
qids[, new_feature] <-NA

for (i in 1:nrow(qids)){
  print(i)
  p_id_select <- qids$p_id[i]
  date_select <- qids$survey_date[i]
  gps_temp <- gps[gps$p_id==p_id_select,]
  gps_temp <- gps_temp[gps_temp$date_str >= date_select - days(win_size) & gps_temp$date_str < date_select,]
  gps_temp <- gps_temp[!is.na(gps_temp$gps_available), ]
  
  qids$gps_day[i] <- sum(gps_temp$gps_available >=12)
  gps_temp <- gps_temp[gps_temp$gps_available >=12,]
  
  if (nrow(gps_temp)>=1){
    for (j in 1:length(gps_feature)){
      # all day
      qids[i, (j-1) *length(second_feature) + 1 + pre_col_num + 1] = mean(gps_temp[,j+6], na.rm = TRUE)
      qids[i, (j-1) *length(second_feature) + 2 + pre_col_num + 1] = sd(gps_temp[,j+6], na.rm = TRUE)
    }
  }
}


## now come back -----
phq_prmt<-qids



### ### 
##  REMEMBER TO EXCLUDE:     ####
# f5978923-cef2-4eeb-a49c-7d79be4b53eb
# 5786af5e-99e3-4f78-848a-d3b67b8eb7ed
### ###


###### V. important exclusion
phq_prmt <- phq_prmt %>% 
  filter(p_id != "f5978923-cef2-4eeb-a49c-7d79be4b53eb", p_id != "5786af5e-99e3-4f78-848a-d3b67b8eb7ed")


phq_temp <- phq_prmt %>%
  filter(sleep_day >=2) # if there are more than 2 days. filtering for at least 2 days of sleep per week



# PHQ for other FITBIT DATA #####

# Date ID
Date_ID_set <- read_excel("HR_210322.xlsx", sheet = "Time Interval ID") %>%
  rename(DATE_ID = timeInterval_ID) %>%
  rename(date_str = datetimeStart)
# read HR feature table
HR_set <- read_excel("HR_210322.xlsx", sheet = "HR") %>%
  dplyr::select(SUBJECT_ID, DATE_ID, MISSING_RATE, FEATURE_001, FEATURE_002, FEATURE_003, FEATURE_004, FEATURE_005
                , FEATURE_006, FEATURE_007, FEATURE_008, FEATURE_009, FEATURE_010, FEATURE_015, FEATURE_016, FEATURE_017
                , FEATURE_018, FEATURE_019, FEATURE_020)
# map dateid
HR_set <- merge(HR_set,Date_ID_set) %>%
  dplyr::select(-DATE_ID) %>%
  dplyr::mutate(date_str = lubridate::ymd(date_str))%>%
  rename(p_id=SUBJECT_ID)

# read Step feature table
Step_set <- read_excel("STEPS_210322.xlsx", sheet = "Hoja1") %>%
  dplyr::select(SUBJECT_ID, DATE_ID, MISSING_RATE, FEATURE_001, FEATURE_002, FEATURE_003, FEATURE_004, FEATURE_005
                , FEATURE_006, FEATURE_007, FEATURE_019, FEATURE_020, FEATURE_022
                , FEATURE_024, FEATURE_025, FEATURE_026)
# map date id
Step_set <- merge(Step_set,Date_ID_set) %>%
  dplyr::select(-DATE_ID) %>%
  dplyr::mutate(date_str = lubridate::ymd(date_str))%>%
  rename(p_id=SUBJECT_ID)

# read Activity feature table
Activity <- read_excel("ACTIVITY_210322.xlsx", sheet = "Hoja1") %>%
  dplyr::select(SUBJECT_ID, DATE_ID, MISSING_RATE,FEATURE_001,FEATURE_002,FEATURE_003,FEATURE_004,FEATURE_005
                ,FEATURE_006,FEATURE_007,FEATURE_008,FEATURE_009,FEATURE_010,FEATURE_011,FEATURE_012,FEATURE_013
                ,FEATURE_014,FEATURE_015,FEATURE_016,FEATURE_017,FEATURE_018)
# map date id
Activity <- merge(Activity, Date_ID_set) %>%
  dplyr::select(-DATE_ID) %>%
  dplyr::mutate(date_str = lubridate::ymd(date_str))%>%
  rename(p_id=SUBJECT_ID)



# extract second-order features for each record

###change from phq to qids to run code
qids<-phq


# HR
# extract second-order hr features
hr_feature <- colnames(HR_set)[2:18]
second_feature <- c("_mean_hr", "_std_hr")
new_feature =c()
for (i in 1:length(hr_feature)){
  for (j in 1:length(second_feature)){
    new_feature = append(new_feature,paste(hr_feature[i],second_feature[j],sep = ""))
  }
}
pre_col_num <- ncol(qids)
qids$hr_day <- NA
qids[, new_feature] <-NA

for (i in 1:nrow(qids)){
  print(i)
  p_id_select <- qids$p_id[i]
  date_select <- qids$survey_date[i]
  hr_temp <- HR_set[HR_set$p_id==p_id_select,]
  hr_temp <- hr_temp[hr_temp$date_str >= date_select - days(win_size) & hr_temp$date_str < date_select,]
  # threshold for daily hr missing rate
  hr_temp <- hr_temp[hr_temp$MISSING_RATE < 50,]
  
  qids$hr_day[i] <- nrow(hr_temp)
  if (nrow(hr_temp) >= 2){
    for (j in 1:length(hr_feature)){
      # all days
      qids[i, (j-1) *length(second_feature) + 1 + pre_col_num + 1] = mean(as.numeric(hr_temp[,j+1]), na.rm = TRUE)
      qids[i, (j-1) *length(second_feature) + 2 + pre_col_num + 1] = sd(as.numeric(hr_temp[,j+1]), na.rm = TRUE)
    }
  }
}

# step features
step_feature <- colnames(Step_set)[2:15]
second_feature <- c("_mean_step", "_std_step")
new_feature =c()
for (i in 1:length(step_feature)){
  for (j in 1:length(second_feature)){
    new_feature = append(new_feature,paste(step_feature[i],second_feature[j],sep = ""))
  }
}
pre_col_num <- ncol(qids)
qids$step_day <- NA
qids[, new_feature] <-NA

for (i in 1:nrow(qids)){
  print(i)
  p_id_select <- qids$p_id[i]
  date_select <- qids$survey_date[i]
  step_temp <- Step_set[Step_set$p_id==p_id_select,]
  step_temp <- step_temp[step_temp$date_str >= date_select - days(win_size) & step_temp$date_str < date_select,]
  # threshold for daily hr missing rate
  step_temp <- step_temp[step_temp$MISSING_RATE < 50,]
  
  qids$step_day[i] <- nrow(step_temp)
  if (nrow(step_temp) >= 2){
    for (j in 1:length(step_feature)){
      # all days
      qids[i, (j-1) *length(second_feature) + 1 + pre_col_num + 1] = mean(as.numeric(step_temp[,j+1]), na.rm = TRUE)
      qids[i, (j-1) *length(second_feature) + 2 + pre_col_num + 1] = sd(as.numeric(step_temp[,j+1]), na.rm = TRUE)
    }
  }
}


# activity features
activity_feature <- colnames(Activity)[2:20]
second_feature <- c("_mean_Activity", "_std_Activity")
new_feature =c()
for (i in 1:length(activity_feature)){
  for (j in 1:length(second_feature)){
    new_feature = append(new_feature,paste(activity_feature[i],second_feature[j],sep = ""))
  }
}
pre_col_num <- ncol(qids)
qids$activity_day <- NA
qids[, new_feature] <-NA

for (i in 1:nrow(qids)){
  print(i)
  p_id_select <- qids$p_id[i]
  date_select <- qids$survey_date[i]
  activity_temp <- Activity[Activity$p_id==p_id_select,]
  activity_temp <- activity_temp[activity_temp$date_str >= date_select - days(win_size) & activity_temp$date_str < date_select,]
  # threshold for daily hr missing rate
  activity_temp <- activity_temp[activity_temp$MISSING_RATE < 50,]
  
  qids$activity_day[i] <- nrow(activity_temp)
  if (nrow(activity_temp) >= 2){
    for (j in 1:length(activity_feature)){
      # all days
      qids[i, (j-1) *length(second_feature) + 1 + pre_col_num + 1] = mean(as.numeric(activity_temp[,j+1]), na.rm = TRUE)
      qids[i, (j-1) *length(second_feature) + 2 + pre_col_num + 1] = sd(as.numeric(activity_temp[,j+1]), na.rm = TRUE)
    }
  }
}



### change back ---
phq_fitbit <- qids


phq_temp <- phq_fitbit %>%
  filter(hr_day >=1)

fit <-lmer(total_phq ~ FEATURE_005_std_step + (1 |p_id), phq_fitbit)
summary(fit)


###### V. important exclusion
phq_fitbit <- phq_fitbit %>% 
  filter(p_id != "f5978923-cef2-4eeb-a49c-7d79be4b53eb", p_id != "5786af5e-99e3-4f78-848a-d3b67b8eb7ed")

phq_temp <- phq_temp %>% 
  filter(p_id != "f5978923-cef2-4eeb-a49c-7d79be4b53eb", p_id != "5786af5e-99e3-4f78-848a-d3b67b8eb7ed")


# combine both second_features! -----
phq_passive <- merge(phq_prmt, phq_fitbit)


## exclude variables with MISSING DATA -----

# missing data <= 50%       =  impute
# missing data > 50%       =  remove variable


#show how many NAs per variable and make a list of them named "a"
a <- as.data.frame((sapply(phq_passive, function(x) sum(is.na(x))))/nrow(phq_passive))
a <- as.data.frame(which(a > .5, arr.ind = TRUE))
low_data <- a$row
glimpse(phq_passive)


names(phq_passive)  #show me the names of all variables

other_vars <- c(3, 19, 
                22:23,  #time in bed
                40, 51, 58:60, 
                65:70, 81:86, 89:90, #HR
                93:95, 
                98:103, 112:113, 116:121, #steps
                122:124,
                133:148, 155:158) #activity   

length(phq_passive)    # 160
length(other_vars)     # 63
length(low_data)       # 16
160-63-16              # 82



phq_avail <- phq_passive[,c(-c(low_data), -c(other_vars))]

inc_vars <- phq_avail[, -c(1:17)]  # for correlation plot

names(inc_vars)

## Create baseline phq  -----
#create baseline df with only first 3 weeks
baseline_phq <-  phq_avail[phq_avail$redcap_event < 4, ] %>% 
  drop_na(p_id)

write.csv(baseline_phq, "C:/Users/k1754359/Downloads/baseline_phq.csv")

baseline_phq %>% 
  count(redcap_event, sort = TRUE)

baseline_phq <- baseline_phq %>% 
  group_by(record_id) %>% 
  summarise_all(mean, na.rm = TRUE)

test <- baseline_phq %>% replace_with_na_all(condition = ~.x == NaN)


# Multilevel Regression models -----


#  ### ### ### ###  ###
### LOAD DESCRIPTIVES.R ####
#  ### ### ### ###  ###

names(demographics)
names(demographics[c(1,4:7, 14,23,24,25,26)])      # demographic variables from script = descriptives.R !!!

d <-merge(phq_avail, demographics[c(1,4:7, 14,23,24,25,26)])

# create a variable that tells you if that week is baseline data
# diff between current date and treatment start date

phq_tx <- d %>% 
  #create baseline tx variable
  mutate(baselinediff = as.double(difftime(survey_date, tx_start, units = "days"))) %>%
  drop_na(tx_start) %>%
  mutate(baseline = ifelse(baselinediff > -15 & baselinediff < 8, 1, 0)) %>%
  
  #create endpoint phq
  mutate(enddiff = as.double(difftime(survey_date, tx_end, units = "days"))) %>%
  drop_na(tx_end) %>%
  mutate(end = ifelse(tx_length > 2 & enddiff > -8 & enddiff < 15, 1, 0)) %>%
  
  #create midpoint for tx
  mutate(half_tx = (as.numeric(tx_length)/2)*7) %>%
  mutate(mid = ifelse(tx_length > 3 & baselinediff > (half_tx-7) & baselinediff < (half_tx + 7), 1, 0))
  
  
View(phq_tx[c("survey_date", "tx_start", "baseline", "tx_end", "end","baselinediff", "half_tx", "mid")])

  
#baseline df
baseline <- d %>% 
  mutate(baselinediff = as.double(difftime(survey_date, tx_start, units = "days"))) %>%
  drop_na(tx_start) %>%
  filter(baselinediff > -8 & baselinediff < 15)


#combine the digital+phq features to demographic variables


# Mixed level models -----



### ### ### ###  ###
##  phq by items ####
### ### ### ###  ###


### rename PHQ items ####

item_phq <- left_join(demo_total, inc_vars, by = "p_id") %>%
  mutate(sleep = pmax(q1, q2, q3, q4)) %>%
  mutate(weight = pmax(q6, q7, q8, q9)) %>%
  mutate(psychomotor = pmax(q15, q16)) %>%
  rename(dep_mood = q5) %>%
  rename(conc = q10) %>%
  rename(guilt = q11) %>%
  rename(suicide = q12) %>%
  rename(interest = q13) %>%
  rename(fatigue = q14) %>%
  dplyr::select(-total_phq.y) %>%
  rename(total_phq = total_phq.x)




#PLOTS ----

plot(fit)  # looks alright, no patterns evident
qqnorm(resid(fit))
qqline(resid(fit))  # points fall nicely onto the line - good!


(split_plot <- ggplot(aes(sleep_onset_std, total_phq), data = data) + 
    geom_point() + 
    facet_wrap(~ p_id) + # create a facet for each mountain range
    xlab("feature") + 
    ylab("phq"))

(prelim_plot <- ggplot(phq_temp, aes(x = sleep_onset_mean, y = total_phq)) +
    geom_point())
