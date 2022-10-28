library(lmerTest)
library(data.table)
library(corrplot)
library(glmnet)
library(caret)
library(groupdata2)
library(readxl)
library(multcomp)
library(agricolae)
library(tidyverse)
library(lubridate)

library(pacman)


select <- dplyr::select

options(scipen=999)
options(scipen=0) # to revert scientific notation


getwd()
#load from IoP:
setwd("C:/Users/k1754359/OneDrive - King's College London/PhD/6. Correlation Digital signals in Depression/R scripts") 



#load from home:
setwd("C:/Users/valer/Downloads")


qids <- fread("QIDS_summarized_updated.csv", data.table=F) %>%
  dplyr::mutate(survey_date = lubridate::ymd(survey_date))


sleep <- fread("daily_sleep_feature.csv", data.table=F) %>%
  dplyr::mutate(date_str = lubridate::ymd(date_str))

bt <- fread("daily_bt_feature.csv", data.table=F) %>%
  dplyr::mutate(date_str = lubridate::ymd(date_str))

gps <- fread("daily_GPS_feature.csv", data.table=F) %>%
  dplyr::mutate(date_str = lubridate::ymd(date_str))



win_size <- 7

# sleep -----

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




qids_temp <- qids %>%
  filter(sleep_day >=3) # if there are more than 2 days. filtering for at least 2 days of sleep per week

qids_prmt <- qids

###### V. important exclusion
qids_prmt <- qids_prmt %>% 
  filter(p_id != "f5978923-cef2-4eeb-a49c-7d79be4b53eb", p_id != "5786af5e-99e3-4f78-848a-d3b67b8eb7ed")


### FITBIT FEATURES ----
# Extract HR, Steps, and Activity data 

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


# extract second-order features for each qids record
# read qids table
qids <- fread("QIDS_summarized_updated.csv", data.table=F) %>%
  dplyr::mutate(survey_date = lubridate::ymd(survey_date))

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



qids_temp <- qids %>%
  filter(hr_day >=3)

fit <-lmer(qids_total ~ FEATURE_005_std_step + (1 |p_id), qids)
summary(fit)


qids_fb <- qids

###### V. important exclusion
qids_fb <- qids_fb %>% 
  filter(p_id != "f5978923-cef2-4eeb-a49c-7d79be4b53eb", p_id != "5786af5e-99e3-4f78-848a-d3b67b8eb7ed")


#### combine both second_features! -----
qids_passive <- merge(qids_prmt, qids_fb)


names(qids_prmt)

### ### 
#  REMEMBER TO EXCLUDE:     ####
# f5978923-cef2-4eeb-a49c-7d79be4b53eb
# 5786af5e-99e3-4f78-848a-d3b67b8eb7ed
### ###


###### V. important exclusion
qids_passive <- qids_passive %>% 
  filter(p_id != "f5978923-cef2-4eeb-a49c-7d79be4b53eb", p_id != "5786af5e-99e3-4f78-848a-d3b67b8eb7ed")

## can read qids_passive.csv

##### exclude variables with MISSING DATA -----
# missing data <= 50%       =  impute
# missing data > 50%       =  remove variable

#show how many NAs per variable and make a list of them named "a"
a <- as.data.frame((sapply(qids_passive, function(x) sum(is.na(x))))/nrow(qids_passive))
a <- as.data.frame(which(a > .5, arr.ind = TRUE))
low_data <- a$row

### Multilevel Regression models -----


 #  ### ### ### ###  ###
### LOAD DESCRIPTIVES.R ####
 #  ### ### ### ###  ###


#combine the digital+qids features to demographic variables
demo <-demographics[c(1, 14,23,24,25,26)]      # demographic variables from script = descriptives.R !!!
# variables with > 50% available data
qids_avail <- qids_passive[,-c(low_data)]

d <-merge(qids_avail, demo)
names(d)
# for (i in 22:147)

data = d         #     <- - - - select the data to be used for the models

## conduct the models adjusting for Age, gender.
for (i in 22:147) {               # change depending on data used
  fit <-lmer(q2               
             ~ data[[i]]          
             + Age + gender  
             + (1 |p_id),data = data) 
  print(toupper(names(data)[i]))
  # print(summary(fit))
  print(round(coef(summary(fit)), digits = 6))
  # print(round(confint(fit), digits = 6))
}

fit <-lmer(qids_total ~ deep_pct_std +  Age + gender + (1 |p_id), data = data)
summary(fit)
confint(fit)


18.470/(18.470 + 7.434) # ~ 70% is the variance explained by individual difference
fit@sigma


 ### ### ### ###  ###
##  QIDS by items ####
 ### ### ### ###  ###


## to save output to an excel/csv doc ####

library(lme4)

FEATURE <- c()
Estimate <- c()
StdError <- c()
df <- c()
t_value <- c()
pr_t <- c()
two_five<- c()
nine_five <- c()
pacman::p_load(progress)
## Basic
pb <- progress_bar$new(total=100)
for (i in 23:26) {
  pb$tick()
  fit <-lmer(q1
             ~ data[[i]]
             + Age + gender
             + (1 |p_id),data = data)
  FEATURE <- append(Q1,toupper(names(data)[i]))
  Estimate <- append(Estimate,round(coef(summary(fit)), digits = 6)[2,1])
  StdError <- append(StdError,round(coef(summary(fit)), digits = 6)[2,2])
  df <- append(df,round(coef(summary(fit)), digits = 6)[2,3])
  t_value <- append(t_value,round(coef(summary(fit)), digits = 6)[2,4])
  pr_t <- append(pr_t,round(coef(summary(fit)), digits = 6)[2,5])
  two_five <- append(two_five,round(confint(fit), digits = 6)[4,1])
  nine_five <- append(nine_five,round(confint(fit), digits = 6)[4,2])
  Sys.sleep(0.1)
  
}

data_results= data.frame(FEATURE,Estimate,StdError,df,t_value,pr_t,two_five,nine_five)
colnames(data_results) <- c("FEATURE","Estimate","Std. eror","df","t value","Pr(>|t|)","2.5 CI","97.5 CI")
write.csv(data_results,"model output.csv",row.names = FALSE)



### group QIDS items ####


d <- d %>%
  mutate(sleep = pmax(q1, q2, q3, q4)) %>%
  mutate(weight = pmax(q6, q7, q8, q9)) %>%
  mutate(psychomotor = pmax(q15, q16)) %>%
  rename(dep_mood = q5) %>%
  rename(conc = q10) %>%
  rename(guilt = q11) %>%
  rename(suicide = q12) %>%
  rename(interest = q13) %>%
  rename(fatigue = q14) 

d = data

for (i in 22:40) {               # change depending on data used
  fit <-lmer(weight               
             ~ data[[i]]          
             + Age + gender  
             + (1 |p_id),data = data) 
  print(toupper(names(data)[i]))
  # print(summary(fit))
  print(round(coef(summary(fit)), digits = 6))
  # print(round(confint(fit), digits = 6))
}

summary(fit)$coefficients[2,5] 

#PLOTS ----

plot(fit)  # looks alright, no patterns evident
qqnorm(resid(fit))
qqline(resid(fit))  # points fall nicely onto the line - good!


(split_plot <- ggplot(aes(sleep_onset_std, qids_total), data = data) + 
    geom_point() + 
    facet_wrap(~ p_id) + # create a facet for each mountain range
    xlab("feature") + 
    ylab("QIDS"))

(prelim_plot <- ggplot(qids_temp, aes(x = sleep_onset_mean, y = qids_total)) +
    geom_point())
