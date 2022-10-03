#GAD 7 analysis

#load datasets 
getwd()
setwd("C:/Users/k1754359/OneDrive - King's College London/PhD/6. Correlation Digital signals in Depression/R scripts")

redcap_full <- read_excel("REDcap full.xlsx")
IDmap <- read_excel("4. aRMT data.xlsx", sheet = "IDmap")

names(redcap_full)

# GAD cleaning-----
#create a function to fix the incorrect scoring of scales.
rescore <- function(x, na.rm = FALSE) (x - 1)
gad_select <- redcap_full %>%
  dplyr::select(c(1, 2, contains("gad7"))) %>%
  filter(record_id > 13) %>%
  filter(str_detect(redcap_event_name, "week|enrolment")) %>% 
  #gad was coded incorrectly from 1 - 4 instead of 0 -3, so need to fix  
  mutate(across(gad7_1:gad7_8, rescore)) %>%
  mutate(total_gad = rowSums(across(gad7_1:gad7_7), na.rm = T)) %>%
  filter(!is.na(gad7_timestamp)) %>%    #select only one row per enrolment event
  # dichotomise gad7 based on clinical cut-off
  mutate(gad_binary = ifelse(total_gad >9, 1, 0))
#change redcap_event_name with week number
redcap_event <- gad_select$redcap_event_name  %>%
  str_remove_all("_arm_1") %>%
  str_remove_all("week_") %>%
  str_replace("enrolment", "0")
gad_select$redcap_event_name <- as.numeric(redcap_event)
gad_select %>% rename(event_name = redcap_event_name)
gad_select$survey_date <- as.Date(gad_select$gad7_timestamp)



#add p_id
gad <- merge(gad_select, IDmap[, c("participant_id", "record_id")]) %>%
  rename(p_id = participant_id)

# ADD features to gad (following Yuezhou's code from QIDS)
qids<-gad


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



### ### 


gad_prmt <- qids


## now run second_order_feature_extraction as QIDS, then come back -----
qids <- gad

setwd("C:/Users/k1754359/OneDrive - King's College London/PhD/6. Correlation Digital signals in Depression/data stream files")

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

win_size = 7
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

# back to gad ----
gad_fb <- qids

glimpse(gad_fb)


gad_passive <- merge(gad_fb, gad_prmt)


###### V. important exclusion
gad_passive <- gad_passive %>% 
  filter(p_id != "f5978923-cef2-4eeb-a49c-7d79be4b53eb", p_id != "5786af5e-99e3-4f78-848a-d3b67b8eb7ed")


##### exclude variables with MISSING DATA -----

#show how many NAs per variable and make a list of them named "a"
a <- as.data.frame((sapply(gad_passive, function(x) sum(is.na(x))))/nrow(gad_passive))
a <- as.data.frame(which(a > .5, arr.ind = TRUE))
low_data <- a$row

names(gad_passive)  #show me the names of all variables

other_vars <- c(1:17, 52, 81, 120, 141, 152)   # For all gad passive

length(gad_passive)    # 158
length(other_vars)     # 22
length(low_data)       # 16
158-22-16              # 120 predictor variables left 


inc_vars <- gad_passive[,c(-c(low_data), -c(other_vars))]
# inc_vars <- gad_passive[, -c(other_vars)]  # For Fitbit -  HR STEPS and ACTIVITY df


### Multilevel Regression models -----

# variables with > 50% available data
gad_avail <- gad_passive[,c(-c(low_data))]

#add QIDS scores as a covariate
gad_avail<-gad_avail %>% rename(event_name = redcap_event_name)

gad_avail <- merge(gad_avail, qids_all[, c(1, 21, 165)], all.x = TRUE) %>%
  drop_na(event_name) %>%
  drop_na(total_gad)

#  ### ### ### ###  ###
### LOAD DESCRIPTIVES.R ####
#  ### ### ### ###  ###


#combine the digital+gad_passive features to demographic variables
names(demographics)
names(demographics[c(1, 14,23,24,25,26)])      # demographic variables from script = descriptives.R !!!
d <-merge(gad_avail, demographics[c(1, 14,23,24,25,26)])
glimpse(gad_passive)

data = d         #     <- - - - select the data to be used for the models
names(data)



## conduct the models adjusting for: Age, gender + depression
#loop for mixed linear models

for (i in 79:140) {    # change depending on data used
  fit <-lmer(total_gad                # predicted variable 
             ~ data[[i]]          
             + Age + gender + qids_total   #covariates
             + (1 |p_id),data = data)  # random effects
  print(toupper(names(data)[i]))
  # print(summary(fit))
  # print(round(coef(summary(fit)), digits = 6))
  print(round(confint(fit), digits = 6))
}





fit <-lmer(total_gad ~ deep_pct_mean +  Age + gender + (1 |p_id), data = data)
summary(fit)
confint(fit)


18.470/(18.470 + 7.434) # ~ 70% is the variance explained by individual difference
fit@sigma

### gad for other FITBIT DATA #####

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

# read gad table
# gad_select already loaded

#add p_id
gad <- merge(gad_select, IDmap[, c("participant_id", "record_id")]) %>%
  rename(p_id = participant_id)

###change from gad to qids to run code
qids<-gad


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
gad <- qids


gad_temp <- gad %>%
  filter(hr_day >=4)

fit <-lmer(total_gad ~ FEATURE_005_std_step + (1 |p_id), gad)
summary(fit)


###### V. important exclusion
gad <- gad %>% 
  filter(p_id != "f5978923-cef2-4eeb-a49c-7d79be4b53eb", p_id != "5786af5e-99e3-4f78-848a-d3b67b8eb7ed")

gad_temp <- gad_temp %>% 
  filter(p_id != "f5978923-cef2-4eeb-a49c-7d79be4b53eb", p_id != "5786af5e-99e3-4f78-848a-d3b67b8eb7ed")

