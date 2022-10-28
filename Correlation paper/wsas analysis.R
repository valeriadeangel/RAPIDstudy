###  WSAS

# first run the second order feature extraction.R 

wsas <- fread("master_wsas.csv", data.table=F) %>%
  dplyr::mutate(date = lubridate::ymd(date)) %>% 
  filter(p_id != "f5978923-cef2-4eeb-a49c-7d79be4b53eb", 
         p_id != "5786af5e-99e3-4f78-848a-d3b67b8eb7ed") %>%
  dplyr::mutate(total_wsas = rowSums(across("work":"relationhips"), na.rm = T))

IDmap <- read_excel("4. aRMT data.xlsx", sheet = "IDmap")
glimpse(wsas)



#add p_id
wsasid <- merge(wsas, IDmap[,2:3], by.x=c("p_id"), by.y=c("participant_id"), all = TRUE, incomparables = T)
#create an event_name variable
wsas <- wsasid %>% 
  mutate(event_name = as.double(difftime(date,date_assessment, units = "weeks")))
wsas$event_name <- round(wsas$event_name, digits = 0)

# mege WSAS with qids
a <- merge(qids_passive, IDmap[,2:3], by.x=c("p_id"), by.y=c("participant_id"), all = TRUE, incomparables = T)
qids_passive <- a %>% 
  mutate(event_name = as.double(difftime(survey_date,date_assessment, units = "weeks")))
qids_passive$event_name <- round(qids_passive$event_name, digits = 0)

names(qids_passive)[c(1, 21, 165)]
wsas_dep <- merge(wsas, qids_passive[, c(1, 21, 165)]) %>%
  drop_na(event_name) %>%
  rename(survey_date = date)
names(wsas_dep)


##now add the features

qids <-wsas_dep
?hist


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


wsas_prmt <- qids


# FITBIT features ----
###change from phq to qids to run code
qids <- wsas_dep


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
wsas_fb <- qids


wsas_passive <- merge(wsas_fb, wsas_prmt)


##### exclude variables with MISSING DATA -----
#show how many NAs per variable and make a list of them named "a"
a <- as.data.frame((sapply(wsas_passive, function(x) sum(is.na(x))))/nrow(wsas_passive))
a <- as.data.frame(which(a > .5, arr.ind = TRUE))
low_data <- a$row

names(wsas_passive)  #show me the names of all variables

other_vars <- c(1:13, 48, 77, 116, 137, 148) # for wsas_passive


length(wsas_passive)   # 154
length(other_vars)     # 18
length(low_data)       # 16
154-18-16              # 120 predictor variables left 


inc_vars <- wsas_passive[,c(-c(low_data), -c(other_vars))]
# inc_vars <- wsas_passive[, -c(other_vars)]  # For Fitbit -  HR STEPS and ACTIVITY df only
names(inc_vars)



### Multilevel Regression models -----

wsas_avail <- wsas_passive[, -c(low_data)]  # get rid of vars with missing data


#  ### ### ### ###  ###
### LOAD DESCRIPTIVES.R ###
#  ### ### ### ###  ###


#combine the digital+qids features to demographic variables
names(demographics[c(1, 14,23,24,25,26)])      # demographic variables from script = descriptives.R !!!
d <-merge(wsas_avail, demographics[c(1, 14,23,24,25,26)])
names(d)


data = d         #     <- - - - select the data to be used for the models

## conduct the models adjusting for: Age, gender, depression

for (i in 115:136) {   
  fit <-lmer(total_wsas            
             ~ data[[i]]          
             + Age + gender + qids_total            #covariates  ++ DEPRESSION???
             + (1 |p_id),data = data)  
  print(toupper(names(data)[i]))
  # print(summary(fit))
  # print(round(coef(summary(fit)), digits = 6))
  print(round(confint(fit), digits = 6))
}

fit <-lmer(qids_total ~ deep_pct_std +  Age + gender + (1 |p_id), data = data)
summary(fit)
confint(fit)
