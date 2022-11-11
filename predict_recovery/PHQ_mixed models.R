##PHQ 9 analysis
library(matrixStats)
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
library(missMethods)
library(openxlsx)

select <- dplyr::select


#load datasets 
getwd()
# setwd("C:/Users/k1754359/OneDrive - King's College London/PhD/6. Correlation Digital signals in Depression/R scripts")
setwd("C:/Users/valer/Downloads")
download <- "write.csv(, C:/Users/k1754359/Downloads/.csv)"


redcap_full <- read_excel("REDcap full.xlsx")
IDmap <- read_excel("4. aRMT data.xlsx", sheet = "IDmap") |>
  rename(p_id = participant_id)


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
phq <- merge(phq_select, IDmap[, c("p_id", "record_id")])

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
160-63-16              # 81



phq_avail <- phq_passive[,c(-c(low_data), -c(other_vars))]
inc_vars <- phq_avail[, -c(1:17)]  # for correlation plot

## Create baseline phq df with only first 3 weeks -----

baseline_phq <-  phq_avail[phq_avail$redcap_event < 4, ] %>% 
  drop_na(p_id)

baseline_phq %>% 
  count(redcap_event, sort = TRUE)

baseline_phq <- baseline_phq %>% 
  group_by(record_id) %>% 
  summarise_all(mean, na.rm = TRUE)

test <- baseline_phq %>% replace_with_na_all(condition = ~.x == NaN)



#  ### ### ### ###  ###
# LOAD DESCRIPTIVES.R ####
#  ### ### ### ###  ###



IDmap <- read_excel("4. aRMT data.xlsx", sheet = "IDmap")

###  Descriptives  ###

demographics <- read_excel("2. Demographic Table.xlsx", sheet = "Demographics (R)") %>%
  select(c(1:27)) %>%
  rename(p_id = P_ID) %>%
  rename(record_id = record_ID) %>%
  rename(date_of_assessment = `Date of assessment`) %>% 
  rename(tx_start = `Treatment start date`) %>%
  rename(tx_end = `Tx end`) %>%
  rename(tx_length = `Tx length (weeks)`) %>%
  rename(tx_start_days = `Tx start lag (days)`) %>%
  mutate(tx_start_days = as.numeric(tx_start_days)) %>%
  mutate(gender = as.factor(Gender) %>% 
           fct_recode("male" = "Male", "female" = "Female", "other" = "Neither")) %>% 
  mutate(ethnicity = as.factor(Ethnicity) %>% 
           fct_recode(
             "Asian / Asian British / Any other Asian background" = "Asian / Asian British: Indian", 
             "Asian / Asian British / Any other Asian background" = "Any other Asian background (Please describe)",
             "Black / African / Caribbean / Black British / Any other Black background" = "Black / African / Caribbean / Black British: Caribbean",
             "Black / African / Caribbean / Black British / Any other Black background" = "Any other Black / African / Caribbean background (Please describe)",
             "Black / African / Caribbean / Black British / Any other Black background" = "Black / African / Caribbean / Black British: African",
             "Middle Eastern" = "Middle Eastern",
             "Mixed/Multiple ethnic groups" = "Mixed/Multiple ethnic groups: White and Black Caribbean",
             "Mixed/Multiple ethnic groups" = "Any other Mixed / Multiple ethnic background (Please describe)",
             "White / White British / Any other White background" = "Any other White background (please describe)", 
             "White / White British / Any other White background" = "White: English / Welsh / Scottish / Northern Irish / British",
             "White / White British / Any other White background" = "White: Irish",
           )) %>% 
  mutate(education_level = as.factor(`What is your highest completed level of education?`) %>% 
           fct_recode(
             "Degree level" = "Degree level education / diploma (e.g. BSc, BA)",
             "Post-graduate Degree" = "Post-graduate Degree (e.g. MSc, MA, PhD)",                                                        
             "College level or equivalent" = "College level education or equivalent (A lever, NVQ, International Baccaleureate, BTEC nationals)",
             "Secondary education" = "Secondary education (GCSE, O Levels)"   
           )) %>% 
  mutate(employment = as.factor(`What is your main employment status?`)) %>% 
  select(-c(Gender, DoB, Ethnicity, `What is your main employment status?`, `What is your highest completed level of education?` ))
demographics$tx_start = convertToDate(demographics$tx_start, origin = "1900-01-01")
demographics$tx_end = convertToDate(demographics$tx_end, origin = "1900-01-01")


names(demographics[c(1,4:7, 14,23,24,25,26)])      # demographic variables from script = descriptives.R 

# create variables:
## baseline data + mid-point data + endpoint data ----
d <-merge(phq_avail, demographics[c(1,4:7, 14,23,24,25,26)])

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
  mutate(mid = ifelse(tx_length > 3 & baselinediff > (half_tx-8) & baselinediff < (half_tx + 8), 1, 0)) |>
  
  #create tx_time as factor
  mutate(tx_time = as.factor(case_when(
    baseline == 1 ~ "baseline",
    mid == 1 ~ "mid",
    end ==1 ~ "end"))) |>
  
  select(-c("baselinediff", "baseline", "enddiff", "end", "half_tx", "mid"))

  
  
# View(phq_tx[c("survey_date", "tx_start", "baseline", 
#               "tx_end", "end","baselinediff", "half_tx", "mid")])

# test <- replace_with_na_all(phq_tx, condition = ~.x == "NaN")


## treatment data with no digital features -----
d <-merge(phq, demographics[c(1,4:7, 14,23,24,25,26)])

phq_tx_nofeatures <- d %>% 
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
  mutate(mid = ifelse(tx_length > 3 & baselinediff > (half_tx-8) & baselinediff < (half_tx + 8), 1, 0)) |>
  
  #create tx_time as factor
  mutate(tx_time = as.factor(case_when(
    baseline == 1 ~ "baseline",
    mid == 1 ~ "mid",
    end ==1 ~ "end"))) |>   
  
  select(-c("baselinediff", "baseline", "enddiff", "end", "half_tx", "mid"))

#create phq in wide format
phq_wide <- phq_tx_nofeatures |>
  select(record_id, redcap_event, total_phq) |>
  spread(key = redcap_event, value = total_phq)
phq_wide_2 <- phq_tx_nofeatures |>
  select(record_id, redcap_event, tx_time) |>
  spread(key = redcap_event, value = tx_time)

phq_bind <- rbind(phq_wide, phq_wide_2)

phq_nof <- phq_tx_nofeatures |>
  #remove those without who did not get treatment
  filter(!record_id %in% c(30, 31, 37, 45, 47, 62, 63)) |> 
  #treatment < 3 weeks
  filter(!record_id %in% c(18, 34, 37, 53)) |>
  
  filter(!is.na(tx_time))

#group any multiple values of tx_time (e.g. 2 baselines)
phq_time <- phq_nof |>
  select(tx_time, total_phq, record_id, redcap_event) |>
  group_by(record_id, tx_time) |>
  summarise(total_phq = mean(total_phq),
            redcap_event = mean(redcap_event),.groups = "keep") |>
  spread(key = tx_time, value = total_phq) |>
  relocate(c("record_id", "baseline", "mid", "end" ))

no_id <- phq_time[,2:4]

##imputation ----

# imputed <- no_id |>
#   apply_imputation(FUN = median, type = "rowwise")
# 
# 
# write.csv(phq_time,"C:/Users/k1754359/Downloads/phq_time.csv")

 # missing phq 9 values were imputed by hand using IAPT data
# setwd("C:/Users/k1754359/OneDrive - King's College London/PhD/7. Predicting Treatment Outcome")
phq_imputed <- read_excel("treatment data.xlsx", sheet = "phq_time_r",
                          col_types = "numeric")


phq_long <- phq_imputed |>
  pivot_longer(
    cols = "phq_baseline":"phq_end",
    names_to = "tx_time",
    values_to = "phq_total") |>
  select(-c("redcap_baseline":"redcap_end"))
phq_long$tx_time <- phq_long$tx_time %>%      
  str_remove_all("phq_")

redcap_long <- phq_imputed |>
  pivot_longer(
    cols = "redcap_baseline":"redcap_end",
    names_to = "tx_time",
    values_to = "redcap_event") |>
  select(-c("phq_baseline":"phq_end"))
redcap_long$tx_time <- redcap_long$tx_time %>%        
  str_remove_all("redcap_")
imp_merged <- merge(redcap_long, phq_long) 


#join imputed phqs with features
#first join with existing weeks
complete <- merge(imp_merged, phq_tx, by = c("record_id", "redcap_event"), all.y = FALSE )

#get a df with missing data and complete with existing phq df scores, i.e. phq_avail
incomplete <- merge(imp_merged, phq_tx, by = c("record_id", "redcap_event"),  all.x = TRUE, all.y = FALSE ) |>
  filter(is.na(p_id)) |>
  select(c("record_id","tx_time.x","redcap_event","phq_total"))

incomplete$redcap_event_plus <- incomplete$redcap_event +1
incomplete$redcap_event_minus <- incomplete$redcap_event -1


a <- incomplete |>
  select(-redcap_event) |>
  rename(redcap_event = redcap_event_plus) |>
  select(-redcap_event_minus)
  
b <- incomplete |>
  select(-redcap_event) |>
  rename(redcap_event = redcap_event_minus) |>
  select(-redcap_event_plus) 

c <- rbind(a,b)

#add features to the missing data phq rows
regained <- c|>
  merge(phq_avail, all.x = TRUE, all.y = FALSE) |>
  drop_na(total_phq)

#we gain back some of the missing values from the original dataset
new_phqs <- regained |>
  select(-c("survey_date", "p_id")) |>
  group_by(record_id, tx_time.x) |>
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "keep")

#and we then merge it with the complete df
phq_tx_time <- merge(new_phqs, complete, all = TRUE) |>
  select(-c("p_id","Age", "gender", "ethnicity", "tx_start", "tx_start_days","tx_end","tx_length","education_level","employment","phq_binary"))

a <- read_excel("2. Demographic Table.xlsx", sheet = "Demographics (R)") %>%
  select(c(2,15,17,66)) %>%
  rename(record_id = record_ID) |>
  rename(age = `Age Rounddown`) |>
  rename(gender = Gender) |>
  rename(tx_intensity = `Therapy Intensity`)
  
phq_tx_time <- phq_tx_time |>
  merge(a, by = "record_id") |>
  mutate(tx_intensity = as.factor(tx_intensity)) |>
  select(-c("tx_time.y", "total_phq", )) |>
  rename(tx_time = tx_time.x) |>
  mutate(phq_binary = ifelse(phq_total >9, 1, 0))
phq_tx_time$tx_time <- factor(phq_tx_time$tx_time, levels=c("baseline", "mid", "end"))

##### FINAL df #####
View(phq_tx_time)
names(phq_tx_time)

m <- (phq_tx_time)[16:79]
M <- as.data.frame(colMeans(m, na.rm = TRUE))
SD <- apply(m, 2, sd, na.rm = TRUE)
msd <- cbind(M, SD)
# write.csv(msd,"msd.csv")

# #  # #  # #  # #  # #   
## RUN speech analysis.R ##
# #  # #  # #  # #  # #  

# speech_u
# speech_s

#if merged both speech tasks:
speech <- merge(speech_u, speech_s, by = c("p_id","date_str", "event_name", "date_assessment" ), all = TRUE)

#switch between _s and _u
speech <- merge(speech_u, IDmap, all = TRUE)
speech <- speech %>% 
  filter(p_id != "f5978923-cef2-4eeb-a49c-7d79be4b53eb", p_id != "5786af5e-99e3-4f78-848a-d3b67b8eb7ed") |>
  rename(redcap_event = event_name)

# Speech features PHQ ---- 
#merge speech and phq
m_speech_s <- merge(phq, speech, by = c("record_id", "redcap_event"), all.x = T)
m_speech_u <- merge(phq, speech, by = c("record_id", "redcap_event"), all.x = T)



# Calculate change in values -----

changedf <- phq_tx[,c(2:13, 15, 18:81)]
rows <- nrow(changedf)
diff_frame <- changedf[-1,] - changedf[-rows,]   
# difference calculated if BOTH the current and previous weeks are available. So much missing data.



#need to separate because they have different NA value amounts
#Sleep
sleep <- phq_tx[,c(2,4:15,17,18:35)]
#HR
hr <- phq_tx[,c(2,4:15,17,36:53)]
#step
step <- phq_tx[,c(2,4:15,17,54:65)]
#activity
activity <- phq_tx[,c(2,4:15,17,66:81)]
#scripted speech
scripted_s <- m_speech_s[,c(1,4:17, 23:50)]
#unscripted speech
unscripted_s <- m_speech_u[,c(1,4:17, 23:50)]

df = sleep        # <----------   CHANGE here depending on feature
names(sleep)
## create df with change in variables for df
df <- df %>% 
  drop_na(15) %>%       # <----------   CHANGE here if speech(23) vs fitbit (15)
  group_by(record_id) %>% 
  arrange(survey_date, .by_group = TRUE)
rows <- nrow(df)
b <- df[-1,]
c <- df[-rows,]
change_df <- b-c

#extract survey_date from dataframe without the first p_id rows
date <- df[, c("record_id", "survey_date")]

# now we need to add p_id again
d <- change_df %>% 
  rename(id_change = record_id) |>
  rename(date_change = survey_date) |>
  bind_cols(date[-1,]) |>
  subset(id_change != 1) |>
  merge(demographics[,c("record_id","Age", "gender")], by.x="record_id")



# change in depression but not in features = t
# change df - the phq items
phq_change <- change_df[,1:14]
phq_change<- rename(phq_change, id_change = record_id)
#normal df features
names(df)
unchanged_df <- df[-1, c(1,15:32)]  # <---------- CHANGE here depending on feature: fitbit(c(1,15:32))


t<-phq_change |>
  cbind(unchanged_df) |>
  subset(id_change != 1) |>
  merge(demographics[,c("record_id","Age", "gender")], by.x="record_id")
#

 

# Linear mixed-effects models -----


data = d         #     <- - - - select the data to be used for the models
names(data)

## conduct the models adjusting for Age, gender.

for (i in 16:33) {        # <---------- change depending on df used : HR = sleep
  fit <-lmer(total_phq               
             ~ data[[i]]          
             + Age + gender  
             + (1 |record_id),data = data) 
  print(toupper(names(data)[i]))
  # print(summary(fit))
  print(round(coef(summary(fit)), digits = 6))
  # print(round(confint(fit), digits = 6))
}

fit <-lmer(total_phq ~ total_sleep_time_mean +  Age + gender + (1 |record_id), data = data)
summary(fit)
confint(fit)


## export results ----



names(data)
data = d        #     <- - - - select the data to be used for the models

FEATURE <- c()
Estimate <- c()
StdError <- c()
z_value <- c()
pr_t <- c()
OR <-c()
lowerCI<-c()
upperCI<-c()

## Basic
for (i in 16:33) {    # <---------- CHANGE here depending on feature

  fit <-lmer(total_phq
             ~ data[[i]]
             + Age + gender
             + (1 |record_id),data = data)
  
  FEATURE <- append(FEATURE,toupper(names(data)[i]))
  Estimate <- append(Estimate,round(coef(summary(fit)), digits = 6)[2,1])
  StdError <- append(StdError,round(coef(summary(fit)), digits = 6)[2,2])
  z_value <- append(z_value,round(coef(summary(fit)), digits = 6)[2,3])
  pr_t <- append(pr_t,round(coef(summary(fit)), digits = 6)[2,4])
  OR <- append(OR,round(exp(coef(fit)), digits = 6)[2])
  lowerCI <- append(lowerCI,round(exp(confint(fit)), digits = 6)[2,1])
  upperCI <- append(upperCI,round(exp(confint(fit)), digits = 6)[2,2])
  
  Sys.sleep(0.1)
  
}
# data = d : change in both, 
# data = t : change in PHQ only

results= data.frame(FEATURE,Estimate,StdError,z_value,pr_t, OR, lowerCI, upperCI)
colnames(results) <- c("FEATURE","Estimate","Std. eror","zvalue","pvalue", "OR", "lower CI", "upper CI")
# write.csv(results,"C:/Users/k1754359/OneDrive - King's College London/PhD/7. Predicting Treatment Outcome/results.csv",row.names = FALSE)
# write.csv(results,"C:/Users/k1754359/Downloads/results.csv",row.names = FALSE)
write.csv(results,"results.csv",row.names = FALSE)

results |> filter(pvalue < 0.05)


# =IF(E2<0.05, "*", "")



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



#plots - PHQ stages of treatment ----
library(GGally)
library(hrbrthemes)
library(viridis)

phq_tx_time |>
  select(record_id, tx_intensity, tx_time, phq_total) |>
  spread(key = tx_time, value = phq_total) |>
  relocate(c("record_id", "baseline", "mid", "end" )) |>
  ggparcoord(columns = 2:4, groupColumn = 5, 
             scale="globalminmax",
             showPoints = TRUE, 
             title = "PHQ at stages of Treatment",
             alphaLines = 0.3) + 
  theme_ipsum()+
  theme(
    # legend.position="none",
    plot.title = element_text(size=13)) +
  xlab("Treatment stage") +
  ylab("PHQ9")


# Boxplot

data <- phq_tx_time |>
  select(record_id, tx_intensity, tx_time, phq_total) |>
  spread(key = tx_time, value = phq_total) |>
  relocate(c("record_id", "baseline", "mid", "end" ))

phq_tx_time %>%
  ggplot( aes(x=tx_time, y=phq_total, fill=tx_time)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  # geom_jitter(color="black", size=0.4, alpha=0.9) +  #without jitter
  theme_ipsum() +
  theme(
    # legend.position="none",
    plot.title = element_text(size=11)) +
  ggtitle("PHQ at stages of Treatment") +
  xlab("Treatment stage") +
  ylab("PHQ9")



### Tables ####
install.packages("gt")
library(gt)
table |> gt()
table <- phq_tx_time |>
  group_by(tx_time) |>
  summarise(phq = mean(phq_total))
