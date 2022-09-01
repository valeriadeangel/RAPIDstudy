
library(tidyverse)
library(stringr)
library(dplyr)
library(reshape2)
library(data.table)
library(broom)
library(readr)
library(readxl)
library(writexl)
library(agricolae)

#load datasets KCL
redcap_full <- read_excel("C:/Users/k1754359/OneDrive - King's College London/PhD/6. Correlation Digital signals in Depression/REDcap full.xlsx")
# names(redcap_full)

wsas_full <- fread("C:/Users/k1754359/Downloads/master_questionnaire_wsas.csv", data.table=F)
IDmap <- read_excel("//kclad.ds.kcl.ac.uk/anywhere/UserData/PSStore01/k1754359/My Documents/R/4. aRMT data.xlsx", sheet = "IDmap")

#load datasets home
redcap_full <- read_excel("C:/Users/valer/Downloads/REDcap full.xlsx")
wsas_full <- fread("C:/Users/valer/Downloads/master_questionnaire_wsas.csv", data.table=F)
IDmap <- read_excel("C:/Users/valer/Downloads/4. aRMT data.xlsx", sheet = "IDmap")




### prepare the outcome variables:

 # - as continuous scales
# 1. qids_total 
# 2. total_phq 
# 3. total_gad
# 4. total_wsas 
# 
# - as binary outcomes
# 5. qids_binary 
# 6. phq_binary  
# 7. gad_binary  
# 8. wsas_binary 
# 9. was_cat




# 1. QIDS ----

# qids <- fread("Downloads/QIDS_summarized.csv", data.table=F) %>%
#   dplyr::mutate(survey_date = lubridate::ymd(survey_date))
# qids$response_time = qids$response_time/60
# qids$complete_time = qids$complete_time/60

# 5. QIDS binary

#split by QIDS = 10/11
#50% of the sample lies on each side in qids_temp and qids
#this is the validated split indicating moderate depression

hist(qids$qids_total)
h<-graph.freq(qids$qids_total,plot=FALSE)
points<-ogive.freq(h,col="red",frame=FALSE,
                   xlab="QIDS", ylab="Accumulated relative frequency", main="Cumulative frequency")


tbl <- table(qids$qids_total)   #change dataset to QIDS_temp?
n <- nrow(qids)                 #change dataset
cbind(Freq=tbl, Cumul=cumsum(tbl), Cumul.prop=cumsum(tbl*100/n))

qids_cat <- qids %>%
  mutate(qids_binary = ifelse(qids_total >9, 1, 0))
  


# 2. PHQ9-----

#create a function to fix the incorrect scoring of scales.
rescore <- function(x, na.rm = FALSE) (x - 1)

phq_select <- redcap_full %>%
  select(c(1, 2, contains("phq9"))) %>%
  filter(record_id > 13) %>%
  filter(str_detect(redcap_event_name, "week|enrolment")) %>% 
  #PHQ was coded incorrectly from 1 - 4 instead of 0 -3, so need to fix  
  mutate(across(phq9_1:phq9_10, rescore)) %>%
  mutate(total_phq = rowSums(across(phq9_1:phq9_9), na.rm = T)) %>%
  filter(!is.na(phq9_timestamp)) %>%    #select only one row per enrolment event
  
  # 6. PHQ9 binary - dichotomise phq9 based on the code further below
  mutate(phq_binary = ifelse(total_phq >9, 1, 0))


#there are several enrolment events per participant. we check how many
a <- phq_select[phq_select$redcap_event_name=="enrolment_arm_1", ] %>%          
  group_by(record_id) %>% 
  summarise(count = n()) 
# View(a)
# we now have los P43 who did not have baseline data PHQ9


#split by PHQ9 = 9/10
#31.74% of the sample scored 9 or less
#this is the validated PHQ9 split indicating moderate depression

hist(phq_select$total_phq, breaks = 5)
h<-graph.freq(phq_select$total_phq,plot=FALSE)
points<-ogive.freq(h,col="red",frame=FALSE,
                   xlab="PHQ", ylab="Accumulated relative frequency", main="Cumulative frequency")


tbl <- table(phq_select$total_phq)   #change dataset to QIDS_temp?
n <- nrow(phq_select)                 #change dataset
cbind(Freq=tbl, Cumul=cumsum(tbl), Cumul.prop=cumsum(tbl*100/n))


# 3. GAD7 ----

gad_select <- redcap_full %>%
  select(c(1, 2, contains("gad7"))) %>%
  filter(record_id > 13) %>%
  filter(str_detect(redcap_event_name, "week|enrolment")) %>% 
  #gad was coded incorrectly from 1 - 4 instead of 0 -3, so need to fix  
  mutate(across(gad7_1:gad7_8, rescore)) %>%
  mutate(total_gad = rowSums(across(gad7_1:gad7_8), na.rm = T)) %>%
  filter(!is.na(gad7_timestamp)) %>%    #select only one row per enrolment event
  
  # 7. GAD7 binary - dichotomise gad7 based on the code further below
  mutate(gad_binary = ifelse(total_gad >9, 1, 0))


#there are several enrolment events per participant. we check how many
a <- gad_select[gad_select$redcap_event_name=="enrolment_arm_1", ] %>%          
  group_by(record_id) %>% 
  summarise(count = n()) 
# View(a)
# we now have los P43 who did not have baseline data gad7

#split by gad7 = 9/10
#40.58% of the sample scored 9 or less
#this is the validated gad7 split indicating moderate depression

hist(gad_select$total_gad, breaks = 5)
h<-graph.freq(gad_select$total_gad,plot=FALSE)
points<-ogive.freq(h,col="red",frame=FALSE,
                   xlab="GAD", ylab="Accumulated relative frequency", main="Cumulative frequency")


tbl <- table(gad_select$total_gad)   #change dataset to QIDS_temp?
n <- nrow(gad_select)                 #change dataset
cbind(Freq=tbl, Cumul=cumsum(tbl), Cumul.prop=cumsum(tbl*100/n))




# 4. WSAS ----

# wsas_full has been loaded as df
# names(wsas_full)
# head(wsas_full)[, 1:3]
# glimpse(wsas_select)

#clean date variable
wsas_full$timestamp <- str_sub(wsas_full$timestamp, 1, 8)
wsas_full$timestamp<-as.Date(as.character(wsas_full$timestamp),format="%Y%m%d")


wsas_id <- merge(wsas_full, IDmap, by.x=c("key.userId"), by.y=c("participant_id"), all.x = TRUE)


# select relevant variables:
wsas_select <- wsas_id %>%
  select(c("key.userId", "record_id", "date_assessment", contains(".value"), "timestamp")) %>%
  mutate(total_wsas = rowSums(across(value.answers.1.value:value.answers.5.value), na.rm = T)) %>%
 
  # 8. WSAS binary
  mutate(wsas_binary = ifelse(total_wsas >9, 1, 0)) %>%
  mutate(wsas_cat = ifelse(total_wsas < 10, 0,
                           ifelse(total_wsas > 9 & total_wsas < 20, 1, 2)))
setnames(wsas_select, old = c('value.answers.0.value','value.answers.1.value','value.answers.2.value','value.answers.3.value','value.answers.4.value','value.answers.5.value'), 
         new = c('wsas0','wsas1','wsas2','wsas3','wsas4','wsas5'))


#split by wsas = 9/10
#18.5% of the sample scored 9 or less
#this is the validated wsas split indicating mild depression

hist(wsas_select$total_wsas)
h<-graph.freq(wsas_select$total_wsas,plot=FALSE)
points<-ogive.freq(h,col="red",frame=FALSE,
                   xlab="wsas", ylab="Accumulated relative frequency", main="Cumulative frequency")


tbl <- table(wsas_select$total_wsas)   #change dataset to QIDS_temp?
n <- nrow(wsas_select)                 #change dataset
cbind(Freq=tbl, Cumul=cumsum(tbl), Cumul.prop=cumsum(tbl*100/n))


### MERGE ALL outcome variables ####

# 1. qids_total 
# 2. total_phq 
# 3. total_gad
# 4. total_wsas 
# 5. qids_binary 
# 6. phq_binary  
# 7. gad_binary  
# 8. wsas_binary 
# 9. was_cat


