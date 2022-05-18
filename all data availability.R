##DATA AVAILABILITY

library(plyr)
library(dplyr)
library(tidyverse)
library(reshape2)
library(data.table)
library(broom)
library(readr)
library(readxl)
library(writexl)
library(stringr)
library(rlist)
library(lubridate)
library(timetk)
library(hrbrthemes)
library(viridis)


# Datasets that need to be combined
# 1. REDCap
# 2. pRMT
# 3. aRMT
# 4. Fitbit
#BY record_id AND event_name

## Download the following
#followup_redcap.xlsx"
#2. Demographic Table.xlsx"
#3. REDCap active data (LongBook1_REDCap).xlsx"
#4. aRMT data.xlsx"
#5. pRMT data.xlsx"
#6. Fitbit data.xlsx"




#### 1. REDCap data -------------------------------------------------------------

# create the follow-up dataset
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


# Create tx status data frame with ALL participants WIDE
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
  select(record_id, redcap_event_name, data_present)

#merge both datasets
fu_redcap <- merge(redcap_subset, fu_long, all.x = TRUE)

fu_redcap <- fu_redcap %>%                           # convert Ended by to factor (categorical) with 3 levels
  mutate(tx_status = factor(as.character(tx_status),
                            levels = c(1, 2, 3),
                            labels = c("pre_tx", "tx", "post_tx")))

# View(fu_redcap)



## REDcap end ----

ls(fu_redcap)
names(fu_redcap)[names(fu_redcap) == "redcap_event_name"] <- 'event_name'     # rename
# names(fu_redcap)[names(fu_redcap) == "perc.complete"] <- 'redcap_completion'  # rename
names(fu_redcap)[names(fu_redcap) == "data_present"] <- 'redcap_binary'       # rename



# View(fu_redcap)


#### 2. from pRMT ------------------------------------------------------------

#import dataframes
IDmap <- read_excel("~/R/5. pRMT data.xlsx")
X5_pRMT_data <- read_excel("~/R/5. pRMT data.xlsx", 2)

#add the REDCap ID numbers 
pRMT <- merge(X5_pRMT_data, IDmap, all.x = TRUE, all.y=TRUE, no.dups = TRUE, incomparables = NULL)
pRMT <- pRMT %>% drop_na()         #drop IDs that are not in the IDmap df i.e. test subjects

#add study days
pRMT_days <- pRMT %>%
  arrange(ymd(pRMT$date)) %>%
  group_by(SUBJECT_ID) %>%
  mutate(day = row_number())

#leave only days in study
pRMT_allIDs <- pRMT_days
test14 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "14" & pRMT_allIDs$day > 282)
test14 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "14" & pRMT_allIDs$day > 282)
test15 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "15" & pRMT_allIDs$day > 222)
test16 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "16" & pRMT_allIDs$day > 223)
test17 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "17" & pRMT_allIDs$day > 214)
test18 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "18" & pRMT_allIDs$day > 214)
test19 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "19" & pRMT_allIDs$day > 194)
test20 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "20" & pRMT_allIDs$day > 188)
test21 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "21" & pRMT_allIDs$day > 264)
test22 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "22" & pRMT_allIDs$day > 293)
test23 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "23" & pRMT_allIDs$day > 267)
test24 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "24" & pRMT_allIDs$day > 186)
test25 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "25" & pRMT_allIDs$day > 280)
test26 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "26" & pRMT_allIDs$day > 140)
test27 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "27" & pRMT_allIDs$day > 222)
test28 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "28" & pRMT_allIDs$day > 291)
test29 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "29" & pRMT_allIDs$day > 232)
test30 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "30" & pRMT_allIDs$day > 199)
test31 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "31" & pRMT_allIDs$day > 261)
test32 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "32" & pRMT_allIDs$day > 249)
test33 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "33" & pRMT_allIDs$day > 92)
test34 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "34" & pRMT_allIDs$day > 245)
test35 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "35" & pRMT_allIDs$day > 252)
test36 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "36" & pRMT_allIDs$day > 269)
test37 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "37" & pRMT_allIDs$day > 226)
test38 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "38" & pRMT_allIDs$day > 245)
test39 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "39" & pRMT_allIDs$day > 192)
test40 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "40" & pRMT_allIDs$day > 219)
test41 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "41" & pRMT_allIDs$day > 201)
test42 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "42" & pRMT_allIDs$day > 182)
test43 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "43" & pRMT_allIDs$day > 187)
test44 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "44" & pRMT_allIDs$day > 223)
test45 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "45" & pRMT_allIDs$day > 212)
test46 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "46" & pRMT_allIDs$day > 216)
test47 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "47" & pRMT_allIDs$day > 213)
test48 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "48" & pRMT_allIDs$day > 190)
test49 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "49" & pRMT_allIDs$day > 290)
test50 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "50" & pRMT_allIDs$day > 189)
test51 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "51" & pRMT_allIDs$day > 250)
test52 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "52" & pRMT_allIDs$day > 243)
test53 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "53" & pRMT_allIDs$day > 154)
test54 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "54" & pRMT_allIDs$day > 191)
test55 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "55" & pRMT_allIDs$day > 135)
test56 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "56" & pRMT_allIDs$day > 156)
test57 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "57" & pRMT_allIDs$day > 197)
test58 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "58" & pRMT_allIDs$day > 354)
test59 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "59" & pRMT_allIDs$day > 182)
test60 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "60" & pRMT_allIDs$day > 214)
test61 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "61" & pRMT_allIDs$day > 154)
test62 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "62" & pRMT_allIDs$day > 105)
test63 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "63" & pRMT_allIDs$day > 183)
test64 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "64" & pRMT_allIDs$day > 156)
test65 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "65" & pRMT_allIDs$day > 239)
test66 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "66" & pRMT_allIDs$day > 144)
test67 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "67" & pRMT_allIDs$day > 238)
test68 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "68" & pRMT_allIDs$day > 160)
test69 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "69" & pRMT_allIDs$day > 147)
test70 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "70" & pRMT_allIDs$day > 253)
test71 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "71" & pRMT_allIDs$day > 176)
test72 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "72" & pRMT_allIDs$day > 181)
test73 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "73" & pRMT_allIDs$day > 257)
test74 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "74" & pRMT_allIDs$day > 251)
test75 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "75" & pRMT_allIDs$day > 182)
test76 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "76" & pRMT_allIDs$day > 239)
test77 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "77" & pRMT_allIDs$day > 122)
test78 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "78" & pRMT_allIDs$day > 164)
test79 <- subset(pRMT_allIDs, pRMT_allIDs$record_ID == "79" & pRMT_allIDs$day > 144)

test2 <- anti_join(pRMT_allIDs, test14)
test2 <- anti_join(test2, test15)
test2 <- anti_join(test2, test16)
test2 <- anti_join(test2, test17)
test2 <- anti_join(test2, test18)
test2 <- anti_join(test2, test19)
test2 <- anti_join(test2, test20)
test2 <- anti_join(test2, test21)
test2 <- anti_join(test2, test22)
test2 <- anti_join(test2, test23)
test2 <- anti_join(test2, test24)
test2 <- anti_join(test2, test25)
test2 <- anti_join(test2, test26)
test2 <- anti_join(test2, test27)
test2 <- anti_join(test2, test28)
test2 <- anti_join(test2, test29)
test2 <- anti_join(test2, test30)
test2 <- anti_join(test2, test31)
test2 <- anti_join(test2, test32)
test2 <- anti_join(test2, test33)
test2 <- anti_join(test2, test34)
test2 <- anti_join(test2, test35)
test2 <- anti_join(test2, test36)
test2 <- anti_join(test2, test37)
test2 <- anti_join(test2, test38)
test2 <- anti_join(test2, test39)
test2 <- anti_join(test2, test40)
test2 <- anti_join(test2, test41)
test2 <- anti_join(test2, test42)
test2 <- anti_join(test2, test43)
test2 <- anti_join(test2, test44)
test2 <- anti_join(test2, test45)
test2 <- anti_join(test2, test46)
test2 <- anti_join(test2, test47)
test2 <- anti_join(test2, test48)
test2 <- anti_join(test2, test49)
test2 <- anti_join(test2, test50)
test2 <- anti_join(test2, test51)
test2 <- anti_join(test2, test52)
test2 <- anti_join(test2, test53)
test2 <- anti_join(test2, test54)
test2 <- anti_join(test2, test55)
test2 <- anti_join(test2, test56)
test2 <- anti_join(test2, test57)
test2 <- anti_join(test2, test58)
test2 <- anti_join(test2, test59)
test2 <- anti_join(test2, test60)
test2 <- anti_join(test2, test61)
test2 <- anti_join(test2, test62)
test2 <- anti_join(test2, test63)
test2 <- anti_join(test2, test64)
test2 <- anti_join(test2, test65)
test2 <- anti_join(test2, test66)
test2 <- anti_join(test2, test67)
test2 <- anti_join(test2, test68)
test2 <- anti_join(test2, test69)
test2 <- anti_join(test2, test70)
test2 <- anti_join(test2, test71)
test2 <- anti_join(test2, test72)
test2 <- anti_join(test2, test73)
test2 <- anti_join(test2, test74)
test2 <- anti_join(test2, test75)
test2 <- anti_join(test2, test76)
test2 <- anti_join(test2, test77)
test2 <- anti_join(test2, test78)
test2 <- anti_join(test2, test79)

pRMT_lengthstudy <- test2

select_pRMT <- pRMT_lengthstudy %>%
  select(record_ID,day, bt_hour, acc_hour, gps_hour, sleep_binary)


## pRMT end ----

ls(select_pRMT)
names(select_pRMT)[names(select_pRMT) == "record_ID"] <- 'record_id'         # rename

select_pRMT$any_pRMT <- ifelse(select_pRMT$bt_hour > 7 | select_pRMT$gps_hour > 7 | select_pRMT$acc_hour > 7, as.numeric(1), as.numeric(0))


weekly_pRMT <- select_pRMT %>%                      
  mutate(event_name = ceiling(day / 7)) %>%            #create variable "week"
  group_by(record_id, event_name) %>%             #create variable "week"
  summarise_if(is.numeric, mean, na.rm = TRUE)    #mean n of hours per day / sleep = mean n of days

cols_to_drop <- c("day")                               #get rid of variable "day"
weekly_pRMT = weekly_pRMT[,!(names(weekly_pRMT) %in% cols_to_drop)]
weekly_pRMT <- subset(weekly_pRMT, event_name < 41)    #filter weeks over 40

conv_weekly_pRMT <- weekly_pRMT %>%
  mutate(gps_perc = gps_hour/24) %>%
  mutate(acc_perc = acc_hour/24) %>%
  mutate(bt_perc = bt_hour/24)

conv_weekly_pRMT <- conv_weekly_pRMT %>%
  select(record_id, event_name, sleep_binary, any_pRMT)

# View(conv_weekly_pRMT)

### for Figure - passive data by data stream

pRMT8hr <- select_pRMT %>% 
  mutate(gps = ifelse(gps_hour > 7, 1, 0)) %>%
  mutate(acc = ifelse(acc_hour > 7, 1, 0)) %>%
  mutate(bt = ifelse(bt_hour > 7, 1, 0)) %>% 
  select(record_id, day, gps, acc, bt, sleep_binary)

names(pRMT8hr)[names(pRMT8hr) == "SUBJECT_ID"] <- 'participant_id'         # rename


fitbit_hr <- fitbit_hr %>%
  drop_na(record_id)

fitbit_hr$day <- as.numeric(as.factor(fitbit_hr$day))
fitbit_hr$HR_missing <- as.numeric(as.character(fitbit_hr$HR_missing))
fitbit_avail <- fitbit_hr %>%
  mutate(hr_perc = 1-(HR_missing/100)) %>% 
  mutate(fitbit = ifelse(hr_perc > 0.32, 1, 0)) %>% 
  select(record_id, day, fitbit)

pRMT_binary <- merge(fitbit_avail, pRMT8hr, by = c("record_id", "day"), all.x = TRUE)

weekly_pRMT <- pRMT_binary %>%                      
  mutate(event_name = ceiling(day / 7)) %>%            #create variable "week"
  group_by(record_id, event_name) %>%             #create variable "week"
  summarise_if(is.numeric, mean, na.rm = TRUE)    #mean n of hours per day / sleep = mean n of days

cols_to_drop <- c("day")                               #get rid of variable "day"
weekly_pRMT = weekly_pRMT[,!(names(weekly_pRMT) %in% cols_to_drop)]
weekly_pRMT <- subset(weekly_pRMT, event_name < 41)    #filter weeks over 40

weekly_pRMT[weekly_pRMT == "NaN"] <- 0


# Study days only ---------------------------------------------------------

df <- weekly_pRMT

### for each participant: NA after last day in study.
df[df$record_id ==14  & df$event_name > 40, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==15  & df$event_name > 31, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==16  & df$event_name > 32, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==17  & df$event_name > 30, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==18  & df$event_name > 30, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==19  & df$event_name > 27, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==20  & df$event_name > 26, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==21  & df$event_name > 38, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==22  & df$event_name > 40, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==23  & df$event_name > 38, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==24  & df$event_name > 27, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==25  & df$event_name > 40, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==26  & df$event_name > 21, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==27  & df$event_name > 31, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==28  & df$event_name > 40, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==29  & df$event_name > 33, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==30  & df$event_name > 28, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==31  & df$event_name > 37, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==32  & df$event_name > 35, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==33  & df$event_name > 14, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==34  & df$event_name > 35, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==35  & df$event_name > 36, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==36  & df$event_name > 38, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==37  & df$event_name > 33, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==38  & df$event_name > 35, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==39  & df$event_name > 27, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==40  & df$event_name > 31, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==41  & df$event_name > 28, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==42  & df$event_name > 27, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==43  & df$event_name > 26, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==44  & df$event_name > 31, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==45  & df$event_name > 30, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==46  & df$event_name > 31, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==47  & df$event_name > 31, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==48  & df$event_name > 27, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==49  & df$event_name > 40, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==50  & df$event_name > 27, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==51  & df$event_name > 35, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==52  & df$event_name > 34, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==53  & df$event_name > 22, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==54  & df$event_name > 27, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==55  & df$event_name > 19, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==56  & df$event_name > 22, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==57  & df$event_name > 28, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==58  & df$event_name > 40, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==59  & df$event_name > 26, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==60  & df$event_name > 30, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==61  & df$event_name > 22, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==62  & df$event_name > 15, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==63  & df$event_name > 26, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==64  & df$event_name > 22, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==65  & df$event_name > 34, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==66  & df$event_name > 20, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==67  & df$event_name > 34, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==68  & df$event_name > 22, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==69  & df$event_name > 21, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==70  & df$event_name > 36, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==71  & df$event_name > 26, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==72  & df$event_name > 26, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==73  & df$event_name > 36, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==74  & df$event_name > 36, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==75  & df$event_name > 26, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==76  & df$event_name > 36, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==77  & df$event_name > 18, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==78  & df$event_name > 23, c(3, 4, 5, 6, 7)] <- NA
df[df$record_id ==79  & df$event_name > 20, c(3, 4, 5, 6, 7)] <- NA

weekly_pRMT <- df

## 8 hr pRMT end -----

pRMTmean_comp <- weekly_pRMT %>% 
  group_by(event_name) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)



#how many people have an average of 8 hrs a day, each week.
weekly_pRMTbinary <- weekly_pRMT %>% 
  mutate(gps = ifelse(gps > 0.49, 1, 0)) %>%
  mutate(acc = ifelse(acc > 0.49, 1, 0)) %>%
  mutate(bt = ifelse(bt > 0.49, 1, 0)) %>% 
  mutate(sleep_binary = ifelse(sleep_binary > 0.49, 1, 0)) %>% 
  mutate(fitbit = ifelse(fitbit > 0.49, 1, 0))

# weekly_pRMTbinary %>% 
#   group_by(event_name) %>% 
#   mutate(week_avail = sum(sleep_binary, na.rm = TRUE)) %>%     #calculate how many participants have data available
#   mutate(total_week = n())  %>%                  #out of the number of people that week
#   mutate(prop.available = (week_avail/total_week)*100) %>%
#   # mutate(is_outlier = total_week < 6) %>%        #to exclude weeks with < 5 people (1)
#   # filter(is_outlier == FALSE) %>%                #(2)
#   summarise_if(is.numeric, mean, na.rm = TRUE)

# write.csv(pRMTmean_comp,"~/R/pRMTcomp.csv", row.names = FALSE)   #export to excel


#### 3. aRMT ---------------------------------------------------------------

# install.packages("rlist")

# library(rlist)
# library(readxl)
# library(dplyr)
# library(ggplot2)
# library(stringr)
# library(tidyr)
# library(writexl)
# library(tidyverse)


QIDS <- read_excel("~/R/4. aRMT data.xlsx", sheet = "QIDS", 
                   col_types = c("text", "date", "date", 
                                 "numeric", "date", "date", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric"))

names(QIDS)[names(QIDS) == "participant_ID"] <- 'participant_id'     # rename

selectq <- QIDS %>% select(participant_id, date_notified, `Total Score`)

WSAS <- read_excel("~/R/4. aRMT data.xlsx", sheet = "WSAS", 
                   col_types = c("text", "date", "date", 
                                 "numeric", "date", "date", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric"))


names(WSAS)[names(WSAS) == "participant_ID"] <- 'participant_id'     # rename

selectw <- WSAS %>% select(participant_id, date_notified, `Total Score`)

IDmap <- read_excel("~/R/4. aRMT data.xlsx", 
                    sheet = "IDmap")


# aRMT questionnaires --------------------------------------

#assign double digit participant IDs
mergedID <- merge(IDmap, selectq,  all.x = TRUE, all.y=TRUE, no.dups = TRUE, incomparables = NULL)
mergedwithtest <- merge(selectw, mergedID, by = c("participant_id", "date_notified"), all.x = TRUE, all.y = TRUE, no.dups = TRUE)
# length(unique(mergedID$participant_id))
# colSums(is.na(mergedID))


mergedaRMT <- mergedwithtest %>% 
  subset(!(participant_id %in% c('f5978923-cef2-4eeb-a49c-7d79be4b53eb',
                                 '5786af5e-99e3-4f78-848a-d3b67b8eb7ed'))) %>% 
  drop_na(record_id)

# write.csv(mergedwithtest,"~/R/mergedtest aRMT.csv", row.names = FALSE)   #export to excel

mergedaRMT$diff_in_days = as.numeric(difftime(mergedaRMT$date_notified, mergedaRMT$date_assessment, units = "days")) 
mergedaRMT$event_name = ceiling((mergedaRMT$diff_in_days)/7)

#some questionnaires were carried out on the same week (e.g. if WSAS completed on days 1 and 7 - both fall in week 1)
#so adding 1 day (+1) to the difference in days spreads them out better. 
mergedaRMT$event_name1 = ceiling((mergedaRMT$diff_in_days+1)/7)


# getting rid of duplicate weeks -----
# we check how many unique values of weeks there are (a and b) 
a <- mergedaRMT %>%         
  group_by(record_id) %>%
  summarise(a = length(unique(event_name)))
b <- mergedaRMT %>%
  group_by(record_id) %>%
  summarise(b = length(unique(event_name1)))
c <- mergedaRMT %>%
  group_by(record_id) %>%
  summarise(count = n())

# View(merge(c,b))

eventcheck <- merge(c,b)
eventcheck$d <- eventcheck$count - eventcheck$b # creates a column showing which Ps have more events than unique week numbers
eventcheck[which(eventcheck$d != 0),]   # calls the rows with duplicate week numbers

# mergedaRMT <- subset(mergedaRMT, !is.na(record_id))  #this step has been done already


# delete (NA) or rearrange duplicate weeks
mergedaRMT[mergedaRMT$record_id == 35  & mergedaRMT$event_name == 5, "event_name1"] <- 6
mergedaRMT[mergedaRMT$record_id == 44  & mergedaRMT$event_name == 4, "event_name1"] <- 5
mergedaRMT[mergedaRMT$record_id == 44  & mergedaRMT$event_name == 19, "event_name1"] <- 19
mergedaRMT[mergedaRMT$record_id == 58  & mergedaRMT$event_name == 4, "event_name1"] <- 4
mergedaRMT[mergedaRMT$record_id == 74  & mergedaRMT$event_name == 13, "event_name1"] <- 13

mergedaRMT[mergedaRMT$record_id == 45  & mergedaRMT$diff_in_days == 15, "date_assessment"] <- NA
mergedaRMT[mergedaRMT$record_id == 45  & mergedaRMT$event_name == 2, "date_assessment"] <- NA
mergedaRMT[mergedaRMT$record_id == 55  & mergedaRMT$event_name == 4, "date_assessment"] <- NA
mergedaRMT[mergedaRMT$record_id == 58  & mergedaRMT$event_name == 0, "date_assessment"] <- NA
mergedaRMT[mergedaRMT$record_id == 74  & mergedaRMT$event_name == 0, "date_assessment"] <- NA
mergedaRMT[mergedaRMT$record_id == 79  & mergedaRMT$event_name == 18, "date_assessment"] <- NA

nodups <- subset(mergedaRMT, !is.na(date_assessment))    #gets rid of participants with duplicate weeks as prepared above

test <- subset(nodups, !is.na(event_name1))         #gets rid of participants with no data ever (i.e. those with no record id)
mergedaRMT[which(is.na(mergedaRMT$event_name1)), c("record_id")]    #checks if there are any record_id = NA


aRMT <- nodups %>%
  select(participant_id, record_id, 'Total Score.x', 'Total Score.y', event_name1)
names(aRMT)[names(aRMT) == "Total Score.x"] <- 'total_qids'     # rename
names(aRMT)[names(aRMT) == "Total Score.y"] <- 'total_wsas'     # rename
names(aRMT)[names(aRMT) == "event_name1"] <- 'event_name'       # rename

## not all participants have aRMT data  ! 
list.count(unique(aRMT$participant_id))   #so there are 66 unique ID numbers in this list
list.count(unique(test$participant_id))   #   there are 61 unique ID numbers /66 participants
length(unique(aRMT$participant_id))  

#create a dataframe for all weeks and all participants
event_name <- rep(1:40, times=66)
record_id <- rep(14:79, each=40)
allweeks <- as.data.frame(cbind(record_id, event_name))

merged <- merge(allweeks, IDmap[, c("record_id","participant_id")], by = "record_id", all.x = TRUE)
# View(merged)

#new data frame with all weeks = aRMT40
aRMT40 <- merge(merged, aRMT, all.x = TRUE)

aRMT40 <- aRMT40 %>% 
  mutate(comp = ifelse(is.na(total_qids) & is.na(total_wsas), 0, 1))

colSums(is.na(aRMT40))


# Study days only ---------------------------------------------------------

df <- aRMT40

### for each participant: NA after last day in study.
df[df$record_id ==14  & df$event_name > 40, 6] <- NA
df[df$record_id ==15  & df$event_name > 31, 6] <- NA
df[df$record_id ==16  & df$event_name > 32, 6] <- NA
df[df$record_id ==17  & df$event_name > 30, 6] <- NA
df[df$record_id ==18  & df$event_name > 30, 6] <- NA
df[df$record_id ==19  & df$event_name > 27, 6] <- NA
df[df$record_id ==20  & df$event_name > 26, 6] <- NA
df[df$record_id ==21  & df$event_name > 38, 6] <- NA
df[df$record_id ==22  & df$event_name > 40, 6] <- NA
df[df$record_id ==23  & df$event_name > 38, 6] <- NA
df[df$record_id ==24  & df$event_name > 27, 6] <- NA
df[df$record_id ==25  & df$event_name > 40, 6] <- NA
df[df$record_id ==26  & df$event_name > 21, 6] <- NA
df[df$record_id ==27  & df$event_name > 31, 6] <- NA
df[df$record_id ==28  & df$event_name > 40, 6] <- NA
df[df$record_id ==29  & df$event_name > 33, 6] <- NA
df[df$record_id ==30  & df$event_name > 28, 6] <- NA
df[df$record_id ==31  & df$event_name > 37, 6] <- NA
df[df$record_id ==32  & df$event_name > 35, 6] <- NA
df[df$record_id ==33  & df$event_name > 14, 6] <- NA
df[df$record_id ==34  & df$event_name > 35, 6] <- NA
df[df$record_id ==35  & df$event_name > 36, 6] <- NA
df[df$record_id ==36  & df$event_name > 38, 6] <- NA
df[df$record_id ==37  & df$event_name > 33, 6] <- NA
df[df$record_id ==38  & df$event_name > 35, 6] <- NA
df[df$record_id ==39  & df$event_name > 27, 6] <- NA
df[df$record_id ==40  & df$event_name > 31, 6] <- NA
df[df$record_id ==41  & df$event_name > 28, 6] <- NA
df[df$record_id ==42  & df$event_name > 27, 6] <- NA
df[df$record_id ==43  & df$event_name > 26, 6] <- NA
df[df$record_id ==44  & df$event_name > 31, 6] <- NA
df[df$record_id ==45  & df$event_name > 30, 6] <- NA
df[df$record_id ==46  & df$event_name > 31, 6] <- NA
df[df$record_id ==47  & df$event_name > 31, 6] <- NA
df[df$record_id ==48  & df$event_name > 27, 6] <- NA
df[df$record_id ==49  & df$event_name > 40, 6] <- NA
df[df$record_id ==50  & df$event_name > 27, 6] <- NA
df[df$record_id ==51  & df$event_name > 35, 6] <- NA
df[df$record_id ==52  & df$event_name > 34, 6] <- NA
df[df$record_id ==53  & df$event_name > 22, 6] <- NA
df[df$record_id ==54  & df$event_name > 27, 6] <- NA
df[df$record_id ==55  & df$event_name > 19, 6] <- NA
df[df$record_id ==56  & df$event_name > 22, 6] <- NA
df[df$record_id ==57  & df$event_name > 28, 6] <- NA
df[df$record_id ==58  & df$event_name > 40, 6] <- NA
df[df$record_id ==59  & df$event_name > 26, 6] <- NA
df[df$record_id ==60  & df$event_name > 30, 6] <- NA
df[df$record_id ==61  & df$event_name > 22, 6] <- NA
df[df$record_id ==62  & df$event_name > 15, 6] <- NA
df[df$record_id ==63  & df$event_name > 26, 6] <- NA
df[df$record_id ==64  & df$event_name > 22, 6] <- NA
df[df$record_id ==65  & df$event_name > 34, 6] <- NA
df[df$record_id ==66  & df$event_name > 20, 6] <- NA
df[df$record_id ==67  & df$event_name > 34, 6] <- NA
df[df$record_id ==68  & df$event_name > 22, 6] <- NA
df[df$record_id ==69  & df$event_name > 21, 6] <- NA
df[df$record_id ==70  & df$event_name > 36, 6] <- NA
df[df$record_id ==71  & df$event_name > 26, 6] <- NA
df[df$record_id ==72  & df$event_name > 26, 6] <- NA
df[df$record_id ==73  & df$event_name > 36, 6] <- NA
df[df$record_id ==74  & df$event_name > 36, 6] <- NA
df[df$record_id ==75  & df$event_name > 26, 6] <- NA
df[df$record_id ==76  & df$event_name > 36, 6] <- NA
df[df$record_id ==77  & df$event_name > 18, 6] <- NA
df[df$record_id ==78  & df$event_name > 23, 6] <- NA
df[df$record_id ==79  & df$event_name > 20, 6] <- NA

armt66 <- df

#to exclude participants with no data


aRMT %>% 
  group_by(record_id) %>% 
  count()


colSums(is.na(armt66))

# Chart -------------------------------------------------------------------

# new df = armt66 - with 66 participants
# new df2 = armt61 - without the 5 participants with no data
mergedaRMT[which(is.na(mergedaRMT$event_name1)), c("record_id")]    #checks if there are any record_id = NA

armt61 <- armt66 %>% 
  subset(!(record_id %in% c("20", "22","23",  "14", "34")))


aRMTcompletion <- armt66 %>%
  group_by(event_name) %>%
  mutate(week_avail = sum(comp, na.rm = TRUE)) %>%     #calculate how many participants have data available
  summarise_if(is.numeric, mean, na.rm = TRUE)


aRMTcomp61 <- armt61 %>%
  group_by(event_name) %>%
  mutate(week_avail = sum(comp, na.rm = TRUE)) %>%     #calculate how many participants have data available
  mutate(total_week = n())  %>%                  #out of the number of people that week
  mutate(prop.available = (week_avail/total_week)*100) %>%
  # mutate(is_outlier = total_week < 6) %>%        #to exclude weeks with < 5 people (1)
  # filter(is_outlier == FALSE) %>%                #(2)
  summarise_if(is.numeric, mean, na.rm = TRUE)


# write.csv(aRMTcompletion,"~/R/aRMTcompletion.csv", row.names = FALSE)   #export to excel



### THINC it ----------------------------------------------------------------

thinc <- read_excel("~/R/4. aRMT data.xlsx", sheet = "thincit", 
                    col_types = c("text", 
                                  "date", "date", "date", "date", "numeric", 
                                  "text", "text", "text", "text", "text", 
                                  "date", "date", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "date", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "date", "date", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "date", "date", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric"))

#create a completion variable = thinc.complete
thinc <- thinc %>% 
  mutate(thinc.comp = ifelse(!is.na("time_trials_complete"), 1, 0))

names(thinc)[names(thinc) == "participant_ID"] <- 'participant_id'       # rename


thinc_id <- merge(IDmap, thinc[, c("participant_id", "date_notified")], all.x = TRUE)

#create a completion variable = thinc.complete
thinc_id <- thinc_id %>%                 
  mutate(thinc.comp = ifelse(!is.na(date_notified), 1, 0))

thinc_id$diff_in_days = as.numeric(difftime(thinc_id$date_notified, thinc_id$date_assessment, units = "days")) 
thinc_id$event_name = ceiling((thinc_id$diff_in_days+1)/7)     # per week
thinc_id$event_month = ceiling((thinc_id$diff_in_days+1)/28)   # per month

#create a dataframe for all weeks and all participants
event_month <- rep(1:10, times=66)
record_id <- rep(14:79, each=10)
allmonths <- as.data.frame(cbind(record_id, event_month))

merged <- merge(allmonths, IDmap[, c("record_id","participant_id")], by = "record_id", all.x = TRUE)


#new data frame with all months = thinc10
thinc10 <- merge(merged, thinc_id, all.x = TRUE)


# we check how many unique values of weeks there are (a and b) com --------
a <- thinc10 %>%         
  group_by(record_id) %>%
  summarise(a = length(unique(event_month)))

c <- thinc10 %>%
  group_by(record_id) %>%
  summarise(count = n())

# View(merge(c,a))


eventcheck <- merge(c,a)
eventcheck$d <- eventcheck$count - eventcheck$a # creates a column showing which Ps have more events than unique week numbers
eventcheck[which(eventcheck$d != 0),]   # calls the rows with duplicate week numbers

#there are many instances of participants completing the thinc it multiple times per month
#so this summarises all the participants by month - is there at least 1 data point per person, per month?
thincbymonth <- thinc10 %>%
  group_by(record_id, event_month) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

thincbymonth[thincbymonth$thinc.comp == "NaN", "thinc.comp"] <- NA

# write.csv(thincbymonth,"~/R/thincbymonth.csv", row.names = FALSE)   #export to excel

thincbymonth <- thincbymonth %>%                 
  mutate(thinc.comp = ifelse(!is.na(thinc.comp), 1, 0))

df <- thincbymonth


# Study months only -------------------------------------------------------

# x <- study_months$study_months
# 
# for (i in 14:79) {
#   for (j in x) {
#     df[df$record_id == i  & df$event_month > x, 4] <- NA
#   }
# }
# df

df[df$record_id ==14  & df$event_month > 10, "thinc.comp"] <- NA
df[df$record_id ==15  & df$event_month > 7, "thinc.comp"] <- NA
df[df$record_id ==16  & df$event_month > 8, "thinc.comp"] <- NA
df[df$record_id ==17  & df$event_month > 7, "thinc.comp"] <- NA
df[df$record_id ==18  & df$event_month > 7, "thinc.comp"] <- NA
df[df$record_id ==19  & df$event_month > 6, "thinc.comp"] <- NA
df[df$record_id ==20  & df$event_month > 6, "thinc.comp"] <- NA
df[df$record_id ==21  & df$event_month > 9, "thinc.comp"] <- NA
df[df$record_id ==22  & df$event_month > 10, "thinc.comp"] <- NA
df[df$record_id ==23  & df$event_month > 9, "thinc.comp"] <- NA
df[df$record_id ==24  & df$event_month > 6, "thinc.comp"] <- NA
df[df$record_id ==25  & df$event_month > 10, "thinc.comp"] <- NA
df[df$record_id ==26  & df$event_month > 5, "thinc.comp"] <- NA
df[df$record_id ==27  & df$event_month > 7, "thinc.comp"] <- NA
df[df$record_id ==28  & df$event_month > 10, "thinc.comp"] <- NA
df[df$record_id ==29  & df$event_month > 8, "thinc.comp"] <- NA
df[df$record_id ==30  & df$event_month > 7, "thinc.comp"] <- NA
df[df$record_id ==31  & df$event_month > 9, "thinc.comp"] <- NA
df[df$record_id ==32  & df$event_month > 8, "thinc.comp"] <- NA
df[df$record_id ==33  & df$event_month > 3, "thinc.comp"] <- NA
df[df$record_id ==34  & df$event_month > 8, "thinc.comp"] <- NA
df[df$record_id ==35  & df$event_month > 9, "thinc.comp"] <- NA
df[df$record_id ==36  & df$event_month > 9, "thinc.comp"] <- NA
df[df$record_id ==37  & df$event_month > 8, "thinc.comp"] <- NA
df[df$record_id ==38  & df$event_month > 8, "thinc.comp"] <- NA
df[df$record_id ==39  & df$event_month > 6, "thinc.comp"] <- NA
df[df$record_id ==40  & df$event_month > 7, "thinc.comp"] <- NA
df[df$record_id ==41  & df$event_month > 7, "thinc.comp"] <- NA
df[df$record_id ==42  & df$event_month > 6, "thinc.comp"] <- NA
df[df$record_id ==43  & df$event_month > 6, "thinc.comp"] <- NA
df[df$record_id ==44  & df$event_month > 7, "thinc.comp"] <- NA
df[df$record_id ==45  & df$event_month > 7, "thinc.comp"] <- NA
df[df$record_id ==46  & df$event_month > 7, "thinc.comp"] <- NA
df[df$record_id ==47  & df$event_month > 7, "thinc.comp"] <- NA
df[df$record_id ==48  & df$event_month > 6, "thinc.comp"] <- NA
df[df$record_id ==49  & df$event_month > 10, "thinc.comp"] <- NA
df[df$record_id ==50  & df$event_month > 6, "thinc.comp"] <- NA
df[df$record_id ==51  & df$event_month > 8, "thinc.comp"] <- NA
df[df$record_id ==52  & df$event_month > 8, "thinc.comp"] <- NA
df[df$record_id ==53  & df$event_month > 5, "thinc.comp"] <- NA
df[df$record_id ==54  & df$event_month > 6, "thinc.comp"] <- NA
df[df$record_id ==55  & df$event_month > 4, "thinc.comp"] <- NA
df[df$record_id ==56  & df$event_month > 5, "thinc.comp"] <- NA
df[df$record_id ==57  & df$event_month > 7, "thinc.comp"] <- NA
df[df$record_id ==58  & df$event_month > 10, "thinc.comp"] <- NA
df[df$record_id ==59  & df$event_month > 6, "thinc.comp"] <- NA
df[df$record_id ==60  & df$event_month > 7, "thinc.comp"] <- NA
df[df$record_id ==61  & df$event_month > 5, "thinc.comp"] <- NA
df[df$record_id ==62  & df$event_month > 3, "thinc.comp"] <- NA
df[df$record_id ==63  & df$event_month > 6, "thinc.comp"] <- NA
df[df$record_id ==64  & df$event_month > 5, "thinc.comp"] <- NA
df[df$record_id ==65  & df$event_month > 8, "thinc.comp"] <- NA
df[df$record_id ==66  & df$event_month > 5, "thinc.comp"] <- NA
df[df$record_id ==67  & df$event_month > 8, "thinc.comp"] <- NA
df[df$record_id ==68  & df$event_month > 5, "thinc.comp"] <- NA
df[df$record_id ==69  & df$event_month > 5, "thinc.comp"] <- NA
df[df$record_id ==70  & df$event_month > 9, "thinc.comp"] <- NA
df[df$record_id ==71  & df$event_month > 6, "thinc.comp"] <- NA
df[df$record_id ==72  & df$event_month > 6, "thinc.comp"] <- NA
df[df$record_id ==73  & df$event_month > 9, "thinc.comp"] <- NA
df[df$record_id ==74  & df$event_month > 8, "thinc.comp"] <- NA
df[df$record_id ==75  & df$event_month > 6, "thinc.comp"] <- NA
df[df$record_id ==76  & df$event_month > 9, "thinc.comp"] <- NA
df[df$record_id ==77  & df$event_month > 4, "thinc.comp"] <- NA
df[df$record_id ==78  & df$event_month > 5, "thinc.comp"] <- NA
df[df$record_id ==79  & df$event_month > 5, "thinc.comp"] <- NA


# THINC it end  --------------------------------------------------------------------

thinc66 <- df

thinc66month <- thinc66 %>%
  group_by(event_month) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) 


#get rid of those who never did it  - no data ever
nothinc <- thinc_id[which(is.na(thinc_id$date_notified)), c("record_id")]    # checks if there are any record_id = NA (those who never did THINC it)
# and adds it to 'nothinc' 
thinc_53 <- thinc66 %>%                                                      # then creates df without 'nothinc' 
  subset(!(record_id %in% nothinc))

month53 <- thinc_53 %>%
  group_by(event_month) %>%
  mutate(week_avail = sum(thinc.comp, na.rm = TRUE)) %>%     #calculate how many participants have data available
  mutate(total_week = n())  %>%                                #out of the number of people that week
  mutate(prop.available = (week_avail/total_week)*100) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE) 

# write.csv(month53,"~/R/thincit53.csv", row.names = FALSE)   #export to excel

### Speech ----------------------------------------------------------------------------

speech <- read_excel("~/R/4. aRMT data.xlsx", 
                     sheet = "speech", col_types = c("text", 
                                                     "date", "skip", "skip", "date", "date", 
                                                     "skip", "skip", "numeric", "date", 
                                                     "skip", "skip", "numeric"))

speech[speech$Scripted_Duration == "NaN", "Scripted_Duration"] <- NA
speech[speech$Free_Duration == "NaN", "Free_Duration"] <- NA



#create a completion variable = speech.complete
speech <- speech %>% 
  mutate(either_comp = ifelse(is.na(Scripted_Duration) & is.na(Free_Duration), 0, 1)) %>% 
  mutate(both_comp = ifelse(is.na(Scripted_Duration) | is.na(Free_Duration), 0, 1))

names(speech)[names(speech) == "Participant_ID"] <- 'participant_id'     # rename

speech_allids <- merge(speech, IDmap, all.x = TRUE)

speech_id <- speech_allids %>% 
  subset(!(participant_id %in% c('f5978923-cef2-4eeb-a49c-7d79be4b53eb',
                                 '5786af5e-99e3-4f78-848a-d3b67b8eb7ed'))) %>% 
  drop_na(record_id)

colSums(is.na(speech_id))


speech_id$diff_in_days = as.numeric(difftime(speech_id$`Prepare date`, speech_id$date_assessment, units = "days")) 
speech_id$event_name = ceiling((speech_id$diff_in_days)/7)     # per week
speech_id$event_fortnight = ceiling((speech_id$diff_in_days+1)/14)   # per fortnight

#create a dataframe for all weeks and all participants
event_fortnight <- rep(1:20, times=66)
record_id <- rep(14:79, each=20)
allfortnight <- as.data.frame(cbind(record_id, event_fortnight))

merged <- merge(allfortnight, IDmap[, c("record_id","participant_id")], by = "record_id", all.x = TRUE)

#new data frame with all months = speech20
speech20 <- merge(speech_id, merged, all.x = TRUE, all.y = TRUE)

colSums(is.na(speech))

# we check how many unique values of weeks there are (a and b) com --------
a <- speech20 %>%         
  group_by(record_id) %>%
  summarise(a = length(unique(event_fortnight)))
# b <- speech20 %>%
#   group_by(record_id) %>%
#   summarise(b = length(unique(event_fortnight1)))
c <- speech20 %>%
  group_by(record_id) %>%
  summarise(count = n())

# View(merge(c,a))

# write.csv(speech66,"~/R/speech666.csv", row.names = FALSE)   #export to excel

eventcheck <- merge(c,a)
eventcheck$d <- eventcheck$count - eventcheck$a # creates a column showing which Ps have more events than unique week numbers
eventcheck[which(eventcheck$d != 0),]   # calls the rows with duplicate week numbers


speech20[is.na(speech20$diff_in_days), "diff_in_days"] <- 0  #I have to change the NAs in this row to 0 otherwise the NA assignment below won't work

# #delete (NA)  duplicate weeks ----
speech20[speech20$record_id == 35  & speech20$diff_in_days == 33, "event_fortnight"] <- NA
speech20[speech20$record_id == 35  & speech20$diff_in_days == 30, "event_fortnight"] <- NA
speech20[speech20$record_id == 42  & speech20$diff_in_days == 117, "event_fortnight"] <- NA
speech20[speech20$record_id == 44  & speech20$diff_in_days == 13, "event_fortnight"] <- NA
speech20[speech20$record_id == 44  & speech20$diff_in_days == 57, "event_fortnight"] <- NA
speech20[speech20$record_id == 52  & speech20$diff_in_days == 60, "event_fortnight"] <- NA
speech20[speech20$record_id == 55  & speech20$diff_in_days == 28, "event_fortnight"] <- NA
speech20[speech20$record_id == 58  & speech20$diff_in_days == 12, "event_fortnight"] <- NA
speech20[speech20$record_id == 58  & speech20$diff_in_days == 41, "event_fortnight"] <- NA
speech20[speech20$record_id == 58  & speech20$diff_in_days == 86, "event_fortnight"] <- NA
speech20[speech20$record_id == 58  & speech20$diff_in_days == 131, "event_fortnight"] <- NA
speech20[speech20$record_id == 74  & speech20$diff_in_days == 12, "event_fortnight"] <- NA
speech20[speech20$record_id == 74  & speech20$diff_in_days == 83, "event_fortnight"] <- NA

nodups <- subset(speech20, !is.na(event_fortnight))    #gets rid of participants with duplicate weeks as prepared above

# transform NAs from the 2 completion variables (either_comp + both_comp) to = 0

nodups[is.na(nodups$either_comp), c("either_comp", "both_comp")] <- 0
nodups <- nodups %>%
  mutate(event_name = event_fortnight*2)

# Study days only ---------------------------------------------------------

df <- nodups

### for each participant: NA after last day in study.
df[df$record_id ==14  & df$event_name > 40, c("either_comp", "both_comp")] <- NA
df[df$record_id ==15  & df$event_name > 31, c("either_comp", "both_comp")] <- NA
df[df$record_id ==16  & df$event_name > 32, c("either_comp", "both_comp")] <- NA
df[df$record_id ==17  & df$event_name > 30, c("either_comp", "both_comp")] <- NA
df[df$record_id ==18  & df$event_name > 30, c("either_comp", "both_comp")] <- NA
df[df$record_id ==19  & df$event_name > 27, c("either_comp", "both_comp")] <- NA
df[df$record_id ==20  & df$event_name > 26, c("either_comp", "both_comp")] <- NA
df[df$record_id ==21  & df$event_name > 38, c("either_comp", "both_comp")] <- NA
df[df$record_id ==22  & df$event_name > 40, c("either_comp", "both_comp")] <- NA
df[df$record_id ==23  & df$event_name > 38, c("either_comp", "both_comp")] <- NA
df[df$record_id ==24  & df$event_name > 27, c("either_comp", "both_comp")] <- NA
df[df$record_id ==25  & df$event_name > 40, c("either_comp", "both_comp")] <- NA
df[df$record_id ==26  & df$event_name > 21, c("either_comp", "both_comp")] <- NA
df[df$record_id ==27  & df$event_name > 31, c("either_comp", "both_comp")] <- NA
df[df$record_id ==28  & df$event_name > 40, c("either_comp", "both_comp")] <- NA
df[df$record_id ==29  & df$event_name > 33, c("either_comp", "both_comp")] <- NA
df[df$record_id ==30  & df$event_name > 28, c("either_comp", "both_comp")] <- NA
df[df$record_id ==31  & df$event_name > 37, c("either_comp", "both_comp")] <- NA
df[df$record_id ==32  & df$event_name > 35, c("either_comp", "both_comp")] <- NA
df[df$record_id ==33  & df$event_name > 14, c("either_comp", "both_comp")] <- NA
df[df$record_id ==34  & df$event_name > 35, c("either_comp", "both_comp")] <- NA
df[df$record_id ==35  & df$event_name > 36, c("either_comp", "both_comp")] <- NA
df[df$record_id ==36  & df$event_name > 38, c("either_comp", "both_comp")] <- NA
df[df$record_id ==37  & df$event_name > 33, c("either_comp", "both_comp")] <- NA
df[df$record_id ==38  & df$event_name > 35, c("either_comp", "both_comp")] <- NA
df[df$record_id ==39  & df$event_name > 27, c("either_comp", "both_comp")] <- NA
df[df$record_id ==40  & df$event_name > 31, c("either_comp", "both_comp")] <- NA
df[df$record_id ==41  & df$event_name > 28, c("either_comp", "both_comp")] <- NA
df[df$record_id ==42  & df$event_name > 27, c("either_comp", "both_comp")] <- NA
df[df$record_id ==43  & df$event_name > 26, c("either_comp", "both_comp")] <- NA
df[df$record_id ==44  & df$event_name > 31, c("either_comp", "both_comp")] <- NA
df[df$record_id ==45  & df$event_name > 30, c("either_comp", "both_comp")] <- NA
df[df$record_id ==46  & df$event_name > 31, c("either_comp", "both_comp")] <- NA
df[df$record_id ==47  & df$event_name > 31, c("either_comp", "both_comp")] <- NA
df[df$record_id ==48  & df$event_name > 27, c("either_comp", "both_comp")] <- NA
df[df$record_id ==49  & df$event_name > 40, c("either_comp", "both_comp")] <- NA
df[df$record_id ==50  & df$event_name > 27, c("either_comp", "both_comp")] <- NA
df[df$record_id ==51  & df$event_name > 35, c("either_comp", "both_comp")] <- NA
df[df$record_id ==52  & df$event_name > 34, c("either_comp", "both_comp")] <- NA
df[df$record_id ==53  & df$event_name > 22, c("either_comp", "both_comp")] <- NA
df[df$record_id ==54  & df$event_name > 27, c("either_comp", "both_comp")] <- NA
df[df$record_id ==55  & df$event_name > 19, c("either_comp", "both_comp")] <- NA
df[df$record_id ==56  & df$event_name > 22, c("either_comp", "both_comp")] <- NA
df[df$record_id ==57  & df$event_name > 28, c("either_comp", "both_comp")] <- NA
df[df$record_id ==58  & df$event_name > 40, c("either_comp", "both_comp")] <- NA
df[df$record_id ==59  & df$event_name > 26, c("either_comp", "both_comp")] <- NA
df[df$record_id ==60  & df$event_name > 30, c("either_comp", "both_comp")] <- NA
df[df$record_id ==61  & df$event_name > 22, c("either_comp", "both_comp")] <- NA
df[df$record_id ==62  & df$event_name > 15, c("either_comp", "both_comp")] <- NA
df[df$record_id ==63  & df$event_name > 26, c("either_comp", "both_comp")] <- NA
df[df$record_id ==64  & df$event_name > 22, c("either_comp", "both_comp")] <- NA
df[df$record_id ==65  & df$event_name > 34, c("either_comp", "both_comp")] <- NA
df[df$record_id ==66  & df$event_name > 20, c("either_comp", "both_comp")] <- NA
df[df$record_id ==67  & df$event_name > 34, c("either_comp", "both_comp")] <- NA
df[df$record_id ==68  & df$event_name > 22, c("either_comp", "both_comp")] <- NA
df[df$record_id ==69  & df$event_name > 21, c("either_comp", "both_comp")] <- NA
df[df$record_id ==70  & df$event_name > 36, c("either_comp", "both_comp")] <- NA
df[df$record_id ==71  & df$event_name > 26, c("either_comp", "both_comp")] <- NA
df[df$record_id ==72  & df$event_name > 26, c("either_comp", "both_comp")] <- NA
df[df$record_id ==73  & df$event_name > 36, c("either_comp", "both_comp")] <- NA
df[df$record_id ==74  & df$event_name > 36, c("either_comp", "both_comp")] <- NA
df[df$record_id ==75  & df$event_name > 26, c("either_comp", "both_comp")] <- NA
df[df$record_id ==76  & df$event_name > 36, c("either_comp", "both_comp")] <- NA
df[df$record_id ==77  & df$event_name > 18, c("either_comp", "both_comp")] <- NA
df[df$record_id ==78  & df$event_name > 23, c("either_comp", "both_comp")] <- NA
df[df$record_id ==79  & df$event_name > 20, c("either_comp", "both_comp")] <- NA

speech66 <- df

colSums(is.na(speech66))

# calculation with speech66

speechsum66 <- speech66 %>%
  group_by(event_fortnight) %>%
  mutate(week_avail = sum(either_comp, na.rm = TRUE)) %>%     #calculate how many participants have data available
  summarise_if(is.numeric, mean, na.rm = TRUE)



# aRMT end ----


names(armt66 )[names(armt66 ) == "comp"] <- 'aRMT_complete'     # rename

aRMT <- armt66 %>%
  select(record_id, event_name, aRMT_complete)

# View(aRMT)

## combined aRMT with speech and thinc it

# convert event month and event fortnight into event_name
thincrep <- thinc66[rep(seq_len(nrow(thinc66)), each = 4), ]  
speechrep <- speech66[rep(seq_len(nrow(speech66)), each = 2), ]  

thincweek <- thincrep %>%
  group_by(record_id) %>%
  mutate(event_name = row_number()) 

speechweek <- speechrep %>%
  group_by(record_id) %>%
  mutate(event_name = row_number())

merged_t_s <- merge(thincweek[,c("record_id", "thinc.comp", "event_name")], speechweek[, c("record_id", "either_comp", "event_name")])

fullaRMT <- merge(merged_t_s, armt66[,c("record_id", "event_name", "aRMT_complete")])

fullaRMTcomp <- fullaRMT %>%
  mutate(anyaRMT_comp = ifelse(is.na(thinc.comp)| is.na(either_comp) | is.na(aRMT_complete), NA,
                               ifelse(thinc.comp == 1 | either_comp == 1 | aRMT_complete == 1,1,0)))

#### 4. Fitbit -----------------------------------------------------------
X6_fitbit <- read_excel("~/R/6. Fitbit data.xlsx", 
                        sheet = "HM_study day")

cols_to_drop <- c("study days")       #get rid of variable "study days"
X6_fitbit = X6_fitbit[,!(names(X6_fitbit) %in% cols_to_drop)]

fitbit_hr <- melt(data = X6_fitbit,               # switch to long format
                  id.vars = c("record_id", "SUBJECT_ID"),
                  variable.name = "day",
                  value.name = "HR_missing")

fitbit_hr <- fitbit_hr %>%
  drop_na(record_id)

fitbit_hr$day <- as.numeric(as.factor(fitbit_hr$day))
fitbit_hr$HR_missing <- as.numeric(as.character(fitbit_hr$HR_missing))


weekly_fitbit_hr <- fitbit_hr %>%                      
  mutate(event_name = ceiling(day / 7)) %>%            #create variable "week"
  group_by(record_id, event_name) %>%             #create variable "week"
  summarise_if(is.numeric, mean, na.rm = TRUE)    #mean n of hours per day / sleep = mean n of days

cols_to_drop <- c("day")                               #get rid of variable "day"
weekly_fitbit_hr = weekly_fitbit_hr[,!(names(weekly_fitbit_hr) %in% cols_to_drop)]
weekly_fitbit_hr <- subset(weekly_fitbit_hr, event_name < 41)    #filter weeks over 40

conv_weekly_hr <- weekly_fitbit_hr %>%
  mutate(hr_perc = 1-(HR_missing/100))


conv_weekly_hr <- conv_weekly_hr %>%
  select(record_id, event_name, hr_perc)

# View(conv_weekly_hr)



# COMBINE ALL 4 TYPES OF DATA ---------------------------------------------

# activemerge <- merge(aRMT, fu_redcap)
# passivemerge <- merge(conv_weekly_hr, conv_weekly_pRMT, all.x = TRUE)
# fourdata <- merge(activemerge, passivemerge)


#test with new aRMT variables (this one includes speech and thinc it, the above does not)
testactivemerge <- merge(fullaRMTcomp[,c("record_id", "event_name", "anyaRMT_comp")], fu_redcap)
passivemerge <- merge(conv_weekly_hr, conv_weekly_pRMT, all.x = TRUE)
fourdata <- merge(testactivemerge, passivemerge)


# add study end -----------------------------------------------------------

X2_Demographic_Table <- read_excel("~/R/2. Demographic Table.xlsx",
                                   sheet = 3)
subset_studyend <- X2_Demographic_Table %>%
  select(record_ID, `Ended by`)
names(subset_studyend)[names(subset_studyend) == "record_ID"] <- 'record_id'     # rename
names(subset_studyend)[names(subset_studyend) == "Ended by"] <- 'ended_by'       # rename

subset_studyend <- subset_studyend %>%                                   # convert Ended by to factor (categorical) with 3 levels
  mutate(ended_by = factor(as.character(ended_by),
                           levels = c("lost to follow up", "withdrawal", "completed"),
                           labels = c("lost to follow up", "withdrawal", "completed")))

# View(subset_studyend)


#merge study end df with data availability - Including sleep!
fourdata <- merge(fourdata, subset_studyend,all.x = TRUE)

glimpse(fourdata)


# STUDY DAYS ONLY ---------------------------------------------------------

### for all participants: change all NAs to 0 - because there is 0 data on those cells
df <- fourdata

df[is.na(df)]<- 0               #change NAs to 0 - Warning message: tx_status doesn't change (because non-numeric variable?)

which(colSums(is.na(df))>0)   # where are the NAs? gives you name and index of column

### for each participant: NA after last day in study.
df[df$record_id ==14  & df$event_name > 40, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==15  & df$event_name > 31, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==16  & df$event_name > 32, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==17  & df$event_name > 30, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==18  & df$event_name > 30, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==19  & df$event_name > 27, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==20  & df$event_name > 26, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==21  & df$event_name > 38, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==22  & df$event_name > 40, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==23  & df$event_name > 38, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==24  & df$event_name > 27, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==25  & df$event_name > 40, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==26  & df$event_name > 21, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==27  & df$event_name > 31, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==28  & df$event_name > 40, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==29  & df$event_name > 33, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==30  & df$event_name > 28, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==31  & df$event_name > 37, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==32  & df$event_name > 35, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==33  & df$event_name > 14, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==34  & df$event_name > 35, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==35  & df$event_name > 36, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==36  & df$event_name > 38, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==37  & df$event_name > 33, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==38  & df$event_name > 35, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==39  & df$event_name > 27, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==40  & df$event_name > 31, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==41  & df$event_name > 28, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==42  & df$event_name > 27, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==43  & df$event_name > 26, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==44  & df$event_name > 31, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==45  & df$event_name > 30, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==46  & df$event_name > 31, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==47  & df$event_name > 31, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==48  & df$event_name > 27, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==49  & df$event_name > 40, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==50  & df$event_name > 27, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==51  & df$event_name > 35, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==52  & df$event_name > 34, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==53  & df$event_name > 22, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==54  & df$event_name > 27, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==55  & df$event_name > 19, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==56  & df$event_name > 22, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==57  & df$event_name > 28, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==58  & df$event_name > 40, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==59  & df$event_name > 26, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==60  & df$event_name > 30, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==61  & df$event_name > 22, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==62  & df$event_name > 15, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==63  & df$event_name > 26, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==64  & df$event_name > 22, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==65  & df$event_name > 34, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==66  & df$event_name > 20, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==67  & df$event_name > 34, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==68  & df$event_name > 22, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==69  & df$event_name > 21, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==70  & df$event_name > 36, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==71  & df$event_name > 26, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==72  & df$event_name > 26, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==73  & df$event_name > 36, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==74  & df$event_name > 36, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==75  & df$event_name > 26, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==76  & df$event_name > 36, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==77  & df$event_name > 18, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==78  & df$event_name > 23, c(3, 4, 5, 6, 7, 8)] <- NA
df[df$record_id ==79  & df$event_name > 20, c(3, 4, 5, 6, 7, 8)] <- NA

df[df$event_name == 28, 4] <-NA

fourdata_clean <- df

which(colSums(is.na(fourdata_clean))>0)
colSums((is.na(fourdata_clean)))


# Set to long format ------------------------------------------------------

# set df as long - subset of variables
fourdata_long <- melt(fourdata_clean, id.vars = c("record_id", "ended_by", "tx_status", "event_name"), 
                      variable.name = "stream",
                      value.name = "availability")

glimpse(fourdata_long)

# write.csv(fourdata_long,"~/R/fourdatalong.csv", row.names = FALSE)   #export to excel

# x <-  fourdata_long %>% 
#   subset(stream %in% "redcap_binary") %>% 
#   group_by(event_name) %>% 
#   summarise(av = sum(availability, na.rm = TRUE))
#   # summarise(av2 = mean(availability, na.rm = TRUE))



# PLOTS -------------------------------------------------------------------

fourdata_long <- fourdata_long[fourdata_long$stream != "sleep_binary", 1:6]   #get rid of sleep
fourdata_long$stream <- droplevels(fourdata_long$stream)                      #drop levels
levels(fourdata_long$stream)                                                  #check it worked

# PLOT 1. 

data_availability <- fourdata_long %>%
  group_by(event_name, stream) %>%
  summarize(availability = mean(availability, na.rm = TRUE))


plot_allstreams <- ggplot(data_availability, aes(x=event_name, y=availability, color=stream)) +
  scale_y_continuous(name = "Availability") +
  scale_x_continuous(name = "Week") +
  geom_line(size = 1.3) 
plot_allstreams


# PLOT 2. difference in data availability by study end

# plot a mean line of data availability of average percentage complete x study end
#x axis is event_name, y is percentage/mean

ended_by <- fourdata_long %>%
  group_by(event_name, ended_by, stream) %>%
  summarize(availability = mean(availability, na.rm = TRUE))

ended_by <- fourdata_long %>%
  group_by(event_name, ended_by, stream) %>%
  drop_na() %>%
  mutate(week_avail = sum(availability)) %>%     #calculate how many participants have data available
  mutate(total_week = n())  %>%                  #out of the number of people that week
  mutate(prop.available = (week_avail/total_week)*100) %>%
  mutate(is_outlier = total_week < 6) %>%        #to exclude weeks with < 5 people (1)
  filter(is_outlier == FALSE) %>%                #(2)
  summarise_if(is.numeric, mean, na.rm = TRUE)


plot_endedby <- ggplot(ended_by, aes(x=event_name, y=availability, color=ended_by)) +
  geom_line(size = 1) +
  # geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~stream)
plot_endedby



# PLOT 3. 
# data availability of average percentage complete x treatment status
#x axis is event_name, y is percentage/mean

tx_status <- fourdata_long %>%
  group_by(event_name, tx_status, stream) %>%
  drop_na() %>%
  summarize(availability = mean(availability, na.rm = TRUE))

tx_status <- fourdata_long %>%
  group_by(event_name, tx_status, stream) %>%
  drop_na() %>%
  mutate(week_avail = sum(availability)) %>%     #calculate how many participants have data available
  mutate(total_week = n())  %>%                  #out of the number of people that week
  mutate(prop.available = (week_avail/total_week)*100) %>%
  mutate(is_outlier = total_week < 6) %>%        #to exclude weeks with < 5 people (1)
  filter(is_outlier == FALSE) %>%                #(2)
  summarise_if(is.numeric, mean, na.rm = TRUE)


plot_txstatus <- ggplot(tx_status, aes(x=event_name, y=availability, color=tx_status)) +
  geom_line(size=1) +
  # geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~stream)
plot_txstatus


# PLOT 4. PRE
# data availability for pre vs Treatment only
#x axis is event_name, y is percentage/mean
View(tx_status_pre)

tx_status_pre <- tx_status[tx_status$event_name == "3"| tx_status$event_name == "4" |tx_status$event_name == "5" |tx_status$event_name == "6" |
                             tx_status$event_name == "7" | tx_status$event_name == "8" , 1:8]
tx_status_pre <- tx_status_pre[tx_status_pre$tx_status == "tx" | tx_status_pre$tx_status == "pre_tx" , 1:8]

plot_pre_tx <- ggplot(tx_status_pre, aes(x=event_name, y=availability, color=tx_status)) +
  geom_line(size=1) +
  scale_color_manual(values = c("tx" = "orange",
                                "pre_tx" ="steelblue")) +
  # geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~stream)
plot_pre_tx

# PLOT 5. POST
# data availability for post vs Treatment only
#x axis is event_name, y is percentage/mean

tx_status_post <- tx_status[tx_status$event_name == "8" | tx_status$event_name == "9" | tx_status$event_name == "10" |
                              tx_status$event_name == "11" |tx_status$event_name == "12" |tx_status$event_name == "13" | tx_status$event_name == "14" |
                              tx_status$event_name == "15" | tx_status$event_name == "16" |tx_status$event_name == "17" |tx_status$event_name == "18" |
                              tx_status$event_name == "19" | tx_status$event_name == "20" |tx_status$event_name == "21" |tx_status$event_name == "22" |
                              tx_status$event_name == "23" | tx_status$event_name == "24", 1:8]

tx_status_post <- tx_status_post[tx_status_post$tx_status == "post_tx" | tx_status_post$tx_status == "tx" , 1:8]

plot_post_tx <- ggplot(tx_status_post, aes(x=event_name, y=availability, color=tx_status)) +
  geom_line(size=1) +
  scale_color_manual(values = c("tx" = "orange",
                                "post_tx" ="steelblue")) +
  # geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~stream)
plot_post_tx

# COMPARE TREATMENT STATUS ------------------------------------------------

# 1. PRE-TREATMENT vs TREATMENT -----
#table variable 1: tx ( Pre vs Tx)
#variable 2: Availability (Y vs N)

#table 1 for overall availability:
filtered <- fourdata_clean %>%
  filter(tx_status == "pre_tx" | tx_status == "tx") %>%
  filter(event_name == "3" | event_name == "4" |event_name == "5" |event_name == "6" |event_name == "7" | event_name == "8") %>%
  droplevels()


###pRMT and Fitbit - semi continuous variables
filtered_binary <- filtered %>%                #convert passive data to binary codes
  mutate(hr_binary = ifelse(hr_perc > 0.49, 1, 0)) %>%
  mutate(pRMT_binary = ifelse(any_pRMT > 0.49, 1, 0))

aRMTtable <- table( filtered$anyaRMT_comp, filtered$tx_status)
fitbitable <- table( filtered_binary$hr_binary, filtered_binary$tx_status)
aRMTtable
fitbitable

prop.table(aRMTtable, margin = 2)*100
prop.table(fitbitable, margin = 2)*100

mosaicplot(redcaptable)
mosaicplot(aRMTtable)

chisq.test(filtered$redcap_binary, filtered$tx_status)
chisq.test(filtered_binary$anyaRMT_comp, filtered_binary$tx_status)  # significant difference:
#overall,  being in treatment improved engagement for those in active tasks, but not online tasks
#for overall OR:
fisher.test(filtered_binary$anyaRMT_comp, filtered_binary$tx_status)

#multinomial logistic regression
# DV : data availability - e.g. redcap_binary
# IV : (1) tx status, (2) event_name (time)

# install.packages("nnet")
# install.packages("jmv", type="binary")

library(nnet)
library(jmv)

#first, get descriptive data
with(filtered_binary, table(tx_status, anyaRMT_comp))
with(filtered_binary, table(event_name, hr_binary))

#we run our model using glm
filtered$anyaRMT_comp <- as.factor(filtered$anyaRMT_comp)
filtered$tx_status <- as.factor(filtered$tx_status)
filtered$event_name <- as.factor(filtered$event_name)

glm.fit <- glm(hr_binary ~ tx_status + event_name, data = filtered_binary, family = binomial)
summary(glm.fit)
#estimate for event name is negative so people at later stages were less likely to have data
#positive sign for tx status shows people in treatment were more likely to have data

# obtain ODDS RATIOS and CIs extract the coefficients from the model and exponentiate
ci <- confint(glm.fit)
exp(cbind(OR = coef(glm.fit), ci))
# for those in treatment the odds of having aRMT data is 3.09 times that of those outside of treatment, holding constant all other variables.

#check model fit
#or check goodness of fit
# A statistically significant result (i.e., p < .05) indicates that the model does not fit the data well
head(predict(glm.fit), 30)
chisq.test(filtered$redcap_binary,predict(glm.fit))

glm.fit <- glm(pRMT_binary ~ tx_status + event_name, data = filtered_binary, family = binomial)
summary(glm.fit)

ci <- confint(glm.fit)
exp(cbind(OR = coef(glm.fit), ci))


## 2. TREATMENT vs POST-TREATMENT ------

#table variable 1: tx ( Post vs Tx)
#variable 2: Availability (Y vs N)

table(fourdata_clean$event_name, fourdata_clean$tx_status) #post tx weeks = 8 - 24

#table 1 for overall availability:
filtered_post <- fourdata_clean %>%
  filter(tx_status == "post_tx" | tx_status == "tx") %>%
  filter(event_name == "8" | event_name == "9" | event_name == "10" |event_name == "11" |event_name == "12" |event_name == "13" | event_name == "14" |
           event_name == "15" | event_name == "16" |event_name == "17" |event_name == "18" |event_name == "19" | event_name == "20"
         |event_name == "21" |event_name == "22" |event_name == "23" | event_name == "24") %>%
  droplevels()


#multinomial logistic regression
# DV : data availability - e.g. redcap_binary
# IV : (1) tx status, (2) event_name (time)

#first, get descriptive data
with(filtered_post, table(tx_status, redcap_binary))
with(filtered_post, table(event_name, redcap_binary))

#we run our model using glm
filtered_post$anyaRMT_comp <- as.factor(filtered_post$anyaRMT_comp)
filtered_post$tx_status <- as.factor(filtered_post$tx_status)
filtered_post$event_name <- as.factor(filtered_post$event_name)

filteredpost_binary <- filtered_post %>%                #convert passive data to binary codes
  mutate(hr_binary = ifelse(hr_perc > 0.49, 1, 0)) %>%
  mutate(pRMT_binary = ifelse(any_pRMT > 0.49, 1, 0))

#estimate for event name is negative so people at later stages were less likely to have data
#positive sign for tx status shows people in treatment were more likely to have data
glm.fit <- glm(hr_binary ~ tx_status + event_name, data = filteredpost_binary, family = binomial)
summary(glm.fit)

# obtain ODDS RATIOS and CIs extract the coefficients from the model and exponentiate
ci <- confint(glm.fit)
exp(cbind(OR = coef(glm.fit), ci))


chisq.test(filtered_post$redcap_binary, filtered_post$tx_status)
chisq.test(filteredpost_binary$hr_binary, filteredpost_binary$tx_status)
#for overall OR:
fisher.test(filteredpost_binary$hr_binary, filteredpost_binary$tx_status)


#proportion tables
postredcaptable <- table( filtered_post$redcap_binary, filtered_post$tx_status)
postaRMTtable <- table( filtered_post$anyaRMT_comp, filtered_post$tx_status)
postredcaptable
postaRMTtable
prop.table(postaRMTtable, margin = 2)*100
prop.table(postredcaptable, margin = 2)*100

mosaicplot(postredcaptable)
mosaicplot(postaRMTtable)

## week 8 analysis ----
View(week8)
week8 <- fourdata_clean %>%
  filter(event_name == "8") %>%
  droplevels()

week8 <- week8[!is.na(week8$tx_status), 1:9]              #get rid of NAs in tx_status
week8$tx_status <- droplevels(week8$tx_status)           #drop levels

head(week8)

group_by(week8, tx_status) %>%
  summarise(
    count = n(),
    mean = mean(any_pRMT, na.rm = TRUE),
    sd = sd(any_pRMT, na.rm = TRUE),
    median = median(any_pRMT, na.rm = TRUE),
    IQR = IQR(any_pRMT, na.rm = TRUE)
  )

kruskal.test(any_pRMT ~ tx_status, data = week8)
kruskal.test(hr_perc ~ tx_status, data = week8)


pairwise.wilcox.test(week8$hr_perc, week8$tx_status,
                     p.adjust.method = "BH")

pairwise.wilcox.test(week8$any_pRMT, week8$tx_status,
                     p.adjust.method = "BH")

fisher.test(week8$redcap_binary, week8$tx_status)
fisher.test(week8$anyaRMT_comp, week8$tx_status)

week8table <- table(week8$anyaRMT_comp, week8$tx_status)
prop.table(week8table, margin = 2)*100
