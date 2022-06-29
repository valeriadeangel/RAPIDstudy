#libraries
library(readxl)
library(plyr)
library(dplyr)
library(tidyverse)
library(plotly)
library(tidyr)
library(ggplot2)
library(reshape2)
library(data.table)
library(broom)
library(readr)
library(lubridate)
library(timetk)

library(hrbrthemes)
library(kableExtra)
library(viridis)
library(radiant)


#import dataframes
IDmap <- read_excel("~/R/5. pRMT data.xlsx")

X5_pRMT_data <- read_excel("~/R/5. pRMT data.xlsx", 2)


View(count(X5_pRMT_data, SUBJECT_ID))

#add the REDCap ID numbers and rearrange columms
pRMT <- merge(X5_pRMT_data, IDmap, all.x = TRUE, all.y=TRUE, no.dups = TRUE, incomparables = NULL)
pRMT <- pRMT %>% drop_na()         #drop IDs that are not in the IDmap df i.e. test subjects
View(count(pRMT, SUBJECT_ID))

#pRMT <- pRMT[, c(11, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)] # reoder columns
# write.csv(pRMT_days,"~/R/pRMT_days.csv", row.names = FALSE)   #export to excel - \OneDrive\Documentos\R

#add study days
pRMT_days <- pRMT %>%
  arrange(ymd(pRMT$date)) %>%
  group_by(SUBJECT_ID) %>%
  mutate(day = row_number())

View(pRMT_days)

#so far so good

# remove days not in study ------------------------------------------------

pRMT_allIDs <- pRMT_days
#leave only days in study
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
  select(record_ID,day, bt_hour, acc_hour, gps_hour)

## to Exclude availability = 0 
# select_pRMT[select_pRMT == 0] <- NA

# plots -------------------------------------------------------------------

#plot a mean line of data availability of average percentage complete by study end

# set df as long
pRMT_long <- melt(select_pRMT, id.vars = c("SUBJECT_ID", "day"), 
               variable.name = "stream",
               value.name = "available_hours")

#plot mean available hours by data stream.
pRMT_bydaystream <- pRMT_long %>%
  group_by(day, stream) %>%
  mutate(total_n = n()) %>%
  mutate(is_outlier = total_n < 6) %>%             #step 1. to exclude weeks with < 5 people
  filter(is_outlier == FALSE) %>%                  #step 2. to exclude weeks with < 5 people
  summarise_at(c("available_hours", "total_n"), mean, na.rm = TRUE)

plot_pRMTday <- ggplot(pRMT_bydaystream, aes(x=day, y=available_hours, color=stream)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)
plot_pRMTday

#plot summarised by week
prMT_byweekstream <- pRMT_long %>%
  group_by(day, stream) %>%
  mutate(total_n = n()) %>%
  mutate(is_outlier = total_n < 6) %>%             #step 1. to exclude weeks with < 5 people
  filter(is_outlier == FALSE) %>%                  #step 2. to exclude weeks with < 5 people
  mutate(week = ceiling(day / 7)) %>%
  group_by(week, stream) %>%
  drop_na() %>%
  summarise_at(c("available_hours", "total_n"), mean, na.rm = TRUE)

plot_pRMTweek <- ggplot(prMT_byweekstream, aes(x=week, y=available_hours, color=stream)) +
  geom_line()
plot_pRMTweek




#  Bluetooth -----

pRMT_lengthstudy <- test2

## BLUETOOTH by study days - switch to wide format
bt_studydate <- pRMT_lengthstudy %>%
  select(record_ID, SUBJECT_ID, day, date_id, date, bt_miss_rate)
bt_studydate_wide <- dcast(bt_studydate, record_ID + SUBJECT_ID ~ day)
# write.csv(bt_studydate_wide,"~/R/by_studydate_wide.csv", row.names = FALSE)   #export to excel - \OneDrive\Documentos\R

# # # some participants will have NA from a date prior to their end date meaning there are no Fitbit files as opposed to there being data = 0
# # # add by hand shaded in blue, cell which reflect days in the study but no data from pRMT



# by real date
View(count(pRMT, record_ID))

bt_realdate <- dcast(bt_studydate, record_ID + SUBJECT_ID ~ date)
write.csv(bt_realdate,"~/R/bt_realdate.csv", row.names = FALSE)   #export to excel - \OneDrive\Documentos\R


# NOT DOING THIS ANYMORE ---------
#Making data availability = 0, based on study dates, regardless of whether pRMT data is there or not
##enter NAs BY HAND for each participant after their study end date on studaydateswideNAs.xls
# 1. replace all NAs to 100
# 2. rename doc to bt_heatmap.xlsx and upload
heatmap <- read_excel("C:/Users/valer/Downloads/bluetooth heatmaps.xlsx")
# 3. set df as long
longdf <- melt(heatmap, id.vars = c("record_ID", "SUBJECT_ID"), 
               variable.name = "day",
               value.name = "missing_rate")

# 4. rename the strangely renamed days 1 and 100...?
levels(longdf$day)[levels(longdf$day)=="1...3"] <- "1" 
levels(longdf$day)[levels(longdf$day)=="1...102"] <- "100" 
longdf$day <- as.character(longdf$day)

pRMT <- longdf
pRMT <- pRMT %>%
  arrange(record_ID, as.numeric(day))
write.csv(pRMT,"~/R/test.csv", row.names = FALSE)   #export to excel - \OneDrive\Documentos\R

pRMT <- pRMT %>% filter(record_ID=="14",day > 282)


# 6. switch back to wide format
test <- pRMT %>%
  select(record_ID, SUBJECT_ID, day, missing_rate) %>%
  group_by(record_ID) %>%
  order()

bt_studydate_wide <- dcast(bt_studydate, record_ID + SUBJECT_ID ~ day)
write.csv(bt_studydate_wide,"~/R/bt_studydate_wide.csv", row.names = FALSE)   #export to excel - \OneDrive\Documentos\R





# GPS by study days - switch to wide format---------------------------------------------------

gps_studydate <- pRMT_lengthstudy %>%
  select(record_ID, SUBJECT_ID, day, date_id, date, gps_miss_rate)
gps_studydate_wide <- dcast(gps_studydate, record_ID + SUBJECT_ID ~ day)
write.csv(gps_studydate_wide,"~/R/gps_studydate_wide.csv", row.names = FALSE)   #export to excel

gps_realdate <- dcast(gps_studydate, record_ID + SUBJECT_ID ~ date)
write.csv(gps_realdate,"~/R/gps_realdate.csv", row.names = FALSE)   #export to excel 


## ACCELERATION by study days - switch to wide format

acc_studydate <- pRMT_lengthstudy %>%
  select(record_ID, SUBJECT_ID, day, date_id, date, acc_miss_rate)
acc_studydate_wide <- dcast(acc_studydate, record_ID + SUBJECT_ID ~ day)
write.csv(acc_studydate_wide,"~/R/acc_studydate_wide.csv", row.names = FALSE)   #export to excel

acc_realdate <- dcast(acc_studydate, record_ID + SUBJECT_ID ~ date)
write.csv(acc_realdate,"~/R/acc_realdate.csv", row.names = FALSE)   #export to excel


## SLEEP BINARY by study days - switch to wide format

sleep_studydate <- pRMT_lengthstudy %>%
  select(record_ID, SUBJECT_ID, day, date_id, date, sleep_binary)
sleep_studydate_wide <- dcast(sleep_studydate, record_ID + SUBJECT_ID ~ day)
write.csv(sleep_studydate_wide,"~/R/sleep_studydate_wide.csv", row.names = FALSE)   #export to excel

sleep_realdate <- dcast(sleep_studydate, record_ID + SUBJECT_ID ~ date)
write.csv(sleep_realdate,"~/R/sleep_realdate.csv", row.names = FALSE)   #export to excel


# pRMT Data Availability --------------------------------------------------
missing_perday <- read_excel("~/R/5. pRMT data.xlsx", 4)
View(missing_perday)


##calculate avg missing rate by week
# assign week to each day
pRMT_perday <- missing_perday %>%
  mutate(week = ceiling(Day / 7))

#calculate weekly average
gps_weekly <- pRMT_perday %>%
  group_by(week) %>%
  summarise_at(vars(GPS_mean), list(gps_avg = mean))

acc_weekly <- pRMT_perday %>%
  group_by(week) %>%
  summarise_at(vars(acc_mean), list(acc_avg = mean))

bt_weekly <- pRMT_perday %>%
  group_by(week) %>%
  summarise_at(vars(bt_mean), list(bt_avg = mean))

sleep_weekly <- pRMT_perday %>%
  group_by(week) %>%
  summarise_at(vars(sleep_missing), list(sleep_avg = mean))

pRMT_weekly <- join_all(list(gps_weekly, acc_weekly, bt_weekly, sleep_weekly), by = "week")
View(pRMT_weekly)
write.csv(pRMT_weekly,"~/R/pRMT_weekly.csv", row.names = FALSE)   #export to excel


##calculate number of participants with at least 50% available data by week
gps_weekly <- pRMT_perday %>%
  group_by(week) %>%
  summarise_at(vars(GPS_50), list(gps_50 = mean))

acc_weekly <- pRMT_perday %>%
  group_by(week) %>%
  summarise_at(vars(acc_50), list(acc_50 = mean))

bt_weekly <- pRMT_perday %>%
  group_by(week) %>%
  summarise_at(vars(bt_50), list(bt_50 = mean))


pRMT_weekly_50 <- join_all(list(gps_weekly, acc_weekly, bt_weekly), by = "week")
View(pRMT_weekly_50)
write.csv(pRMT_weekly_50,"~/R/pRMT_weekly_50.csv", row.names = FALSE)   #export to excel


##calculate number of participants with at least 66% available data by week
gps_weekly <- pRMT_perday %>%
  group_by(week) %>%
  summarise_at(vars(GPS_66), list(gps_66 = mean))

acc_weekly <- pRMT_perday %>%
  group_by(week) %>%
  summarise_at(vars(acc_66), list(acc_66 = mean))

bt_weekly <- pRMT_perday %>%
  group_by(week) %>%
  summarise_at(vars(bt_66), list(bt_66 = mean))


pRMT_weekly_66 <- join_all(list(gps_weekly, acc_weekly, bt_weekly), by = "week")
View(pRMT_weekly_66)
write.csv(pRMT_weekly_66,"~/R/pRMT_weekly_66.csv", row.names = FALSE)   #export to excel



#####Create new variable with overall pRMT total availability. If any stream is available (8+ hours) = 1
pRMT_lengthstudy$pRMT_available <- ifelse(pRMT_lengthstudy$bt_hour > 7 | pRMT_lengthstudy$gps_hour > 7 | pRMT_lengthstudy$acc_hour > 7, as.numeric(1), as.numeric(0))
write.csv(pRMT_lengthstudy,"~/R/pRMT_lengthstudy.csv", row.names = FALSE)   #export to excel


# Sum of the column by group 
missing_perday <- missing_perday %>% drop_na() 
anypRMT <- aggregate(x = pRMT_lengthstudy$pRMT_available,
          by= list(pRMT_lengthstudy$day),
          FUN=sum)

anypRMT <- anypRMT %>%
  select("x") %>%
  rename(anypRMT = x)

missing_perday <- cbind(missing_perday, anypRMT)
write.csv(missing_perday,"~/R/missing_perday.csv", row.names = FALSE)   #export to excel


pRMT_perday <- missing_perday %>%
  mutate(week = ceiling(Day / 7))
anypRMT_weekly <- pRMT_perday %>%
  group_by(week) %>%
  summarise_at(vars(anypRMT), list(anypRM = mean))
write.csv(anypRMT_weekly,"~/R/anypRMT_weekly.csv", row.names = FALSE)   #export to excel
