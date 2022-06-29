#import datasets
install.packages("ggplot2", dependencies = TRUE)
install.packages("readr")
install.packages("tidyverse")
install.packages("esquisse", type="binary")
install.packages("timetk", type="binary")
install.packages("radiant", repos = "https://radiant-rstats.github.io/minicran/", type = "binary")

library(readxl)
library(plyr)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(reshape2)
library(data.table)
library(broom)
library(readr)

library(radiant)
library(lubridate)
library(timetk)
library(kableExtra)
library(viridis)
library(plotly)
library(hrbrthemes)

#import dataframes
datekey <- read_excel("~/R/HR_210322.xlsx", 3,
                   col_types = c("numeric", "date"))
datekey <- rename(datekey, DATE_ID = timeInterval_ID)
HR <- read_excel("~/R/HR_210322.xlsx", 4, 
                   col_types = c("text", "numeric", "skip", "numeric"))

demographic_table <- read_excel("~/R/HR_210322.xlsx", 1, 
                                     range = "A1:M67", 
                                    col_types = c("numeric", "numeric", "date",
                                                    "date", "text", "date", "text", 
                                                    "date", "text", "numeric", "numeric",
                                                    "numeric","text"))
IDmap <- read_excel("~/R/HR_210322.xlsx", 5)

#add dates to df and export
HR <- merge(HR, datekey, by = c("DATE_ID"))
write.csv(HR,"~/R/HR.csv", row.names = FALSE)   #export to excel - \OneDrive\Documentos\R

#create a new column and number each day in the study
HRdates <- HR %>%
  group_by(SUBJECT_ID) %>%
  mutate(day = row_number())

#how many days per participant?
IDfreq <- HRdates %>% 
  group_by(SUBJECT_ID) %>% 
  summarise(max(day))
names(IDfreq)[1] <- "SUBJECT_ID"
names(IDfreq)[2] <- "ndays"          # number of days with Fitbit data


#write.csv(HRdateswide,"~/R/HRdateswide.csv", row.names = FALSE)   #export to C:\Users\valer\OneDrive\Documentos\R
#write.csv(avg_missing_rate,"~/R/avg_missingrate.csv", row.names = FALSE)   #export to C:\Users\valer\OneDrive\Documentos\R

#add the REDCap ID numbers and rearrange columms
HRdates <- merge(HRdates, IDmap, all.x = TRUE, all.y=TRUE, no.dups = TRUE, incomparables = NULL)
HRdates <- HRdates[, c(6, 1, 2, 4, 5, 3)] # reoder columns


#CHECK -  participants with no fitbit data are still included in the table
HRcount <- HRdates %>%
       count(record_ID)
write.csv(HRcount,"~/R/HRcount.csv", row.names = FALSE)   #export to C:\Users\valer\OneDrive\Documentos\R


#heatmap prep for study by study date
studydatewide <- HRdates %>%
  select(record_ID, SUBJECT_ID, DATE_ID, MISSING_RATE)
studydatewide <- dcast(HRdates, record_ID + SUBJECT_ID ~ day, value.var = "MISSING_RATE")
write.csv(studydatewide,"~/R/studydatewide.csv", row.names = FALSE)   #export to C:\Users\valer\OneDrive\Documentos\R

#heatmap prep for study by real date
realdatewide <- realdatesdiff %>%
  select(record_ID, SUBJECT_ID, DATE_ID, MISSING_RATE, datediff)
realdatewide <- dcast(realdatewide, record_ID + datediff ~ DATE_ID, value.var = "MISSING_RATE")
write.csv(realdatewide,"~/R/realdatewide.csv", row.names = FALSE)   #export to C:\Users\valer\OneDrive\Documentos\R



##enter NAs BY HAND for each participant after their study end date on studaydateswideNAs.xls
# 1. replace all NAs to 100
# 2. Enter NA in dates after each subject's end date. 



#calculate avg missing rate by week
HRdates <- HRdates %>%
  mutate(week = ceiling(day / 7))

#calculate weekly average
avg_missing_rate <- HRdates %>%
  group_by(week) %>%
  summarise_at(vars(MISSING_RATE), list(avg_missing_rate = mean))



#from a binary spreadsheet of data availability -> availability = HR > 49% 
X6_Fitbit_data <- read_excel("C:/Users/valer/Downloads/6. Fitbit data.xlsx", 8,
                             range = "A1:F249")
binarydaily <- X6_Fitbit_data %>%
  mutate(week = ceiling(day / 7))
binaryweekly <- binarydaily %>%
  group_by(week) %>%
  summarise_at(vars('%'), list(avg_missing_rate = mean))
write.csv(binaryweekly,"~/R/binaryweekly.csv", row.names = FALSE)   #export to C:\Users\valer\OneDrive\Documentos\R



realdatesdiff <- HRdates %>% 
  group_by(SUBJECT_ID) %>% 
  mutate(max(DATE_ID)) %>%
  mutate(min(DATE_ID))

realdatesdiff <- realdatesdiff %>%
  group_by(SUBJECT_ID) %>%
  mutate(datediff = max(DATE_ID)- min(DATE_ID))



#average weekly completion rates on cleaned database
missing_perweek <- read_excel("~/R/HR_210322.xlsx", 7)
missing_perweek <- missing_perweek %>%
  mutate(week = ceiling(day / 7))

#calculate weekly average
avg_missing_week <- missing_perweek %>%
  group_by(week) %>%
  summarise_at(vars(Missing), list(avg_missing_rate = mean))
write.csv(missing_perweek,"~/R/missing_perweek.csv", row.names = FALSE)   #export to C:\Users\valer\OneDrive\Documentos\R
write.csv(avg_missing_week,"~/R/avg_missing week.csv", row.names = FALSE)   #export to C:\Users\valer\OneDrive\Documentos\R


#make NAs for those dates not in the study

temporaryenddatedf[temporaryenddatedf[4:5]] <- 101
temporaryenddatedf[temporaryenddatedf[1:2, 1]] <- "101"

View(temporaryenddatedf)


##TEMP study end time!
temporaryenddatedf <- read_excel("~/Downloads/Demographic Table.xlsx", 3,
                                range = "A1:L67", 
                                col_types = c("numeric", "numeric", "date", 
                                              "date", "text", "date", "text", "date", 
                                              "date", "text", "numeric", "text"))

View(temporarystudyenddate)


demographic_table$TEMPstudydays <- demographic_table$TEMPstudyENDdate - demographic_table$`Date of assessment`








IDmap <- read_excel("~/R/aRMT data.xlsx", 4)
names(IDmap)[1] <- "REDCAP Participant ID"
names(IDmap)[2] <- "SUBJECT_ID"

IDfreq <- merge(IDfreq, IDmap, all.x = TRUE, all.y=TRUE, no.dups = TRUE, incomparables = NULL)
IDfreq$studydays <- demographic_table$studydays 
IDfreq <- IDfreq[, c(3, 1, 2, 4)]    # reoder columns


HR %>%                                    #plot timeseries
  # Get the columns
  select(datetimeStart, MISSING_RATE) %>%
  # Change the columns name
  set_names(c("date", "value")) %>%
  plot_time_series(date, value)


##heatmap prep for standardised dates
library(reshape2)
HRdateswide <- HRdates %>%
  select(`REDCAP Participant ID`, SUBJECT_ID, day, MISSING_RATE)
HRdateswide <- dcast(HRdates, `REDCAP Participant ID` + SUBJECT_ID ~ day, value.var = "MISSING_RATE")
#replace NAs with 100 (= 100% missing data)
HRdateswide[is.na(HRdateswide)] = 100


