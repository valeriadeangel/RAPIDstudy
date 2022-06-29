install.packages("rlist")

library(rlist)
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(writexl)
library(tidyverse)

 
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

selectq <- QIDS %>% select(participant_ID, date_notified, `Total Score`)

WSAS <- read_excel("~/R/4. aRMT data.xlsx", sheet = "WSAS", 
                   col_types = c("text", "date", "date", 
                                 "numeric", "date", "date", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric"))

selectw <- WSAS %>% select(participant_ID, date_notified, `Total Score`)

IDmap <- read_excel("~/R/4. aRMT data.xlsx", 
                    sheet = "IDmap")

#assign double digit participant IDs
mergedID <- merge(IDmap, selectq,  all.x = TRUE, all.y=TRUE, no.dups = TRUE, incomparables = NULL)
mergedwithtest <- merge(selectw, mergedID, by = c("participant_ID", "date_notified"), all.x = TRUE, all.y = TRUE, no.dups = TRUE)
# length(unique(mergedID$participant_ID))
# colSums(is.na(mergedID))


mergedaRMT <- mergedwithtest %>% 
  subset(!(participant_ID %in% c('f5978923-cef2-4eeb-a49c-7d79be4b53eb',
                                 '5786af5e-99e3-4f78-848a-d3b67b8eb7ed'))) %>% 
  drop_na(record_id)

# write.csv(mergedwithtest,"~/R/mergedtest aRMT.csv", row.names = FALSE)   #export to excel

mergedaRMT$diff_in_days = as.numeric(difftime(mergedaRMT$date_notified, mergedaRMT$date_assessment, units = "days")) 
mergedaRMT$event_name = ceiling((mergedaRMT$diff_in_days)/7)

#some questionnaires were carried out on the same week (e.g. if WSAS completed on days 1 and 7 - both fall in week 1)
#so adding 1 day (+1) to the difference in days spreads them out better. 
mergedaRMT$event_name1 = ceiling((mergedaRMT$diff_in_days+1)/7)


# we check how many unique values of weeks there are (a and b) com --------
a <- mergedaRMT %>%         
  group_by(record_id) %>%
  summarise(a = length(unique(event_name)))
b <- mergedaRMT %>%
  group_by(record_id) %>%
  summarise(b = length(unique(event_name1)))
c <- mergedaRMT %>%
  group_by(record_id) %>%
  summarise(count = n())

View(merge(c,b))

eventcheck <- merge(c,b)
eventcheck$d <- eventcheck$count - eventcheck$b # creates a column showing which Ps have more events than unique week numbers
eventcheck[which(eventcheck$d != 0),]   # calls the rows with duplicate week numbers

# mergedaRMT <- subset(mergedaRMT, !is.na(record_id))  #this step has been done already


# #delete (NA) or rearrange duplicate weeks ----
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
  select(participant_ID, record_id, 'Total Score.x', 'Total Score.y', event_name1)
names(aRMT)[names(aRMT) == "Total Score.x"] <- 'total_qids'     # rename
names(aRMT)[names(aRMT) == "Total Score.y"] <- 'total_wsas'     # rename
names(aRMT)[names(aRMT) == "event_name1"] <- 'event_name'       # rename

## not all participants have aRMT data  ! 
list.count(unique(aRMT$participant_ID))   #so there are 66 unique ID numbers in this list
list.count(unique(test$participant_ID))   #   there are 61 unique ID numbers /66 participants
length(unique(aRMT$participant_ID))  

#create a dataframe for all weeks and all participants
event_name <- rep(1:40, times=66)
record_id <- rep(14:79, each=40)
allweeks <- as.data.frame(cbind(record_id, event_name))

merged <- merge(allweeks, IDmap[, c("record_id","participant_ID")], by = "record_id", all.x = TRUE)
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
  count() %>% 
  View()


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


write.csv(aRMTcompletion,"~/R/aRMTcompletion.csv", row.names = FALSE)   #export to excel

  