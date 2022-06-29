install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyverse")

library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)

#import dataset
redcap_completion <- read_excel("~/REDcap completion data.xlsb.xlsx", 1)



redcap_weeks <- redcap_completion %>%
  filter(redcap_event_name != "End of study", 
         redcap_event_name != "Enrolment", 
         redcap_event_name != "WAI") %>%
  droplevels() %>%                                                    #drop Enrolment, WAI and end of study levels of 'event' variable
  mutate(type = case_when(redcap_event_name == "Week 5" |             #create new column with total number of tasks per week
                            redcap_event_name == "Week 9" | 
                            redcap_event_name == "Week 13" |	
                            redcap_event_name == "Week 17" |	
                            redcap_event_name == "Week 21" | 
                            redcap_event_name == "Week 25" | 
                            redcap_event_name == "Week 29" |	
                            redcap_event_name == "Week 33" |	
                            redcap_event_name == "Week 37" ~ "4",
                            redcap_event_name == "Week 1" ~ "2",
                          TRUE ~ "6"))

View(redcap_weeks_clean)

redcap_weeks_clean <- redcap_weeks$redcap_event_name %>%           #remove the word "weeks"
  str_remove_all("Week ")
redcap_weeks_clean <- redcap_weeks %>%
  mutate(redcap_event_name = redcap_weeks_clean)              #and add to previous df


dfwithoutNAs_clean <- dfwithoutNAs$redcap_event_name %>%           #remove the word "weeks"
  str_remove_all("Week ")

dfwithoutNAs_clean <- dfwithoutNAs %>%
  mutate(redcap_event_name = dfwithoutNAs_clean)              #and add to previous df
View(dfwithoutNAs_clean)



# #plots
# 
# redcap_weeks %>%
#   mutate(week = factor(redcap_event_name)) %>%
#   ggplot(aes(week, weekly_complete)) + geom_violin() + geom_jitter(alpha=.5,width=.1,height=0) +
#   labs(x="Week", y = "Completion")
# 
# 
# redcap_weeks %>%
#   ggplot(aes(redcap_event_name, weekly_complete, group=1))  +
#   stat_summary(fun.data = mean_cl_boot, geom="ribbon", alpha=.3) +
#   stat_summary(fun = match.fun(mean), geom="line")
# 
# dfwithoutNAs_clean$redcap_event_name <- as.character(as.integer(dfwithoutNAs_clean$redcap_event_name))
# 
# dfwithoutNAs %>%
#   ggplot(aes(redcap_event_name, weekly_complete, group=1))  +
#   stat_summary(fun = match.fun(mean), geom="line")
# 
# 
# redcap_weeks[redcap_weeks$record_id == 15, "phq9_complete" == "Complete"]
# count(redcap_weeks[redcap_weeks$record_id == 15, 3 == "Complete"])
