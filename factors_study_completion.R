

library(tidyverse)
library(readxl)




#summarise clinical data
X2_Demographic_Table <- read_excel("~/R/2. Demographic Table.xlsx", 3)
ls(X2_Demographic_Table)

class(X2_Demographic_Table$`study weeks (int)`)

## study outcome x clinical state
subset1 <- X2_Demographic_Table %>%
  select(record_ID,"Ended by", "Age Rounddown", "Children", 
         "Comorbidities (mental or physical health)", 
         "Date of assessment", "Ethnicity", "Gender", 
         "IAPT primary diagnosis", "Therapy Intensity", 
         "Tx length (weeks)", "Years of formal education",
         "Suicidality", "Relationship status", "Previous Digital health use")  

glimpse(subset1)

subset1 <- subset1 %>%                                   # convert Ended by to factor (categorical) with 3 levels
  mutate(ended_by = factor(as.character(`Ended by`),
                             levels = c("lost to follow up", "withdrawal", "completed"),
                             labels = c("lost to follow up", "withdrawal", "completed"))) %>% 
  mutate(`Children` = factor(as.character(`Children`),
                             levels = c("Yes", "No"),
                             labels = c("Yes", "No"))) %>% 
  rename(comorbidities = `Comorbidities (mental or physical health)`) %>%
  mutate(`comorbidities` = factor(as.character(`comorbidities`),
                             levels = c("Yes", "No"),
                             labels = c("Yes", "No"))) %>% 
  mutate(tx_intensity = factor(as.character(`Therapy Intensity`),
                                  levels = c("Step 3", "Step 2"),
                                  labels = c("step 3", "step 2"))) %>% 
  mutate(tx_length_weeks = as.numeric("Tx length (weeks)")) %>%
  mutate(digital_experience = factor(as.character(`Previous Digital health use`),
                                     levels = c("Yes", "No"),
                                     labels = c("Yes", "No"))) %>% 
  mutate(gender = factor(as.character(Gender),
                                     levels = c("Female", "Male"),
                                     labels = c("Female", "Male"))) %>% 
  select(-`Ended by`, -"Tx length (weeks)", -`Previous Digital health use`, -Gender, -`Therapy Intensity`)


subset2 <- X2_Demographic_Table %>%
  select(record_ID,"Ended by", "Ethnicity","IAPT primary diagnosis",
         "Tx length (weeks)", "Suicidality")  

subset2 <- subset2 %>% 
  mutate(ended_by = factor(as.character(`Ended by`),
                           levels = c("lost to follow up", "withdrawal", "completed"),
                           labels = c("lost to follow up", "withdrawal", "completed"))) %>%  
  mutate(suicidality_cat = factor(as.character(Suicidality),
                           levels = c("0", "1", "2", "3"),
                           labels = c("0", "1", "2", "3"))) %>% 
  mutate(tx_length = as.numeric(substr(`Tx length (weeks)`, 1, 2)))
  

glimpse(subset1)
levels(subset1$gender)
colSums(is.na(subset1))


#join withdrawals with lost-to-follow up as "non-completers"
subset1$completion <-  as.factor(ifelse(subset1$ended_by == "completed", "completer", "non-completer"))
subset2$completion <-  as.factor(ifelse(subset2$ended_by == "completed", "completer", "non-completer"))


a <- table(subset1$completion, subset1$Children)
b <- table(subset1$completion, subset1$comorbidities)
c <- table(subset1$completion, subset1$tx_intensity)
d <- table(subset1$completion, subset1$digital_experience)
e <- table(subset1$completion, subset1$gender)
aggregate(subset1$`Years of formal education`, list(subset1$completion), FUN=mean)
aggregate(subset1$Suicidality, list(subset1$completion), FUN=median, na.rm = TRUE)
aggregate(subset2$tx_length, list(subset2$completion), FUN=mean, na.rm = TRUE)


table(subset1$completion, subset1$Suicidality)


mylist <- list(a, b, c, d, e)

sapply(mylist, chisq.test)

## treatment intensity is significant ->  p = 0.03252333 


#### Data viz for differences between completers and non-completers 

###### treatment length  #####

#means
tx_length.means <- data.frame(aggregate(subset2$tx_length, list(subset2$completion), FUN=mean, na.rm = TRUE))

#histograms
p <- ggplot(data = subset2, aes(x = tx_length)) + geom_histogram(binwidth = 4)
p + facet_wrap(~completion)


#density plots
a <- ggplot(subset2, aes(x = tx_length))
a +  geom_density(aes(color = completion))

# Change fill color by sex and add mean line
# Use semi-transparent fill: alpha = 0.4
a + geom_density(aes(fill = completion), alpha = 0.4) +
  geom_vline(aes(xintercept = x),
             data = tx_length.means, linetype = "dashed") 
# +
#   scale_color_manual(values = c("#868686FF", "#EFC000FF"))+
#   scale_fill_manual(values = c("#868686FF", "#EFC000FF"))

#anova
one.way <- aov(tx_length ~ completion, data = subset2)
summary(one.way)

###ANOVA SUMMARY: 
# Df Sum Sq Mean Sq F value Pr(>F)  
# completion   1  268.8  268.82   5.809 0.0189 *
#   Residuals   62 2868.9   46.27                 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 2 observations deleted due to missingness


####### treatment intensity  #####

#frequencies
table(subset1$completion, subset1$tx_intensity)

aggregate(subset2$tx_length, list(subset1$tx_intensity), FUN=mean, na.rm = TRUE)

X2_Demographic_Table$`study weeks (int)`

aggregate(X2_Demographic_Table$`study weeks (int)`, list(subset1$tx_intensity), FUN=mean, na.rm = TRUE)
df <- as.data.frame(cbind(X2_Demographic_Table$`study weeks (int)`, subset1$tx_intensity))

df <- df %>% rename(study_weeks = V1)

#anova
one.way <- aov(V1 ~ V2, data = df)
summary(one.way)

