######################################
#    
# Analysis of  study completion status by Treatment    
#
######################################

#  DV: Study completion status (completer vs non-completer) 
#  IV1: Treatment length in weeks
#  IV2: Treatment intensity (step 2 vs step 3)

# covariates: Depression and anxiety severity
#             Study length


#libraries
library(tidyverse)
library(readxl)


X2_Demographic_Table <- read_excel("~/R/2. Demographic Table.xlsx", 
                                   sheet = "Demographics (R)", 
                                   col_types = c("text", 
                                                                             "numeric", "date", "date", "text", 
                                                                             "date", "numeric", "date", "numeric", 
                                                                             "numeric", "numeric", "numeric", 
                                                                             "text", "numeric", "numeric", "date", 
                                                                             "text", "text", "skip", "text", "numeric", 
                                                                             "text", "text", "text", "numeric", 
                                                                             "text", "text", "numeric", "text", 
                                                                             "numeric", "text", "numeric", "numeric", 
                                                                             "numeric", "text", "text", "text", 
                                                                             "text", "numeric", "numeric", "numeric", 
                                                                             "text", "numeric", "numeric", "text", 
                                                                             "text", "text", "text", "text", "text", 
                                                                             "numeric", "text", "text", "numeric", 
                                                                             "numeric", "text", "numeric", "numeric", 
                                                                             "numeric", "text", "text", "numeric", 
                                                                             "numeric", "numeric", "text", "text", 
                                                                             "text", "text", "numeric", "numeric", 
                                                                             "numeric", "numeric", "numeric", 
                                                                             "numeric", "numeric", "numeric", 
                                                                             "numeric", "text", "numeric", "numeric", 
                                                                             "text", "numeric", "numeric", "numeric", 
                                                                             "numeric", "numeric", "text", "numeric"))

#summarise clinical data
ls(X2_Demographic_Table)

## study outcome x clinical state
subset1 <- X2_Demographic_Table %>%
  select(record_ID,"Ended by", "Age Rounddown", "Children", 
         "Comorbidities (mental or physical health)", 
         "Date of assessment", "Ethnicity", "Gender", 
         "IAPT primary diagnosis", "Therapy Intensity", 
         "Tx length (weeks)", "Years of formal education", "study weeks (int)", 
         "Suicidality", "Relationship status", "Previous Digital health use", 
         "Total GAD7", "Total PHQ 9")  

class(subset1$`study weeks (int)`)



subset1 <- subset1 %>%                                  
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
  rename(tx_length = `Tx length (weeks)`)%>%
  mutate(tx_length = round(tx_length, digits = 0)) %>% 
  mutate(digital_experience = factor(as.character(`Previous Digital health use`),
                                     levels = c("Yes", "No"),
                                     labels = c("Yes", "No"))) %>% 
  mutate(gender = factor(as.character(Gender),
                                     levels = c("Female", "Male"),
                                     labels = c("Female", "Male"))) %>% 
  mutate(suicidality_cat = factor(as.character(Suicidality),
                                  levels = c("0", "1", "2", "3"),
                                  labels = c("0", "1", "2", "3"))) %>% 
  rename(study_length = `study weeks (int)`)%>%
  rename(total_gad = `Total GAD7`)%>%
  rename(total_phq = `Total PHQ 9`)%>%
  rename(age = `Age Rounddown`)%>%
  select(-`Ended by`, -`Previous Digital health use`, -Gender, -`Therapy Intensity`)


#### create 2 levels of the new variable = STUDY COMPLETION (1 = completer, 2 = non-completer)
#join withdrawals with lost-to-follow up as "non-completers"
subset1$completion <-  as.factor(ifelse(subset1$ended_by == "completed", "completer", "non-completer"))

glimpse(subset1)
  
colSums(is.na(subset1))


## frequency tables and means
a <- table(subset1$completion, subset1$Children)
b <- table(subset1$completion, subset1$comorbidities)
c <- table(subset1$completion, subset1$tx_intensity)
d <- table(subset1$completion, subset1$digital_experience)
e <- table(subset1$completion, subset1$gender)
table(subset1$completion, subset1$Suicidality)
aggregate(subset1$`Years of formal education`, list(subset1$completion), FUN=mean, na.rm = TRUE)
aggregate(subset1$Suicidality, list(subset1$completion), FUN=mean, na.rm = TRUE)
aggregate(subset1$study_length, list(subset1$completion), FUN=mean, na.rm = TRUE)
aggregate(subset1$total_gad, list(subset1$completion), FUN=median, na.rm = TRUE)
aggregate(subset1$total_phq, list(subset1$completion), FUN=median, na.rm = TRUE)
aggregate(subset1$age, list(subset1$completion), FUN=median, na.rm = TRUE)



##### Chi squared test of association between variables and Completion
mylist <- list(a, b, c, d, e)

sapply(mylist, chisq.test)
## treatment intensity is significant ->  p = 0.033



##### completion by treatment length #####

aggregate(subset1$tx_length, list(subset1$completion), FUN=median, na.rm = TRUE)
median <- aggregate(subset1$tx_length, list(subset1$completion), FUN=median, na.rm = TRUE)

# Mann-Whitney U test for non-parametric data
t <- wilcox.test(subset1$tx_length ~ subset1$completion, conf.int = TRUE, distribution="exact") 
t$estimate


#histograms
p <- ggplot(data = subset1, aes(x = tx_length)) + geom_histogram(binwidth = 4)
p + facet_wrap(~completion)


#cute density plots
a <- ggplot(subset1, aes(x = tx_length))
# a +  geom_density(aes(color = completion))
a + geom_density(aes(fill = completion), alpha = 0.4) +
  geom_vline(aes(xintercept = x),
             data = median, linetype = "dashed") 

# Boxplot
boxplot(tx_length~completion,data=subset1, main="Tx Length by Attrition",
        xlab="Attrition", ylab="Treatment Length")


####### completion by treatment intensity  #####

#frequencies
table(subset1$completion, subset1$tx_intensity)

chisqtable <- table(subset1$completion, subset1$tx_intensity)
chisq.test(chisqtable)

# stacked bar chart
ggplot(subset1, aes(x = tx_intensity, fill = completion)) + 
  geom_bar() 

##Balloon plot
library(gplots)
balloonplot(t(chisqtable))


####### tx length by treatment intensity  #####


aggregate(subset1$tx_length, list(subset1$tx_intensity), FUN=mean, na.rm = TRUE)
aggregate(subset1$tx_length, list(subset1$completion), FUN=median, na.rm = TRUE)
median <- aggregate(subset1$tx_length, list(subset1$completion), FUN=median, na.rm = TRUE)

# Mann-Whitney U test for non-parametric data
t <- wilcox.test(subset1$tx_length ~ subset1$tx_intensity) 
t


#density plots
a <- ggplot(subset1, aes(x = tx_length))
# Use semi-transparent fill: alpha = 0.4
a + geom_density(aes(fill = tx_intensity), alpha = 0.4) +
  geom_vline(aes(xintercept = x),
             data = median, linetype = "dashed") 

###################

#   There is a sig difference between completers and non-completer on (1) TX intensity and statistical trend on (2) Tx length
#   (Tx intensity is associated with differing tx lengths as per IAPT procedures)

# is there an effect of study length?

###################


#### treatment intensity x study length  #####

aggregate(subset1$study_length, list(subset1$tx_intensity), FUN=mean, na.rm = TRUE)
aggregate(subset1$study_length, list(subset1$tx_intensity), FUN=median, na.rm = TRUE)
mean <- aggregate(subset1$study_length, list(subset1$tx_intensity), FUN=mean, na.rm = TRUE)

# t-test
t <- t.test(subset1$study_length ~ subset1$tx_intensity) 
t
mean <- data.frame(t$estimate)


#density plots
a <- ggplot(subset1, aes(x = study_length))
# Use semi-transparent fill: alpha = 0.4
a + geom_density(aes(fill = tx_intensity), alpha = 0.4) +
  geom_vline(aes(xintercept = t.estimate),
             data = mean, linetype = "dashed") 


##### Treatment length x study length #####

library("ggpubr")

#scatterplot
ggscatter(subset1, x = "tx_length", y = "study_length",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "tx_length", ylab = "study_length")


# Shapiro-Wilk normality test for tx length
shapiro.test(subset1$tx_length) # => p > 0.05 - data not normally distributed
# Shapiro-Wilk normality test for study_length
shapiro.test(subset1$study_length) # => p < 0.05 - can assume normality

# Q-Q plots
ggqqplot(subset1$tx_length, ylab = "tx length")
ggqqplot(subset1$study_length, ylab = "study_length")

## correlation analysis
res <- cor.test(subset1$tx_length, subset1$study_length, 
                method = "spearman")
res



###### Completion by Study length ####

glimpse(subset1)

mean <- aggregate(subset1$study_length, list(subset1$completion), FUN=mean, na.rm = TRUE)
mean

#histograms
p <- ggplot(data = subset1, aes(x = study_length)) + geom_histogram(binwidth = 5)
p + facet_wrap(~completion)

#cute density plots
a <- ggplot(subset1, aes(x = study_length))
# a +  geom_density(aes(color = completion))
a + geom_density(aes(fill = completion), alpha = 0.4) +
  geom_vline(aes(xintercept = x),
             data = mean, linetype = "dashed") 

# t-test
t <- t.test(subset1$study_length ~ subset1$completion) 
t


######  Covariate: Baseline clinical Severity  ####

# PHQ9 and GAD7

# Shapiro-Wilk normality test for tx length
shapiro.test(subset1$total_gad) # => p < 0.05 - can assume normality
# Shapiro-Wilk normality test for study_length
shapiro.test(subset1$total_phq) # => p < 0.05 - can assume normality

# Q-Q plots
ggqqplot(subset1$total_gad, ylab = "total gad")
ggqqplot(subset1$total_phq, ylab = "total phq")


## TX length to Severity

#scatterplot tx length to Depression
ggscatter(subset1, x = "total_phq", y = "tx_length",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "total_phq", ylab = "tx_length")

#scatterplot tx length to anxiety
ggscatter(subset1, x = "total_gad", y = "tx_length",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "total_gad", ylab = "tx_length")


## correlation analysis tx lenth to depression + anxiety
res <- cor.test(subset1$tx_length, subset1$total_phq, 
                method = "spearman")
res2 <- cor.test(subset1$tx_length, subset1$total_gad, 
                method = "spearman")
res
res2


###  Severity by tx intensity

mean <- aggregate(subset1$total_phq, list(subset1$tx_intensity), FUN=mean, na.rm = TRUE)
mean <- aggregate(subset1$total_gad, list(subset1$tx_intensity), FUN=mean, na.rm = TRUE)
mean

#DEPRESSION

p <- ggplot(data = subset1, aes(x = total_phq)) + geom_histogram(binwidth = 2)
p + facet_wrap(~tx_intensity)
#cute density plots
a <- ggplot(subset1, aes(x =total_phq))
a + geom_density(aes(fill = tx_intensity), alpha = 0.4) +
  geom_vline(aes(xintercept = x),
             data = mean, linetype = "dashed")
# t-test
t <- t.test(subset1$total_phq ~ subset1$tx_intensity) 
t

#ANXIETY
p <- ggplot(data = subset1, aes(x = total_gad)) + geom_histogram(binwidth = 2)
p + facet_wrap(~tx_intensity)
#cute density plots
a <- ggplot(subset1, aes(x =total_gad))
a + geom_density(aes(fill = tx_intensity), alpha = 0.4) +
  geom_vline(aes(xintercept = x),
             data = mean, linetype = "dashed") 

# t-test
t <- t.test(subset1$total_gad ~ subset1$tx_intensity) 
t



###  Severity by study completion

mean <- aggregate(subset1$total_phq, list(subset1$completion), FUN=mean, na.rm = TRUE)
mean <- aggregate(subset1$total_gad, list(subset1$completion), FUN=mean, na.rm = TRUE)
mean

#DEPRESSION

p <- ggplot(data = subset1, aes(x = total_phq)) + geom_histogram(binwidth = 2)
p + facet_wrap(~completion)
#cute density plots
a <- ggplot(subset1, aes(x =total_phq))
a + geom_density(aes(fill = completion), alpha = 0.4) +
  geom_vline(aes(xintercept = x),
             data = mean, linetype = "dashed")
# t-test
t <- t.test(subset1$total_phq ~ subset1$completion) 
t

#ANXIETY
p <- ggplot(data = subset1, aes(x = total_gad)) + geom_histogram(binwidth = 2)
p + facet_wrap(~completion)
#cute density plots
a <- ggplot(subset1, aes(x =total_gad))
a + geom_density(aes(fill = completion), alpha = 0.4) +
  geom_vline(aes(xintercept = x),
             data = mean, linetype = "dashed") 

# t-test
t <- t.test(subset1$total_gad ~ subset1$completion) 
t





