names(data)

## Q1 ----



FEATURE <- c()
Estimate <- c()
StdError <- c()
df <- c()
t_value <- c()
pr_t <- c()
two_five<- c()
nine_five <- c()
pacman::p_load(progress)
## Basic
pb <- progress_bar$new(total=124)
for (i in 23:147) {
  pb$tick()
  fit <-lmer(q1
             ~ data[[i]]
             + Age + gender
             + (1 |p_id),data = data)
  FEATURE <- append(FEATURE,toupper(names(data)[i]))
  Estimate <- append(Estimate,round(coef(summary(fit)), digits = 6)[2,1])
  StdError <- append(StdError,round(coef(summary(fit)), digits = 6)[2,2])
  df <- append(df,round(coef(summary(fit)), digits = 6)[2,3])
  t_value <- append(t_value,round(coef(summary(fit)), digits = 6)[2,4])
  pr_t <- append(pr_t,round(coef(summary(fit)), digits = 6)[2,5])
  two_five <- append(two_five,round(confint(fit), digits = 6)[4,1])
  nine_five <- append(nine_five,round(confint(fit), digits = 6)[4,2])
  Sys.sleep(0.1)
  
}

data_results= data.frame(FEATURE,Estimate,StdError,df,t_value,pr_t,two_five,nine_five)
colnames(data_results) <- c("FEATURE","Estimate","Std. eror","df","t value","Pr(>|t|)","2.5 CI","97.5 CI")
write.csv(data_results,"test.csv",row.names = FALSE)

## Q2 ----

FEATURE <- c()
Estimate <- c()
StdError <- c()
df <- c()
t_value <- c()
pr_t <- c()
two_five<- c()
nine_five <- c()
pacman::p_load(progress)
## Basic
pb <- progress_bar$new(total=125)
for (i in 23:147) {
  pb$tick()
  fit <-lmer(q2
             ~ data[[i]]
             + Age + gender
             + (1 |p_id),data = data)
  FEATURE <- append(FEATURE,toupper(names(data)[i]))
  Estimate <- append(Estimate,round(coef(summary(fit)), digits = 6)[2,1])
  StdError <- append(StdError,round(coef(summary(fit)), digits = 6)[2,2])
  df <- append(df,round(coef(summary(fit)), digits = 6)[2,3])
  t_value <- append(t_value,round(coef(summary(fit)), digits = 6)[2,4])
  pr_t <- append(pr_t,round(coef(summary(fit)), digits = 6)[2,5])
  two_five <- append(two_five,round(confint(fit), digits = 6)[4,1])
  nine_five <- append(nine_five,round(confint(fit), digits = 6)[4,2])
  Sys.sleep(0.1)
  
}

data_results= data.frame(FEATURE,Estimate,StdError,df,t_value,pr_t,two_five,nine_five)
colnames(data_results) <- c("FEATURE","Estimate","Std. eror","df","t value","Pr(>|t|)","2.5 CI","97.5 CI")
write.csv(data_results,"q22.csv",row.names = FALSE)

## Q3 ----

FEATURE <- c()
Estimate <- c()
StdError <- c()
df <- c()
t_value <- c()
pr_t <- c()
two_five<- c()
nine_five <- c()
pacman::p_load(progress)
## Basic
pb <- progress_bar$new(total=124)
for (i in 23:147) {
  pb$tick()
  fit <-lmer(q3
             ~ data[[i]]
             + Age + gender
             + (1 |p_id),data = data)
  FEATURE <- append(FEATURE,toupper(names(data)[i]))
  Estimate <- append(Estimate,round(coef(summary(fit)), digits = 6)[2,1])
  StdError <- append(StdError,round(coef(summary(fit)), digits = 6)[2,2])
  df <- append(df,round(coef(summary(fit)), digits = 6)[2,3])
  t_value <- append(t_value,round(coef(summary(fit)), digits = 6)[2,4])
  pr_t <- append(pr_t,round(coef(summary(fit)), digits = 6)[2,5])
  two_five <- append(two_five,round(confint(fit), digits = 6)[4,1])
  nine_five <- append(nine_five,round(confint(fit), digits = 6)[4,2])
  Sys.sleep(0.1)
  
}

data_results= data.frame(FEATURE,Estimate,StdError,df,t_value,pr_t,two_five,nine_five)
colnames(data_results) <- c("FEATURE","Estimate","Std. eror","df","t value","Pr(>|t|)","2.5 CI","97.5 CI")
write.csv(data_results,"Q3.csv",row.names = FALSE)

## Q4 ----

FEATURE <- c()
Estimate <- c()
StdError <- c()
df <- c()
t_value <- c()
pr_t <- c()
two_five<- c()
nine_five <- c()
pacman::p_load(progress)
## Basic
pb <- progress_bar$new(total=124)
for (i in 23:147) {
  pb$tick()
  fit <-lmer(q4
             ~ data[[i]]
             + Age + gender
             + (1 |p_id),data = data)
  FEATURE <- append(FEATURE,toupper(names(data)[i]))
  Estimate <- append(Estimate,round(coef(summary(fit)), digits = 6)[2,1])
  StdError <- append(StdError,round(coef(summary(fit)), digits = 6)[2,2])
  df <- append(df,round(coef(summary(fit)), digits = 6)[2,3])
  t_value <- append(t_value,round(coef(summary(fit)), digits = 6)[2,4])
  pr_t <- append(pr_t,round(coef(summary(fit)), digits = 6)[2,5])
  two_five <- append(two_five,round(confint(fit), digits = 6)[4,1])
  nine_five <- append(nine_five,round(confint(fit), digits = 6)[4,2])
  Sys.sleep(0.1)
  
}

data_results= data.frame(FEATURE,Estimate,StdError,df,t_value,pr_t,two_five,nine_five)
colnames(data_results) <- c("FEATURE","Estimate","Std. eror","df","t value","Pr(>|t|)","2.5 CI","97.5 CI")
write.csv(data_results,"Q4.csv",row.names = FALSE)
## Q5 ----

FEATURE <- c()
Estimate <- c()
StdError <- c()
df <- c()
t_value <- c()
pr_t <- c()
two_five<- c()
nine_five <- c()
pacman::p_load(progress)
## Basic
pb <- progress_bar$new(total=124)
for (i in 23:147) {
  pb$tick()
  fit <-lmer(q5
             ~ data[[i]]
             + Age + gender
             + (1 |p_id),data = data)
  FEATURE <- append(FEATURE,toupper(names(data)[i]))
  Estimate <- append(Estimate,round(coef(summary(fit)), digits = 6)[2,1])
  StdError <- append(StdError,round(coef(summary(fit)), digits = 6)[2,2])
  df <- append(df,round(coef(summary(fit)), digits = 6)[2,3])
  t_value <- append(t_value,round(coef(summary(fit)), digits = 6)[2,4])
  pr_t <- append(pr_t,round(coef(summary(fit)), digits = 6)[2,5])
  two_five <- append(two_five,round(confint(fit), digits = 6)[4,1])
  nine_five <- append(nine_five,round(confint(fit), digits = 6)[4,2])
  Sys.sleep(0.1)
  
}

data_results= data.frame(FEATURE,Estimate,StdError,df,t_value,pr_t,two_five,nine_five)
colnames(data_results) <- c("FEATURE","Estimate","Std. eror","df","t value","Pr(>|t|)","2.5 CI","97.5 CI")
write.csv(data_results,"Q5.csv",row.names = FALSE)

## Q6 ----

FEATURE <- c()
Estimate <- c()
StdError <- c()
df <- c()
t_value <- c()
pr_t <- c()
two_five<- c()
nine_five <- c()
pacman::p_load(progress)
## Basic
pb <- progress_bar$new(total=124)
for (i in 23:147) {
  pb$tick()
  fit <-lmer(q6
             ~ data[[i]]
             + Age + gender
             + (1 |p_id),data = data)
  FEATURE <- append(FEATURE,toupper(names(data)[i]))
  Estimate <- append(Estimate,round(coef(summary(fit)), digits = 6)[2,1])
  StdError <- append(StdError,round(coef(summary(fit)), digits = 6)[2,2])
  df <- append(df,round(coef(summary(fit)), digits = 6)[2,3])
  t_value <- append(t_value,round(coef(summary(fit)), digits = 6)[2,4])
  pr_t <- append(pr_t,round(coef(summary(fit)), digits = 6)[2,5])
  two_five <- append(two_five,round(confint(fit), digits = 6)[4,1])
  nine_five <- append(nine_five,round(confint(fit), digits = 6)[4,2])
  Sys.sleep(0.1)
  
}

data_results= data.frame(FEATURE,Estimate,StdError,df,t_value,pr_t,two_five,nine_five)
colnames(data_results) <- c("FEATURE","Estimate","Std. eror","df","t value","Pr(>|t|)","2.5 CI","97.5 CI")
write.csv(data_results,"Q6.csv",row.names = FALSE)

## Q7 ----

FEATURE <- c()
Estimate <- c()
StdError <- c()
df <- c()
t_value <- c()
pr_t <- c()
two_five<- c()
nine_five <- c()
pacman::p_load(progress)
## Basic
pb <- progress_bar$new(total=124)
for (i in 23:147) {
  pb$tick()
  fit <-lmer(q7
             ~ data[[i]]
             + Age + gender
             + (1 |p_id),data = data)
  FEATURE <- append(FEATURE,toupper(names(data)[i]))
  Estimate <- append(Estimate,round(coef(summary(fit)), digits = 6)[2,1])
  StdError <- append(StdError,round(coef(summary(fit)), digits = 6)[2,2])
  df <- append(df,round(coef(summary(fit)), digits = 6)[2,3])
  t_value <- append(t_value,round(coef(summary(fit)), digits = 6)[2,4])
  pr_t <- append(pr_t,round(coef(summary(fit)), digits = 6)[2,5])
  two_five <- append(two_five,round(confint(fit), digits = 6)[4,1])
  nine_five <- append(nine_five,round(confint(fit), digits = 6)[4,2])
  Sys.sleep(0.1)
  
}

data_results= data.frame(FEATURE,Estimate,StdError,df,t_value,pr_t,two_five,nine_five)
colnames(data_results) <- c("FEATURE","Estimate","Std. eror","df","t value","Pr(>|t|)","2.5 CI","97.5 CI")
write.csv(data_results,"Q7.csv",row.names = FALSE)

## Q8 ----

FEATURE <- c()
Estimate <- c()
StdError <- c()
df <- c()
t_value <- c()
pr_t <- c()
two_five<- c()
nine_five <- c()
pacman::p_load(progress)
## Basic
pb <- progress_bar$new(total=124)
for (i in 23:147) {
  pb$tick()
  fit <-lmer(q8
             ~ data[[i]]
             + Age + gender
             + (1 |p_id),data = data)
  FEATURE <- append(FEATURE,toupper(names(data)[i]))
  Estimate <- append(Estimate,round(coef(summary(fit)), digits = 6)[2,1])
  StdError <- append(StdError,round(coef(summary(fit)), digits = 6)[2,2])
  df <- append(df,round(coef(summary(fit)), digits = 6)[2,3])
  t_value <- append(t_value,round(coef(summary(fit)), digits = 6)[2,4])
  pr_t <- append(pr_t,round(coef(summary(fit)), digits = 6)[2,5])
  two_five <- append(two_five,round(confint(fit), digits = 6)[4,1])
  nine_five <- append(nine_five,round(confint(fit), digits = 6)[4,2])
  Sys.sleep(0.1)
  
}

data_results= data.frame(FEATURE,Estimate,StdError,df,t_value,pr_t,two_five,nine_five)
colnames(data_results) <- c("FEATURE","Estimate","Std. eror","df","t value","Pr(>|t|)","2.5 CI","97.5 CI")
write.csv(data_results,"Q8.csv",row.names = FALSE)

## Q9 ----

FEATURE <- c()
Estimate <- c()
StdError <- c()
df <- c()
t_value <- c()
pr_t <- c()
two_five<- c()
nine_five <- c()
pacman::p_load(progress)
## Basic
pb <- progress_bar$new(total=124)
for (i in 23:147) {
  pb$tick()
  fit <-lmer(q9
             ~ data[[i]]
             + Age + gender
             + (1 |p_id),data = data)
  FEATURE <- append(FEATURE,toupper(names(data)[i]))
  Estimate <- append(Estimate,round(coef(summary(fit)), digits = 6)[2,1])
  StdError <- append(StdError,round(coef(summary(fit)), digits = 6)[2,2])
  df <- append(df,round(coef(summary(fit)), digits = 6)[2,3])
  t_value <- append(t_value,round(coef(summary(fit)), digits = 6)[2,4])
  pr_t <- append(pr_t,round(coef(summary(fit)), digits = 6)[2,5])
  two_five <- append(two_five,round(confint(fit), digits = 6)[4,1])
  nine_five <- append(nine_five,round(confint(fit), digits = 6)[4,2])
  Sys.sleep(0.1)
  
}

data_results= data.frame(FEATURE,Estimate,StdError,df,t_value,pr_t,two_five,nine_five)
colnames(data_results) <- c("FEATURE","Estimate","Std. eror","df","t value","Pr(>|t|)","2.5 CI","97.5 CI")
write.csv(data_results,"Q9.csv",row.names = FALSE)

## Q10 ----

FEATURE <- c()
Estimate <- c()
StdError <- c()
df <- c()
t_value <- c()
pr_t <- c()
two_five<- c()
nine_five <- c()
pacman::p_load(progress)
## Basic
pb <- progress_bar$new(total=124)
for (i in 23:147) {
  pb$tick()
  fit <-lmer(q10
             ~ data[[i]]
             + Age + gender
             + (1 |p_id),data = data)
  FEATURE <- append(FEATURE,toupper(names(data)[i]))
  Estimate <- append(Estimate,round(coef(summary(fit)), digits = 6)[2,1])
  StdError <- append(StdError,round(coef(summary(fit)), digits = 6)[2,2])
  df <- append(df,round(coef(summary(fit)), digits = 6)[2,3])
  t_value <- append(t_value,round(coef(summary(fit)), digits = 6)[2,4])
  pr_t <- append(pr_t,round(coef(summary(fit)), digits = 6)[2,5])
  two_five <- append(two_five,round(confint(fit), digits = 6)[4,1])
  nine_five <- append(nine_five,round(confint(fit), digits = 6)[4,2])
  Sys.sleep(0.1)
  
}

data_results= data.frame(FEATURE,Estimate,StdError,df,t_value,pr_t,two_five,nine_five)
colnames(data_results) <- c("FEATURE","Estimate","Std. eror","df","t value","Pr(>|t|)","2.5 CI","97.5 CI")
write.csv(data_results,"Q10.csv",row.names = FALSE)

## Q11 ----

FEATURE <- c()
Estimate <- c()
StdError <- c()
df <- c()
t_value <- c()
pr_t <- c()
two_five<- c()
nine_five <- c()
pacman::p_load(progress)
## Basic
pb <- progress_bar$new(total=124)
for (i in 23:147) {
  pb$tick()
  fit <-lmer(q11
             ~ data[[i]]
             + Age + gender
             + (1 |p_id),data = data)
  FEATURE <- append(FEATURE,toupper(names(data)[i]))
  Estimate <- append(Estimate,round(coef(summary(fit)), digits = 6)[2,1])
  StdError <- append(StdError,round(coef(summary(fit)), digits = 6)[2,2])
  df <- append(df,round(coef(summary(fit)), digits = 6)[2,3])
  t_value <- append(t_value,round(coef(summary(fit)), digits = 6)[2,4])
  pr_t <- append(pr_t,round(coef(summary(fit)), digits = 6)[2,5])
  two_five <- append(two_five,round(confint(fit), digits = 6)[4,1])
  nine_five <- append(nine_five,round(confint(fit), digits = 6)[4,2])
  Sys.sleep(0.1)
  
}

data_results= data.frame(FEATURE,Estimate,StdError,df,t_value,pr_t,two_five,nine_five)
colnames(data_results) <- c("FEATURE","Estimate","Std. eror","df","t value","Pr(>|t|)","2.5 CI","97.5 CI")
write.csv(data_results,"Q11.csv",row.names = FALSE)

## Q12 ----

FEATURE <- c()
Estimate <- c()
StdError <- c()
df <- c()
t_value <- c()
pr_t <- c()
two_five<- c()
nine_five <- c()
pacman::p_load(progress)
## Basic
pb <- progress_bar$new(total=124)
for (i in 23:147) {
  pb$tick()
  fit <-lmer(q12
             ~ data[[i]]
             + Age + gender
             + (1 |p_id),data = data)
  FEATURE <- append(FEATURE,toupper(names(data)[i]))
  Estimate <- append(Estimate,round(coef(summary(fit)), digits = 6)[2,1])
  StdError <- append(StdError,round(coef(summary(fit)), digits = 6)[2,2])
  df <- append(df,round(coef(summary(fit)), digits = 6)[2,3])
  t_value <- append(t_value,round(coef(summary(fit)), digits = 6)[2,4])
  pr_t <- append(pr_t,round(coef(summary(fit)), digits = 6)[2,5])
  two_five <- append(two_five,round(confint(fit), digits = 6)[4,1])
  nine_five <- append(nine_five,round(confint(fit), digits = 6)[4,2])
  Sys.sleep(0.1)
  
}

data_results= data.frame(FEATURE,Estimate,StdError,df,t_value,pr_t,two_five,nine_five)
colnames(data_results) <- c("FEATURE","Estimate","Std. eror","df","t value","Pr(>|t|)","2.5 CI","97.5 CI")
write.csv(data_results,"Q12.csv",row.names = FALSE)

## Q13 ----

FEATURE <- c()
Estimate <- c()
StdError <- c()
df <- c()
t_value <- c()
pr_t <- c()
two_five<- c()
nine_five <- c()
pacman::p_load(progress)
## Basic
pb <- progress_bar$new(total=124)
for (i in 23:147) {
  pb$tick()
  fit <-lmer(q13
             ~ data[[i]]
             + Age + gender
             + (1 |p_id),data = data)
  FEATURE <- append(FEATURE,toupper(names(data)[i]))
  Estimate <- append(Estimate,round(coef(summary(fit)), digits = 6)[2,1])
  StdError <- append(StdError,round(coef(summary(fit)), digits = 6)[2,2])
  df <- append(df,round(coef(summary(fit)), digits = 6)[2,3])
  t_value <- append(t_value,round(coef(summary(fit)), digits = 6)[2,4])
  pr_t <- append(pr_t,round(coef(summary(fit)), digits = 6)[2,5])
  two_five <- append(two_five,round(confint(fit), digits = 6)[4,1])
  nine_five <- append(nine_five,round(confint(fit), digits = 6)[4,2])
  Sys.sleep(0.1)
  
}

data_results= data.frame(FEATURE,Estimate,StdError,df,t_value,pr_t,two_five,nine_five)
colnames(data_results) <- c("FEATURE","Estimate","Std. eror","df","t value","Pr(>|t|)","2.5 CI","97.5 CI")
write.csv(data_results,"Q13.csv",row.names = FALSE)

## Q14 ----

FEATURE <- c()
Estimate <- c()
StdError <- c()
df <- c()
t_value <- c()
pr_t <- c()
two_five<- c()
nine_five <- c()
pacman::p_load(progress)
## Basic
pb <- progress_bar$new(total=124)
for (i in 23:147) {
  pb$tick()
  fit <-lmer(q14
             ~ data[[i]]
             + Age + gender
             + (1 |p_id),data = data)
  FEATURE <- append(FEATURE,toupper(names(data)[i]))
  Estimate <- append(Estimate,round(coef(summary(fit)), digits = 6)[2,1])
  StdError <- append(StdError,round(coef(summary(fit)), digits = 6)[2,2])
  df <- append(df,round(coef(summary(fit)), digits = 6)[2,3])
  t_value <- append(t_value,round(coef(summary(fit)), digits = 6)[2,4])
  pr_t <- append(pr_t,round(coef(summary(fit)), digits = 6)[2,5])
  two_five <- append(two_five,round(confint(fit), digits = 6)[4,1])
  nine_five <- append(nine_five,round(confint(fit), digits = 6)[4,2])
  Sys.sleep(0.1)
  
}

data_results= data.frame(FEATURE,Estimate,StdError,df,t_value,pr_t,two_five,nine_five)
colnames(data_results) <- c("FEATURE","Estimate","Std. eror","df","t value","Pr(>|t|)","2.5 CI","97.5 CI")
write.csv(data_results,"Q14.csv",row.names = FALSE)

## Q15 ----

FEATURE <- c()
Estimate <- c()
StdError <- c()
df <- c()
t_value <- c()
pr_t <- c()
two_five<- c()
nine_five <- c()
pacman::p_load(progress)
## Basic
pb <- progress_bar$new(total=124)
for (i in 23:147) {
  pb$tick()
  fit <-lmer(q15
             ~ data[[i]]
             + Age + gender
             + (1 |p_id),data = data)
  FEATURE <- append(FEATURE,toupper(names(data)[i]))
  Estimate <- append(Estimate,round(coef(summary(fit)), digits = 6)[2,1])
  StdError <- append(StdError,round(coef(summary(fit)), digits = 6)[2,2])
  df <- append(df,round(coef(summary(fit)), digits = 6)[2,3])
  t_value <- append(t_value,round(coef(summary(fit)), digits = 6)[2,4])
  pr_t <- append(pr_t,round(coef(summary(fit)), digits = 6)[2,5])
  two_five <- append(two_five,round(confint(fit), digits = 6)[4,1])
  nine_five <- append(nine_five,round(confint(fit), digits = 6)[4,2])
  Sys.sleep(0.1)
  
}

data_results= data.frame(FEATURE,Estimate,StdError,df,t_value,pr_t,two_five,nine_five)
colnames(data_results) <- c("FEATURE","Estimate","Std. eror","df","t value","Pr(>|t|)","2.5 CI","97.5 CI")
write.csv(data_results,"Q15.csv",row.names = FALSE)

## Q16 ----

FEATURE <- c()
Estimate <- c()
StdError <- c()
df <- c()
t_value <- c()
pr_t <- c()
two_five<- c()
nine_five <- c()
pacman::p_load(progress)
## Basic
pb <- progress_bar$new(total=124)
for (i in 23:147) {
  pb$tick()
  fit <-lmer(q16
             ~ data[[i]]
             + Age + gender
             + (1 |p_id),data = data)
  FEATURE <- append(FEATURE,toupper(names(data)[i]))
  Estimate <- append(Estimate,round(coef(summary(fit)), digits = 6)[2,1])
  StdError <- append(StdError,round(coef(summary(fit)), digits = 6)[2,2])
  df <- append(df,round(coef(summary(fit)), digits = 6)[2,3])
  t_value <- append(t_value,round(coef(summary(fit)), digits = 6)[2,4])
  pr_t <- append(pr_t,round(coef(summary(fit)), digits = 6)[2,5])
  two_five <- append(two_five,round(confint(fit), digits = 6)[4,1])
  nine_five <- append(nine_five,round(confint(fit), digits = 6)[4,2])
  Sys.sleep(0.1)
  
}

data_results= data.frame(FEATURE,Estimate,StdError,df,t_value,pr_t,two_five,nine_five)
colnames(data_results) <- c("FEATURE","Estimate","Std. eror","df","t value","Pr(>|t|)","2.5 CI","97.5 CI")
write.csv(data_results,"Q16.csv",row.names = FALSE)



#### Grouped Items ####

data <- data %>%
  mutate(sleep = pmax(q1, q2, q3, q4)) %>%
  mutate(weight = pmax(q6, q7, q8, q9)) %>%
  mutate(psychomotor = pmax(q15, q16)) %>%
  rename(dep_mood = q5) %>%
  rename(conc = q10) %>%
  rename(guilt = q11) %>%
  rename(suicide = q12) %>%
  rename(interest = q13) %>%
  rename(fatigue = q14) 

## Sleep  ----

FEATURE <- c()
Estimate <- c()
StdError <- c()
df <- c()
t_value <- c()
pr_t <- c()
two_five<- c()
nine_five <- c()
pacman::p_load(progress)
## Basic
pb <- progress_bar$new(total=126)
for (i in 22:147) {
  pb$tick()
  fit <-lmer(sleep
             ~ data[[i]]
             + Age + gender
             + (1 |p_id),data = data)
  FEATURE <- append(FEATURE,toupper(names(data)[i]))
  Estimate <- append(Estimate,round(coef(summary(fit)), digits = 6)[2,1])
  StdError <- append(StdError,round(coef(summary(fit)), digits = 6)[2,2])
  df <- append(df,round(coef(summary(fit)), digits = 6)[2,3])
  t_value <- append(t_value,round(coef(summary(fit)), digits = 6)[2,4])
  pr_t <- append(pr_t,round(coef(summary(fit)), digits = 6)[2,5])
  two_five <- append(two_five,round(confint(fit), digits = 6)[4,1])
  nine_five <- append(nine_five,round(confint(fit), digits = 6)[4,2])
  Sys.sleep(0.1)
  
}

data_results= data.frame(FEATURE,Estimate,StdError,df,t_value,pr_t,two_five,nine_five)
colnames(data_results) <- c("FEATURE","Estimate","Std. eror","df","t value","Pr(>|t|)","2.5 CI","97.5 CI")
write.csv(data_results,"sleep.csv",row.names = FALSE)

## Weight  ----

FEATURE <- c()
Estimate <- c()
StdError <- c()
df <- c()
t_value <- c()
pr_t <- c()
two_five<- c()
nine_five <- c()
pacman::p_load(progress)
## Basic
pb <- progress_bar$new(total=126)
for (i in 22:147) {
  pb$tick()
  fit <-lmer(weight
             ~ data[[i]]
             + Age + gender
             + (1 |p_id),data = data)
  FEATURE <- append(FEATURE,toupper(names(data)[i]))
  Estimate <- append(Estimate,round(coef(summary(fit)), digits = 6)[2,1])
  StdError <- append(StdError,round(coef(summary(fit)), digits = 6)[2,2])
  df <- append(df,round(coef(summary(fit)), digits = 6)[2,3])
  t_value <- append(t_value,round(coef(summary(fit)), digits = 6)[2,4])
  pr_t <- append(pr_t,round(coef(summary(fit)), digits = 6)[2,5])
  two_five <- append(two_five,round(confint(fit), digits = 6)[4,1])
  nine_five <- append(nine_five,round(confint(fit), digits = 6)[4,2])
  Sys.sleep(0.1)
  
}

data_results= data.frame(FEATURE,Estimate,StdError,df,t_value,pr_t,two_five,nine_five)
colnames(data_results) <- c("FEATURE","Estimate","Std. eror","df","t value","Pr(>|t|)","2.5 CI","97.5 CI")
write.csv(data_results,"weight.csv",row.names = FALSE)


## Psychomotor  ----

FEATURE <- c()
Estimate <- c()
StdError <- c()
df <- c()
t_value <- c()
pr_t <- c()
two_five<- c()
nine_five <- c()
pacman::p_load(progress)
## Basic
pb <- progress_bar$new(total=126)
for (i in 22:147) {
  pb$tick()
  fit <-lmer(psychomotor
             ~ data[[i]]
             + Age + gender
             + (1 |p_id),data = data)
  FEATURE <- append(FEATURE,toupper(names(data)[i]))
  Estimate <- append(Estimate,round(coef(summary(fit)), digits = 6)[2,1])
  StdError <- append(StdError,round(coef(summary(fit)), digits = 6)[2,2])
  df <- append(df,round(coef(summary(fit)), digits = 6)[2,3])
  t_value <- append(t_value,round(coef(summary(fit)), digits = 6)[2,4])
  pr_t <- append(pr_t,round(coef(summary(fit)), digits = 6)[2,5])
  two_five <- append(two_five,round(confint(fit), digits = 6)[4,1])
  nine_five <- append(nine_five,round(confint(fit), digits = 6)[4,2])
  Sys.sleep(0.1)
  
}

data_results= data.frame(FEATURE,Estimate,StdError,df,t_value,pr_t,two_five,nine_five)
colnames(data_results) <- c("FEATURE","Estimate","Std. eror","df","t value","Pr(>|t|)","2.5 CI","97.5 CI")
write.csv(data_results,"psychomotor.csv",row.names = FALSE)
