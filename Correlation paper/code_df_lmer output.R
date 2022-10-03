data = read.csv('dataset.csv')

library(lme4)
library(lmerTest)
## conduct the models adjusting for Age, gender.

Q1 <- c()
Estimate <- c()
StdError <- c()
df <- c()
t_value <- c()
pr_t <- c()
two_five<- c()
nine_five <- c()
pacman::p_load(progress)
## Basic
pb <- progress_bar$new(total=100)
for (i in 23:125) {
  pb$tick()
  fit <-lmer(q1
             ~ data[[i]]
             + Age + gender
             + (1 |p_id),data = data)
  Q1 <- append(Q1,toupper(names(data)[i]))
  Estimate <- append(Estimate,round(coef(summary(fit)), digits = 6)[2,1])
  StdError <- append(StdError,round(coef(summary(fit)), digits = 6)[2,2])
  df <- append(df,round(coef(summary(fit)), digits = 6)[2,3])
  t_value <- append(t_value,round(coef(summary(fit)), digits = 6)[2,4])
  pr_t <- append(pr_t,round(coef(summary(fit)), digits = 6)[2,5])
  two_five <- append(two_five,round(confint(fit), digits = 6)[4,1])
  nine_five <- append(nine_five,round(confint(fit), digits = 6)[4,2])
  Sys.sleep(0.1)
  
}

data_results= data.frame(Q1,Estimate,StdError,df,t_value,pr_t,two_five,nine_five)
colnames(data_results) <- c("Q1","Estimate
","Std. eror","df","t value","Pr(>|t|)","2.5 CI","97.5 CI
")
library(xlsx)
write.xlsx(data_results, "model output code.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = FALSE, append = FALSE)
write.csv(data_results,"model output.csv",row.names = FALSE)
