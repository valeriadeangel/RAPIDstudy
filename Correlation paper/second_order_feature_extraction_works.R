
library(lmerTest)
library(lubridate)
library(tidyverse)
library(data.table)
library(dplyr)

library(mlmi)
library(plotmo)

options(scipen=999)
# options(scipen=0) # to revert scientific notation

# read qids table ----
# qids <- fread("C:\Users\k1754359\Downloads\QIDS_summarized.csv", data.table = F) %>%

qids <- fread("C:/Users/k1754359/Downloads/QIDS_summarized.csv", data.table=F) %>%
  dplyr::mutate(survey_date = lubridate::ymd(survey_date))
qids$response_time = qids$response_time/60
qids$complete_time = qids$complete_time/60
fit <-lmer(q16 ~ response_time + (1 |p_id), qids)
summary(fit)
fit <-lmer(q16 ~ complete_time + (1 |p_id), qids)
summary(fit)

win_size <- 7

# sleep -----
# read sleep table 

sleep <- fread("C:/Users/k1754359/Downloads/daily_sleep_feature.csv", data.table=F) %>%
# sleep <- fread("C:/Users/valer/Downloads/daily_sleep_feature.csv", data.table=F) %>%
  dplyr::mutate(date_str = lubridate::ymd(date_str))

# extract second-order features
sleep_feature <- colnames(sleep)[6:15]
second_feature <- c("_mean", "_std")
new_feature =c()
for (i in 1:length(sleep_feature)){
  for (j in 1:length(second_feature)){
    new_feature = append(new_feature,paste(sleep_feature[i],second_feature[j],sep = ""))
  }
}
pre_col_num <- ncol(qids)
qids$sleep_day <- NA
qids[, new_feature] <-NA

for (i in 1:nrow(qids)){
  print(i)
  p_id_select <- qids$p_id[i]
  date_select <- qids$survey_date[i]
  sleep_temp <- sleep[sleep$p_id==p_id_select,]
  sleep_temp <- sleep_temp[sleep_temp$date_str >= date_select - days(win_size) & sleep_temp$date_str < date_select,]
  sleep_temp <- na.omit(sleep_temp)
  qids$sleep_day[i] <- nrow(sleep_temp)
  if (nrow(sleep_temp) >= 2){
    for (j in 1:length(sleep_feature)){
      # all days
      qids[i, (j-1) *length(second_feature) + 1 + pre_col_num + 1] = mean(sleep_temp[,j+5], na.rm = TRUE)
      qids[i, (j-1) *length(second_feature) + 2 + pre_col_num + 1] = sd(sleep_temp[,j+5], na.rm = TRUE)
    }
  }
}
# BT features ----

bt <- fread("C:/Users/k1754359/Downloads/daily_bt_feature.csv", data.table=F) %>%
  dplyr::mutate(date_str = lubridate::ymd(date_str))

bt_feature <- colnames(bt)[7:11]
new_feature =c()

for (i in 1:length(bt_feature)){
  for (j in 1:length(second_feature)){
    new_feature = append(new_feature,paste(bt_feature[i], second_feature[j],sep = ""))
  }
}

pre_col_num <- ncol(qids)
qids$bt_day <- NA
qids[, new_feature] <-NA

for (i in 1:nrow(qids)){
  print(i)
  p_id_select <- qids$p_id[i]
  date_select <- qids$survey_date[i]
  bt_temp <- bt[bt$p_id==p_id_select,]
  bt_temp <- bt_temp[bt_temp$date_str >= date_select - days(win_size) & bt_temp$date_str < date_select,]
  bt_temp <- bt_temp[!is.na(bt_temp$bt_available_day), ]
  qids$bt_day[i] <- sum(bt_temp$bt_available_day >=12)
  bt_temp <- bt_temp[bt_temp$bt_available_day >=12,]
  
  if (nrow(bt_temp)>=2){
    for (j in 1:length(bt_feature)){
      # all day
      qids[i, (j-1) *length(second_feature) + 1 + pre_col_num + 1] = mean(bt_temp[,j+6], na.rm = TRUE)
      qids[i, (j-1) *length(second_feature) + 2 + pre_col_num + 1] = sd(bt_temp[,j+6], na.rm = TRUE)
      
    }
    
  }
}

# GPS ----
gps <- fread("C:/Users/k1754359/Downloads/daily_GPS_feature.csv", data.table=F) %>%
  dplyr::mutate(date_str = lubridate::ymd(date_str))

gps_feature <- colnames(gps)[7:9]
new_feature =c()
for (i in 1:length(gps_feature)){
  for (j in 1:length(second_feature)){
    new_feature = append(new_feature,paste(gps_feature[i], second_feature[j],sep = ""))
  }
}
pre_col_num <- ncol(qids)
qids$gps_day <- NA
qids[, new_feature] <-NA

for (i in 1:nrow(qids)){
  print(i)
  p_id_select <- qids$p_id[i]
  date_select <- qids$survey_date[i]
  gps_temp <- gps[gps$p_id==p_id_select,]
  gps_temp <- gps_temp[gps_temp$date_str >= date_select - days(win_size) & gps_temp$date_str < date_select,]
  gps_temp <- gps_temp[!is.na(gps_temp$gps_available), ]
  
  qids$gps_day[i] <- sum(gps_temp$gps_available >=12)
  gps_temp <- gps_temp[gps_temp$gps_available >=12,]
  
  if (nrow(gps_temp)>=1){
    for (j in 1:length(gps_feature)){
      # all day
      qids[i, (j-1) *length(second_feature) + 1 + pre_col_num + 1] = mean(gps_temp[,j+6], na.rm = TRUE)
      qids[i, (j-1) *length(second_feature) + 2 + pre_col_num + 1] = sd(gps_temp[,j+6], na.rm = TRUE)
    }
  }
}




qids_temp <- qids %>%
  filter(sleep_day >=2)

fit <-lmer(qids_total ~ qids_temp[, 62] + (1 |p_id), qids_temp)
summary(fit)




### Multilevel Regression models -----

hist(qids$qids_total)
hist(qids_temp$qids_total)


basic.lm <- lm(qids_total ~ total_sleep_time_std, data = qids)
summary(basic.lm)


fit <-lmer(qids_total ~ sleep_onset_mean + (1 |p_id),data = qids)
summary(fit)



(split_plot <- ggplot(aes(sleep_onset_std, qids_total), data = qids_temp) + 
    geom_point() + 
    facet_wrap(~ p_id) + # create a facet for each mountain range
    xlab("feature") + 
    ylab("QIDS"))

(prelim_plot <- ggplot(qids_temp, aes(x = sleep_onset_mean, y = qids_total)) +
    geom_point())

#loop for mixed linear models

x<-25
for (i in 25:62) {
  fit <-lmer(qids_total ~ qids_temp[[i]] + (1 |p_id),data = qids_temp)
  print(x)
  x<-x+1
  print(names(qids_temp)[i])
  print(summary(fit))
  # print(coef(summary(fit)))
  # print(summary(fit)$coefficients[2,"Pr(>|t|)"])
}


#loop for correlation coeffs

library(corrplot)
x <- cor(qids_temp[25:30])
corrplot(x, type="upper", order="hclust")

cor.test(qids_temp[25:28], qids_temp$qids_total) 

for (i in 25:62) {
  corr <- cor.test(qids_temp$qids_total, qids_temp$qids_temp[[i]])
  print(x)
  x<-x+1
  print(names(qids_temp)[i])
  print(corr$estimate)
  print(corr$p.value)
  # print(coef(summary(fit)))
  # print(summary(fit)$coefficients[2,"Pr(>|t|)"])
}


#extract correlations and p values
r <- cor(qids_temp$qids_total, qids_temp[25:28])
r

p <- sapply(qids_temp[,25:28], FUN=function(x, y) cor.test(x, y)$p.value, y=qids_temp$qids_total)
p

rbind(r, p)


corr <- cor.test(qids_temp$qids_total, qids_temp$sleep_onset_mean)
corr$p.value
scatter.smooth(x=qids_temp$sleep_onset_mean, y=qids_temp$qids_total, main="plot")  # scatterplot
linearMod <- lm(qids_total ~ sleep_onset_mean, data=qids_temp)  # build linear regression model on full data
print(linearMod)
summary(linearMod)



####QIDS
#SIGNIFICANT:
#[1] "deep_pct_std"
#[1] "sleep_onset_mean"
#[1] "sleep_onset_std"
# [1] "sleep_offset_mean"

#TREND:
# [1] "sleep_efficiency_mean"
# [1] "awake_pct_mean"
# [1] "bt_max_day_std"
#[1] "bt_std_day_std"
#[1] "bt_range_day_std"


####QIDS TEMP
#SIGNIFICANT:
# deep_pct_std
#"sleep_onset_mean"
#"sleep_onset_std"
# "sleep_offset_mean"

#TREND:
#[1] "awake_pct_mean" *
# [1] "sleep_efficiency_mean *
# [1] "bt_max_day_std"
#[1] "bt_std_day_std"
#[1] "bt_range_day_std"

colnames(qids_temp)

18.470/(18.470 + 7.434) # ~ 70% is the variance explained by individual difference

plot(fit)  # looks alright, no patterns evident
qqnorm(resid(fit))
qqline(resid(fit))  # points fall nicely onto the line - good!


#### Histograms ####

hist(qids_temp$qids_total)

#### Missing values ####
colSums(is.na(qids_temp))

#which columns have more than 80% (i.e. > 411) missing values
names(qids_temp)[colSums(is.na(qids_temp))>411]
# [1] "bt_mean_day_mean"       "bt_mean_day_std"        "bt_max_day_mean"       
# [4] "bt_max_day_std"         "bt_min_day_mean"        "bt_min_day_std"        
# [7] "bt_std_day_mean"        "bt_std_day_std"         "bt_range_day_mean"     
# [10] "bt_range_day_std"       "location_variance_mean" "location_variance_std" 
# [13] "total_distance_mean"    "total_distance_std"     "hour_distance_mean"    
# [16] "hour_distance_std" 

#it may therefore be best to exclude the BT and GPS variables from the analysis.



###### Lasso regularised regression - unstandardised model 1 #####

library(glmnet)
library(caret)


#select predictor variables = x
x<-as.matrix(qids_temp[,25:62])
#select outcome variable = y
y<-qids_temp$qids_total

#t to transpose
# t(summary(x))

#Fit a penalised regression model for predicting depression outcome based on the available covariates
# using LASSO. Plot the penalised regression coefficients against the penalty values.

lasso<-glmnet(x,y, family="gaussian", alpha=1)
plot(lasso, xvar="lambda", label=TRUE)
plot_glmnet(lasso, label=5)                  # label the 5 biggest final coefs


# Use 5-fold cross-validation to find the optimal penalty. 
# Select (identify) variables that are predictive of depression
# based on the optimal LASSO model.

set.seed(101) # This command is optional, but helps getting the same answer every time the code is run

cv10<-cv.glmnet(x,y, nfold=5)
plot(cv10)
#The optimal penalty can be obtained by extracting the lambda.min 
# component of the cross-validation object
cv10$lambda.min    #0.1140423

# the LASSO coefficients at the optimal penalty can be obtained by using the 
# coef() command as shown below
coef(cv10, s="lambda.min")
# total_sleep_time_mean  -0.774381773
# time_in_bed_std         0.494869061
# light_pct_std          -0.046034800
# deep_pct_mean          -0.021796179
# deep_pct_std            0.166638614
# REM_pct_mean            0.042043450
# REM_pct_std             0.117227443
# awake_pct_std           0.002766796
# sleep_onset_mean        0.204980677
# sleep_onset_std         0.362882461
# awakenings_mean        -0.184894049
# awakenings_std          0.592813522
# bt_day                 -0.150695157



##### Lasso regularised regression - standardised model 2 ####

# Before training the model, we normalize each feature to have zeros mean and 
# one standard deviation. Feature normalization avoids the different feature 
# scales adversely affecting regularization.

#standardize the x matrix 
x_std <- scale(x)
mean(x_std[, 10])
sd(x_std[,10])

# y also standardised (not sure this is necessary)

y_std <- scale(y)
lasso<-glmnet(x_std,y_std, family="gaussian", alpha=1)
plot(lasso, xvar="lambda")


set.seed(101)
cv10<-cv.glmnet(x_std,y_std, nfold=5)
plot(cv10)


cv10$lambda.min    #0.02173938
coef(cv10, s="lambda.min")

# total_sleep_time_mean  -1.860714e-01
# time_in_bed_std         8.961934e-02
# light_pct_std          -2.067267e-02
# deep_pct_mean          -1.493869e-02
# deep_pct_std            4.484192e-02
# REM_pct_mean            3.232540e-02
# REM_pct_std             4.217351e-02
# awake_pct_std           6.554963e-04
# sleep_onset_mean        7.673804e-02
# sleep_onset_std         6.116396e-02
# awakenings_mean        -3.327695e-02
# awakenings_std          6.744254e-02
# bt_day                 -5.718160e-02


### Get the MSE for the best lambda model  ###
MSE <- (cv10$cvm)  # $cvm inlcudes the MSE of all 100 lambdas , min() selects the snmallest
print(MSE)  #0.9253454
# The Mean Squared Error (MSE) is a measure of how close a fitted line is to data points
# MSE = 0.93 suggests overfitting line?


#### Calculate the r2 of the best model

# Explained variance of unseen cases
paste("R2 is: ",  1-(MSE_min/var(y))) #0.967796286364433"
# The explained variance of predicting unseen cases is about 97%. The best model, 
# therefore, predicted 97% of the variance in unseen data. This does not mean that 
# we expect that our model will explain 9% of the data with a completely new random 
# sample: We assessed 100 different lambdas and this r2 is optimistic because we 
# performed model selection using the MSE (and therefore r2 ) of unseen cases  as 
# a selection criteria, see lecture Validation!


##### Alternative best lambda (lambda.1SE)
# This lambda penalizes slightly more than the minimum lambda
# It selects a more parsimonious model with almost the same MSE

coef(cv10, s="lambda.1se")  #selects much fewer variables
# total_sleep_time_mean  -5.513739e-02
# sleep_onset_mean        6.261432e-02
# sleep_onset_std         1.375154e-02

# Addtional information: Get MSE for minimum + 1SE lambda
MSE_1se<- cv10$cvm[which(cv10$lambda == cv10$lambda.1se)]
MSE_1se #0.9645882

paste("R2 is: ",  1-(MSE_1se/var(y)))  # 0.964948670789199"


###Now I need to compare two models (the baseline and the predictor) but not sure how to do that. 
# We report the mean absolute error
# and correlations between the predicted PHQ-8 scores and the groundtruth. 
# We use the population PHQ-8 mean as the prediction baseline to compare the prediction performance.
# The MAE of the baseline model is 4.29. Our prediction model predict
# the pre PHQ-8 scores with MAE of 3.60, which is 0.59 lower than the baseline. 
# The predicted PHQ-8 score strongly correlate with the groundtruth with r = 0.578,p < 0.001


#### Cross-validation grouped by participant####
install.packages("groupdata2")
library(groupdata2)



#### Regression model with selected (sig) features ####

# (a) --- MODEL WITH ONLY SIG FEATURES  ---

#select predictor variables = x
sig_x <- qids_temp %>% 
  select("deep_pct_std", 
         "sleep_onset_mean", 
         "sleep_onset_std", 
         "sleep_offset_mean")

x<-as.matrix(sig_x)
#select outcome variable = y
y<-qids_temp$qids_total


#standardize the variables
x_std <- scale(x)
y_std <- scale(y)

mean(x_std[, 1])
sd(x_std[,1])

lasso<-glmnet(x_std,y_std, family="gaussian", alpha=1)
plot(lasso, xvar="lambda")

set.seed(101)
cv10<-cv.glmnet(x_std,y_std, nfold=10)
plot(cv10)
cv10$lambda.min    #0.0006337146
coef(cv10, s="lambda.min")
# all 4 variables are selected.

# a normal multiple regression
# model <- lm(qids_total ~ deep_pct_std + sleep_onset_mean + sleep_onset_std + sleep_offset_mean, data = qids_temp)
# summary(model)
# all variables are significant (trend for deep_pct_std)


### Get the MSE for the best lambda model  ###
MSE <- min(cv10$cvm)
print(MSE)  # 0.9199701
# The Mean Squared Error (MSE) is a measure of how close a fitted line is to data points
MSE<- cv10$cvm[which(cv10$lambda == cv10$lambda.min)]
MSE


# Calculate the r2 of the best model
# Explained variance of unseen cases
paste("R2 is: ",  1-(MSE_min/var(y))) #0.967796286364433"


#  (b) --- MODEL WITH SIG AND TREND FEATURES  ---

#select predictor variables = x - significant and TREND
sig_x <- qids_temp %>% 
  select("deep_pct_std", 
         "sleep_onset_mean", 
         "sleep_onset_std", 
         "sleep_offset_mean",
         "deep_pct_std",
         "sleep_onset_mean",
         "sleep_onset_std",
         "sleep_offset_mean",
         "awake_pct_mean",
         "sleep_efficiency_mean",
         "bt_max_day_std",
         "bt_std_day_std",
         "bt_range_day_std")

x<-as.matrix(sig_x)
y<-qids_temp$qids_total


#standardize the variables
x_std <- scale(x)
y_std <- scale(y)

mean(x_std[, 1])
sd(x_std[,1])

lasso<-glmnet(x_std,y_std, family="gaussian", alpha=1)
plot(lasso, xvar="lambda")

set.seed(101)
cv10<-cv.glmnet(x_std,y_std, nfold=10)
plot(cv10)
cv10$lambda.min    #0.0006337146 - same
coef(cv10, s="lambda.min")
# selected variables:
# deep_pct_std           7.831076e-02
# sleep_onset_mean       3.431133e-01
# sleep_onset_std        1.263320e-01
# sleep_offset_mean     -2.008403e-01
# awake_pct_mean         4.191298e-02
# sleep_efficiency_mean -6.938894e-18


### Get the MSE for the best lambda model  ###
MSE <- (cv10$cvm)  
print(min(MSE))  # 0.9251753
# The Mean Squared Error (MSE) is a measure of how close a fitted line is to data points

# Calculate the r2 of the best model
paste("R2 is: ",  1-(MSE_min/var(y))) #0.967796286364433"



####
#There isn't any difference between the model pre and post multilevel regression analyses. 
####


##### MISIING DATA --------

#inexplicably, the glmnet model only runs in my work PC, not in my personal laptop.

# (1) exclusion of missing data variables - missing data is too numerous.
# BT and GPS variables excluded #

names(qids_temp)
x<-as.matrix(qids_temp[,25:44])
y<-qids_temp$qids_total

#standardize 
x_std <- scale(x)
y_std <- scale(y)

lasso<-glmnet(x_std,y_std, family="gaussian", alpha=1)
plot(lasso, xvar="lambda", label=TRUE)
plot_glmnet(lasso, label=5)                  # label the 5 biggest final coefs

set.seed(101)
cv10<-cv.glmnet(x_std,y_std, nfold=10)
plot(cv10)

cv10$lambda.min    #0.03461525
coef(cv10, s="lambda.min")

# total_sleep_time_mean -1.741682e-01
# time_in_bed_std        7.868866e-02
# deep_pct_std           3.468861e-02
# REM_pct_mean           2.768565e-02
# REM_pct_std            2.721481e-02
# sleep_onset_mean       8.057596e-02
# sleep_onset_std        6.145526e-02
# awakenings_mean       -1.202504e-02
# awakenings_std         4.842135e-02


MSE <- min(cv10$cvm)   # MSE for  best lambda model
print(MSE)  # 0.9293589

MSE_min<- cv10$cvm[which(cv10$lambda == cv10$lambda.min)]
MSE_min

# Calculate the r2 of the best model - Explained variance of unseen cases
paste("R2 is: ",  1-(MSE_min/var(y))) #0.966228835580019


## (2) Missing data imputation through column means 
#the result is practically the same.

x<-as.matrix(qids_temp[,25:62])
y<-qids_temp$qids_total

x_std <- scale(x)
y_std <- scale(y)

x<-as.data.frame(x)
#We impute missing values with column means by passing na.impute = TRUE
x<-as.matrix(makeX(x, na.impute = TRUE))

lasso<-glmnet(x_std,y_std, family="gaussian", alpha=1)
plot(lasso, xvar="lambda", label=TRUE)

set.seed(101)
cv10<-cv.glmnet(x_std,y_std, nfold=10)
plot(cv10)

cv10$lambda.min    #0.03799019
coef(cv10, s="lambda.min")
# total_sleep_time_mean  -1.639747e-01
# time_in_bed_std         7.556667e-02
# deep_pct_std            3.087484e-02
# REM_pct_mean            1.024903e-02
# REM_pct_std             2.340290e-02
# sleep_onset_mean        7.801681e-02
# sleep_onset_std         5.955959e-02
# awakenings_mean        -1.605103e-02
# awakenings_std          4.331626e-02
# bt_day                 -4.518093e-02


MSE <- min(cv10$cvm)   # MSE for  best lambda model
print(MSE)  # 0.9275304

MSE_min<- cv10$cvm[which(cv10$lambda == cv10$lambda.min)]
MSE_min

# Calculate the r2 of the best model - Explained variance of unseen cases
paste("R2 is: ",  1-(MSE_min/var(y))) #0.966295279582395



# (3) Full Information maximum-likelihood imputation method - Missing at Random


x<-as.matrix(qids_temp[,25:62])
y<-qids_temp$qids_total

x_std <- scale(x)
y_std <- scale(y)

impx <- normImp(as.data.frame(x_std), M = 1, pd = FALSE, steps = 100, rseed= 101)

lasso<-glmnet(impx,y_std, family="gaussian", alpha=1)
plot(lasso, xvar="lambda", label=TRUE)
plot_glmnet(lasso, label=7)                  # label the 7 biggest final coefs

set.seed(101)
cv10<-cv.glmnet(as.matrix(impx),y_std, nfold=10)
plot(cv10)

cv10$lambda.min    #0.2400481 - non standardised lambda
                   #0.04169419 - std x and y variables

coef(cv10, s="lambda.min")
# total_sleep_time_mean  -0.60613109
# time_in_bed_std         0.39919494
# deep_pct_std            0.08627641
# REM_pct_std             0.04094580
# sleep_onset_mean        0.22375694
# sleep_onset_std         0.32902749
# awakenings_mean        -0.03305565
# awakenings_std          0.30790444
# bt_day                 -0.09933288
# bt_min_day_mean         0.01773057
# location_variance_std  -0.06976327


# standadrd variable coefficients
# total_sleep_time_mean  -0.147614275
# time_in_bed_std         0.070841381
# deep_pct_std            0.024292815
# REM_pct_mean            0.004915488
# REM_pct_std             0.021066666
# sleep_onset_mean        0.081882686
# sleep_onset_std         0.057253593
# awakenings_mean        -0.006956188
# awakenings_std          0.031568538
# bt_day                 -0.038541278
# bt_max_day_std          0.009607733
# bt_min_day_mean         0.047925721


MSE <- min(cv10$cvm)   # MSE for  best lambda model_std
print(MSE)  # 0.9351612

MSE_min<- cv10$cvm[which(cv10$lambda == cv10$lambda.min)]
MSE_min
# Calculate the r2 of the best model - Explained variance of unseen cases_ std
paste("R2 is: ",  1-(MSE_min/var(y))) #0.966017991483581



#### Cross-sectional analyses of baseline data ------
library(readxl)
IDmap <- read_excel("//kclad.ds.kcl.ac.uk/anywhere/UserData/PSStore01/k1754359/My Documents/R/4. aRMT data.xlsx", sheet = "IDmap")
merged <- merge(qids_temp, IDmap, by.x=c("p_id"), by.y=c("participant_id"))
View(merged)

merged$week <- floor(as.numeric(difftime(merged$survey_date, merged$date_assessment, units = "weeks")))

#how many unique IDs? - 54
length(unique(merged$record_id))

#how many rows does each P have?
# merged %>%
#   group_by(record_id) %>%
#   summarise(count = n()) %>%
#   View()


#pick the lowest count of week 
baseline <- merged %>% 
  group_by(record_id) %>%
  slice_min(week) %>% 
  #keep ony those Ps whose lowest week n. is week 1 or 2
  subset(week < 3) # only 34 participants left

#delete the bt and GPS columns
base_sleep <- baseline[-c(2, 3, 45:62)]

#run analysis
x<-as.matrix(base_sleep[,23:42])
y<-base_sleep$qids_total

x_std <- scale(x)
y_std <- scale(y)

lasso<-glmnet(x_std,y_std, family="gaussian", alpha=1)
plot(lasso, xvar="lambda", label=TRUE)
plot_glmnet(lasso, label=7)                  # label the 7 biggest final coefs

set.seed(101)
cv10<-cv.glmnet(x_std,y_std, nfold=5)
plot(cv10)
cv10$lambda.min    #0.2712393 

coef(cv10, s="lambda.min") #no variables were selected.

MSE <- min(cv10$cvm)   # MSE for  best lambda model_std
print(MSE)  # 0.9351612
paste("R2 is: ",  1-(MSE_min/var(y))) #0.942383871615379

#a normal multiple regression on baseline qids (using only sleep data) is not significant#####
model <- lm(qids_total ~ deep_pct_std + sleep_onset_mean + sleep_onset_std + sleep_offset_mean, data = base_sleep)
summary(model)


