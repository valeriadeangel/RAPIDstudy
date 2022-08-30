
#######################
##### REGRESSION with TEST / TRAIN SETS #######
#######################

#second_order_feature_extraction script needs to be run before executing the following script. 



#### SPLIT 1. validating the model with an 80/20 total split: ####
#### ### ###### ### #### ### ###### ### #### ### ###### ###

# 1)	Randomly order the dataset
rows <- sample(nrow(qids))
qids_r <- qids[rows, ]
# 2)	Find a row to split the data on:
#for a 80/20 split:
split_prcent <- 0.8
split <- round(nrow(qids_r) * split_prcent)
train <- qids_r[1:split, ]
test <- qids_r[(split + 1): nrow(qids_r), ]
#confirm test set size:
nrow(train) / nrow(qids_r)


#Fit a model to the data
model <- lm(qids_total ~ sleep_onset_mean, train) #we pick the first 47 ids = 515 rows only
#predict in-sample - make in-sample prediction
predicted <- predict(model, train, type = "response") 
#then calculate the RMSE 
actual <- train[,"qids_total"] # this is the first 515 rows on the y variable
sqrt(mean((predicted-actual) ^2, na.rm = TRUE))
# RMSE = 5.050575

#out-of sample validation
#next we test this on a new dataset - the last observations 
predicted <- predict(model, test, type = "response") 

#Finally, we evaluate the RMSE on the test set by comparing predictions from our model to the actual mpg values for the test set 
actual <- test[,"qids_total"] 
sqrt(mean((predicted-actual) ^2, na.rm = TRUE))
# RMSE = 5.211082

#RMSE on in-sample = 5.05, RMSE on new data = 5.21 - not too much of a difference.



####  SPLIT 2. validating with 80/20 PARTICIPANT split: ####
#### ### ###### ### #### ### ###### ### #### ### ###### ###


#find out how many rows each participant contributes - cumulative frequency
qids_count <- qids %>%
  group_by(p_id) %>%
  summarise(count = n()) %>% 
  mutate(csum = cumsum(count)) %>% 
  mutate(prop.csum = csum/sum(count)) %>% 
  View()

#how many participants would be on either side of the 80/20 split?
count(qids_count) #total ids - n = 59

59*0.8 # 47 p_ids on the train set
59*0.2 # 12 on the test set


# 1) separate sample by participants
train <- qids[1:515, ]
test <- qids[516:nrow(qids), ]


# 2)	Randomly order the dataset
rows <- sample(nrow(test))
test <- test[rows, ]

rows <- sample(nrow(train))
train <- train[rows, ]
#confirm test set size:
nrow(train) / nrow(qids)


#Fit a model to the data
model <- lm(qids_total ~ sleep_onset_mean, train) #we pick the first 47 ids = 515 rows only
#predict in-sample - make in-sample prediction
predicted <- predict(model, train, type = "response") 
#then calculate the RMSE 
actual <- train[,"qids_total"] # this is the first 515 rows on the y variable
sqrt(mean((predicted-actual) ^2, na.rm = TRUE))
# RMSE = 5.32568

#out-of sample validation
#next we test this on a new dataset - the last observations 
predicted <- predict(model, test, type = "response") 

#Finally, we evaluate the RMSE on the test set by comparing predictions from our model to the actual mpg values for the test set 
actual <- test[,"qids_total"] 
sqrt(mean((predicted-actual) ^2, na.rm = TRUE))
# RMSE = 4.097718

#RMSE on in-sample = 5.33, RMSE on new data = 4.10 - it performs better!



###  SPLIT 3. first 20 are test, last 80 are train  ####
#### ### ###### ### #### ### ###### ### #### ### ### ###

 
#checking the cumulative frequency, the first 20% of participant rows go up to row 122

# 1) separate sample by participants
test <- qids[1:122, ]
train <- qids[123:nrow(qids), ]


# 2)	Randomly order the dataset
rows <- sample(nrow(test))
test <- test[rows, ]

rows <- sample(nrow(train))
train <- train[rows, ]
#confirm test set size:
nrow(train) / nrow(qids)


#Fit a model to the data
model <- lm(qids_total ~ sleep_onset_mean, train) #we pick the first 47 ids = 515 rows only
#predict in-sample - make in-sample prediction
predicted <- predict(model, train, type = "response") 
#then calculate the RMSE 
actual <- train[,"qids_total"] # this is the first 515 rows on the y variable
sqrt(mean((predicted-actual) ^2, na.rm = TRUE))
# RMSE = 4.778693

#out-of sample validation
#next we test this on a new dataset - the last observations 
predicted <- predict(model, test, type = "response") 

#Finally, we evaluate the RMSE on the test set by comparing predictions from our model to the actual mpg values for the test set 
actual <- test[,"qids_total"] 
sqrt(mean((predicted-actual) ^2, na.rm = TRUE))
# RMSE = 6.36253

#RMSE on in-sample = 4.78, RMSE on new data = 6.36 - much worse.




##### Cross validation with caret #####
### ### ### ### ### ### ### ### ### ###

# Dealing with missing data
# (1) We consider the data is MAR so first we replace NAs with medians

#split target from predictors:
y<-qids$qids_total
x<-as.matrix(qids[,25:62])

names(qids)

#fit caret models

set.seed(101)
rft_median_model <- train(
  x = x, 
  y = y,
  method = "rf", #can be changed to rf = random forest. All code remains same
  trControl = trainControl(
    method = "cv", 
    number = 10,
    verboseIter = TRUE ), #verbose gives us updates or running code
  preProcess = c("medianImpute")
)

# for RF models only
plot(rft_median_model)
varImp(rft_median_model)

glm_median_model
# Generalized Linear Model 
# 
# 635 samples
# 38 predictor
# 
# Pre-processing: median imputation (38) 
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 573, 572, 570, 571, 572, 572, ... 
# Resampling results:
#   
#   RMSE      Rsquared    MAE     
# 5.366835  0.07497354  4.268011

glmnet_median_model
# glmnet 
# 
# 635 samples
# 20 predictor
# 
# Pre-processing: median imputation (20) 
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 571, 572, 571, 571, 572, 573, ... 
# Resampling results across tuning parameters:
#   
#   alpha  lambda      RMSE      Rsquared    MAE     
# 1.00   0.23121503  5.242637  0.08064520  4.265654
# 
# RMSE was used to select the optimal model using the smallest value.
# The final values used for the model were alpha = 1 and lambda = 0.231215.

rf_median_model
# Random Forest 
# 
# 635 samples
# 38 predictor
# 
# Pre-processing: median imputation (38) 
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 571, 572, 571, 571, 572, 573, ... 
# Resampling results across tuning parameters:
#   
#   mtry  RMSE      Rsquared   MAE     
# 2    4.953004  0.1766356  4.021906
# 20    4.829976  0.1961542  3.878556
# 38    4.799753  0.2026859  3.851100
# 
# RMSE was used to select the optimal model using the smallest value.
# The final value used for the model was mtry = 38.

# 
# rf variable importance
# 
# only 20 most important variables shown (out of 38)
# 
# Overall
# sleep_onset_mean       100.00
# light_pct_mean          40.28
# time_in_bed_mean        39.40
# sleep_offset_mean       38.18
# total_sleep_time_mean   34.61
# awakenings_mean         34.26
# sleep_onset_std         33.04
# REM_pct_mean            31.76
# deep_pct_mean           29.74
# REM_pct_std             28.48
# deep_pct_std            26.83
# light_pct_std           23.93
# sleep_offset_std        23.03
# time_in_bed_std         21.59
# awakenings_std          20.83
# total_sleep_time_std    18.35
# awake_pct_mean          16.55
# sleep_efficiency_std    14.34
# awake_pct_std           13.32
# sleep_efficiency_mean   12.67

#knn Impute doesn't work


##
##  Random forests create best prediction models. 
##


#now make predictions on new data
predict(glm_median_model, qids)


