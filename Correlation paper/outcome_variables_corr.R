
### prepare the outcome variables:

 # - as continuous scales
# 1. QIDS
# 2. PHQ9
# 3. GAD7
# 4. WSAS
# 
# - as binary outcomes
# 5. QIDS_binary
# 6. PHQ_binary
# 7. GAD_binary
# 8. WSAS_binary



# 1. QIDS

qids <- fread("Downloads/QIDS_summarized.csv", data.table=F) %>%
  dplyr::mutate(survey_date = lubridate::ymd(survey_date))
qids$response_time = qids$response_time/60
qids$complete_time = qids$complete_time/60

# 5. QIDS binary
#split by QIDS = 10/11
#50% of the sample lies on each side in qids_temp and qids
#this is the validated split indicating moderate depression

hist(qids$qids_total)
h<-graph.freq(qids$qids_total,plot=FALSE)
points<-ogive.freq(h,col="red",frame=FALSE,
                   xlab="QIDS", ylab="Accumulated relative frequency", main="Cumulative frequency")


tbl <- table(qids$qids_total)   #change dataset to QIDS_temp?
n <- nrow(qids)                 #change dataset
cbind(Freq=tbl, Cumul=cumsum(tbl), Cumul.prop=cumsum(tbl*100/n))

qids <- qids %>%
  mutate(qids_binary = ifelse(qids_total >9, 1, 0))
  
