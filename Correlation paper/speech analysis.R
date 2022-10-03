

options(scipen=999)
# options(scipen=0) # to revert scientific notation


getwd()
#load from IoP:
setwd("C:/Users/k1754359/OneDrive - King's College London/PhD/6. Correlation Digital signals in Depression/R scripts")

#load from home:
setwd("C:/Users/k1754359/Downloads")

IDmap <- read_excel("4. aRMT data.xlsx", sheet = "IDmap")

qids <- fread("QIDS_summarized_updated.csv", data.table=F) %>%
  dplyr::mutate(survey_date = lubridate::ymd(survey_date))


speech <- fread("speech-Unscripted-25-09-2022.csv", data.table=F) %>%
  mutate(date_str = as.Date(date_str, tz = NULL, formats = NULL))

speech <- fread("speech-Scripted-25-09-2022.csv", data.table=F) %>%
  mutate(date_str = as.Date(date_str, tz = NULL, formats = NULL))


#speech needs event_name and date_of assessment
speech_merge <- merge(speech, IDmap[,2:3], by.x=c("p_id"), by.y=c("participant_id"), all = TRUE, incomparables = T)
speech_event <- speech_merge %>% 
  mutate(event_name = as.double(difftime(date_str,date_assessment, units = "weeks")))
speech_event$event_name <- round(speech_event$event_name, digits = 0)
names(speech_event)

speech_event$Task[1]

#

# Speech features QIDS---- 
# merging speech with QIDS
qids_merge <- merge(qids_all, IDmap[,2:3], by = c("p_id")) %>%
  select(-date_assessment.y) %>%
  rename(date_assessment = date_assessment.x)
names(qids_merge)

#create event_name for QIDS
qids_event <- qids_merge %>% 
  mutate(event_name = as.double(difftime(survey_date,date_assessment, units = "weeks")))
qids_event$event_name <- round(qids_event$event_name, digits = 0)




# Speech features GAD ---- 

#GAD needs p_id
gad <- merge(gad_select, IDmap[, c("participant_id", "record_id")]) %>%
  rename(p_id = participant_id) %>%
  rename(event_name = redcap_event_name)

#merge speech and GAD
m_speech <- merge(speech_event, gad, by = c("p_id", "event_name"))
m_speech <- m_speech %>% 
  filter(p_id != "f5978923-cef2-4eeb-a49c-7d79be4b53eb", p_id != "5786af5e-99e3-4f78-848a-d3b67b8eb7ed")

#use GAD df but add qids total as covariate
gad_qidstot <- merge(m_speech, qids_event[,c("p_id", "event_name", "qids_total")], by = c("p_id", "event_name"), all.x = TRUE, all.y = FALSE)
names(gad_qidstot)

speech <- gad_qidstot
#

# Speech features WSAS ---- 

#merge speech and wsas
m_speech <- merge(speech_event, wsas, by = c("p_id", "event_name"))
m_speech <- m_speech %>% 
  filter(p_id != "f5978923-cef2-4eeb-a49c-7d79be4b53eb", p_id != "5786af5e-99e3-4f78-848a-d3b67b8eb7ed") %>%
  select(-date_assessment.y) %>%
  rename(date_assessment = date_assessment.x)

# add qids total as covariate
wsas_qidstot <- merge(m_speech, qids_event[,c("p_id", "event_name", "qids_total")], by = c("p_id", "event_name"), all.x = TRUE, all.y = FALSE)
names(wsas_qidstot)
speech <- wsas_qidstot


# Speech features analysis ---- 

a <- as.data.frame((sapply(wsas_qidstot, function(x) sum(is.na(x))))/nrow(wsas_qidstot))
a <- as.data.frame(which(a > .5, arr.ind = TRUE))


names(speech)


other_vars <- c(1:5, 33:length(speech)) # columns that are not predictor variables

length(speech)        # 54
length(other_vars)    # 26
54-26                 # QIDS 28 predictor variables left 
47-20                 # GAD 27 predictor variables left 

inc_vars <- speech[, -c(other_vars)]
#

# --- correlation plot ----
m <- cor(inc_vars, use = "complete.obs",
         method = "spearman")

mat <- inc_vars
# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(inc_vars)
head( round( p.mat[, 1:10], digits=4))

# Leave blank on non-significant coefficient
corrplot(m, type="upper", order="hclust",
         p.mat = p.mat, sig.level = 0.05, insig = "blank")


#extract correlations and p values
r <- cor(speech$qids_total, inc_vars, use = "complete.obs",
         method = "spearman")
r
p <- sapply(inc_vars, FUN=function(x, y) cor.test(x, y)$p.value, y=speech$qids_total)
p

cors <- as.data.frame(rbind(r, p))
sig_cors <- as.data.frame(t(cors)) %>%
  rename(cor_coef = V1) %>%
  mutate(sig = ifelse(p < 0.01, "**",ifelse(p < 0.05, "*","_"))) %>%
  arrange(desc(cor_coef))

sig_cors[sig_cors$p < 0.05, ] 

### Multilevel Regression models -----


#  ### ### ### ###  ###
### LOAD DESCRIPTIVES.R ###
#  ### ### ### ###  ###



#combine the digital+qids features to demographic variables
d <-merge(speech, demographics[c(1, 14,23,24,25,26)])
names(d)

## conduct the models adjusting for Age, gender, (and depression)

data = d         #     <- - - - select the data to be used for the models

for (i in 5:32) {    
  fit <-lmer(total_gad              
             ~ data[[i]]          
             + Age + gender + qids_total               #covariates
             + (1 |p_id),data = data)  
  print(toupper(names(data)[i]))
  # print(coef(summary(fit)))
  print(round(coef(summary(fit)), digits = 6))
  # print(round(confint(fit), digits = 6))
}

fit <-lmer(qids_total ~ deep_pct_std +  Age + gender + (1 |p_id), data = data)
summary(fit)
confint(fit)
demo_total <- merge(demographics, baseline_qids, all = TRUE)

