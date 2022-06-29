install.packages(c("rstatix", "ggpubr", "AICcmodavg", "broom"))

# Library
library(fmsb)
library(rstatix)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

# PREPARE BASELINE CLINICAL DATA ------------------------------------------

#summarise clinical data
X2_Demographic_Table <- read_excel("~/R/2. Demographic Table.xlsx", 3)
ls(X2_Demographic_Table)


## study outcome x clinical state
subset1 <- X2_Demographic_Table %>%
  select(record_ID,`Ended by`, `Total PHQ 9`, `Total GAD7`)    # PHQ + WSAS

wsas <- read_excel("~/R/4. aRMT data.xlsx", 7)                               # add in WSAS

merged <- merge(subset1, wsas, by = "record_ID")                             # merge
merged$WSAS <- as.numeric(merged$WSAS)


base_clin <- read_excel("~/R/2. Demographic Table.xlsx", 8)         #add all other baseline clinical data
base_totals <- base_clin %>%
  select(record_ID, audit_sum, life_stress_total, oslo_sum, pss_total, 
         qids_total, sapas_total, sc_social_participation_total, 
         sd_total, sum_rss, total_bipq, wsas_total, ctq_total)

base_totals <- data.frame(lapply(base_totals, function(x) as.numeric(as.character(x))))   # convert variables to NUMERIC
#View(base_totals)

baseline_clin <- merge(merged, base_totals, by = "record_ID", all.x = TRUE, all.y=TRUE, no.dups = TRUE, incomparables = NULL)                         # merge

names(baseline_clin)[names(baseline_clin) == "Total PHQ 9"] <- 'phq9'     # rename
names(baseline_clin)[names(baseline_clin) == "Total GAD7"] <- 'gad7'     # rename
names(baseline_clin)[names(baseline_clin) == "WSAS"] <- 'app_wsas'     # rename

names(baseline_clin)


baseline_clin <- baseline_clin %>%                                   # convert Ended by to factor (categorical) with 3 levels
  mutate(`Ended by` = factor(as.character(`Ended by`),
                           levels = c("lost to follow up", "withdrawal", "completed"),
                           labels = c("lost to follow up", "withdrawal", "completed")))


#count NAs per column
na_count <-sapply(baseline_clin, function(y) sum(is.na(y)))
na_count <- data.frame(na_count)
na_count$names <- rownames(na_count)


col_means <- sapply(baseline_clin, function(y) mean(y, na.rm = TRUE))
col_means <- data.frame(col_means)
col_means$names <- rownames(col_means)


#write.csv(baseline_clin,"~/R/baseline_clin.csv", row.names = FALSE)

glimpse(baseline_clin)

#  #  # FINAL DATA FRAME:  baseline_clin #  #  #


# Create tables by Study end ----------------------------------------------

# Prepare data: clinical baseline for WSAS, PHQ and GAD x Study End 
clin_table <- merged %>%
  group_by(`Ended by`) %>%
  summarise(mean_wsas = mean(WSAS, na.rm = TRUE), mean_phq = mean(`Total PHQ 9`), mean_gad = mean(`Total GAD7`)) %>%
  mutate(wsas_std = mean_wsas/40, phq_std = mean_phq/27, gad_std = mean_gad/21)
#write.csv(clin_table,"~/R/clin_table.csv", row.names = FALSE) 
names(base_clin)



# Prepare data: clinical baseline x Study End  ~~ RAW

endedby_raw <- baseline_clin %>%                            #extract the means for each variable
  group_by(`Ended by`) %>%
  summarise(mean_sapas = mean(sapas_total, na.rm = TRUE),
            mean_bipq = mean(total_bipq, na.rm = TRUE),
            mean_stress = mean(life_stress_total, na.rm = TRUE),
            mean_ctq = mean(ctq_total, na.rm = TRUE),
            mean_phq9 = mean(phq9, na.rm = TRUE),
            mean_gad7 = mean(gad7, na.rm = TRUE),
            mean_rss = mean(sum_rss, na.rm = TRUE),
            mean_audit = mean(audit_sum, na.rm = TRUE),
            mean_oslo = mean(oslo_sum, na.rm = TRUE),
            mean_pss = mean(pss_total, na.rm = TRUE),
            mean_sd = mean(sd_total, na.rm = TRUE),
            mean_sc = mean(sc_social_participation_total, na.rm = TRUE),
            mean_qids = mean(qids_total, na.rm = TRUE),
            mean_wsas = mean(wsas_total, na.rm = TRUE),
            mean_appwsas = mean(app_wsas, na.rm = TRUE))
#write.csv(endedby_raw,"~/R/ended by.csv", row.names = FALSE) 



# # Standardised version --(not using atm)--------------------------------------------------
# # create z scores for each variable
# zphq9 <- scale(baseline_clin$phq9)
# zgad7 <- scale(baseline_clin$gad7)
# zapp_wsas <- scale(baseline_clin$app_wsas)
# zaudit_sum <- scale(baseline_clin$audit_sum)
# zlife_stress_total <- scale(baseline_clin$life_stress_total)
# zoslo_sum <- scale(baseline_clin$oslo_sum)
# zpss_total <- scale(baseline_clin$pss_total)
# zqids_total <- scale(baseline_clin$qids_total)
# zsapas_total <- scale(baseline_clin$sapas_total)
# zsc_social_participation_total <- scale(baseline_clin$sc_social_participation_total)
# zsd_total <- scale(baseline_clin$sd_total)
# zsum_rss <- scale(baseline_clin$sum_rss)
# ztotal_bipq <- scale(baseline_clin$total_bipq)
# zwsas_total <- scale(baseline_clin$wsas_total)
# zctq_total <- scale(baseline_clin$ctq_total)
# 
# 
# zscores <- cbind(zphq9,                         #combine zscore varibles
#                  zgad7,
#                  zapp_wsas,
#                  zaudit_sum,
#                  zlife_stress_total,
#                  zoslo_sum,
#                  zpss_total,
#                  zqids_total,
#                  zsapas_total,
#                  zsc_social_participation_total,
#                  zsd_total,
#                  zsum_rss,
#                  ztotal_bipq,
#                  zwsas_total,
#                  zctq_total)
# 
# 
# zscores <- cbind(zscores, baseline_clin$`Ended by`)  # add ENDED BY
# zscores <- as.data.frame(zscores)                  # turn into data frame
# 
# #write.csv(zscores2,"~/R/zcsores2.csv", row.names = FALSE) 
# 
# #test <- data.frame(lapply(zscores, function(x) as.numeric(as.character(x))))   # convert variables to NUMERIC
# #View(test)
# 
# 
# zscore_means <- zscores %>%                            #extract the means for each variable
#   group_by(V16) %>%
#   summarise(mean_sapas = mean(zsapas_total, na.rm = TRUE),
#             mean_bipq = mean(ztotal_bipq, na.rm = TRUE),
#             mean_stress = mean(zlife_stress_total, na.rm = TRUE),
#             mean_ctq = mean(zctq_total, na.rm = TRUE),
#             mean_phq9 = mean(zphq9, na.rm = TRUE),
#             mean_gad7 = mean(zgad7, na.rm = TRUE),
#             mean_rss = mean(zsum_rss, na.rm = TRUE),
#             mean_audit = mean(zaudit_sum, na.rm = TRUE),
#             mean_oslo = mean(zoslo_sum, na.rm = TRUE),
#             mean_pss = mean(zpss_total, na.rm = TRUE),
#             mean_sd = mean(zsd_total, na.rm = TRUE),
#             mean_sc = mean(zsc_social_participation_total, na.rm = TRUE),
#             mean_qids = mean(zqids_total, na.rm = TRUE),
#             mean_wsas = mean(zwsas_total, na.rm = TRUE),
#             mean_appwsas = mean(zapp_wsas, na.rm = TRUE))
# 
# #View(zscore_means)
# 
# 
# zscore_names <- setNames(zscores, c("zphq9",               # add variable names
#                             "zgad7",
#                             "zapp_wsas",
#                             "zaudit_sum",
#                             "zlife_stress_total",
#                             "zoslo_sum",
#                             "zpss_total",
#                             "zqids_total",
#                             "zsapas_total",
#                             "zsc_social_participation_total",
#                             "zsd_total",
#                             "zsum_rss",
#                             "ztotal_bipq",
#                             "zwsas_total",
#                             "zctq_total", "Ended by"))
# 
# zscore_names <- zscore_names %>%                                   # convert Ended by to factor (categorical) with 3 levels
#   mutate(`Ended by` = factor(as.character(`Ended by`),
#                              levels = c("1", "2", "3"),
#                              labels = c("lost to follow up", "withdrawal", "completed")))
# #write.csv(test,"~/R/test.csv", row.names = FALSE) 
# 
# 
# # by_studyend_std <- endedby_raw %>%
# #   mutate(sapas_std = mean_sapas/8,                         #standardise
# #         bipq_std = mean_bipq/80,        lifest_std = mean_stress/400,
# #         ctq_std = mean_ctq/100,
# #         phq9_std = mean_phq9/27,
# #         gad7_std = mean_gad7/21,
# #         rss_std = mean_rss/66,
# #         audit_std = mean_audit/40,
# #         oslo_std = (mean_oslo-3)/11,
# #         pss_std = mean_pss/40,
# #         sd_std = mean_sd/45,
# #         sc_std = mean_sc/45,
# #         qids_std = mean_qids/27,
# #         wsas_std = mean_wsas/40,
# #         appwsas_std = mean_appwsas/40)
# 
# #by_studyend_std <- by_studyend_std[, c(1, 17:31)]
# #write.csv(baseline_clin,"~/R/baseline clin.csv", row.names = FALSE) 
# 
# # # Select numeric columns
# # data.numcols <- baseline_clin[, sapply(baseline_clin, is.numeric)]
# # data.numcols$`Ended by` <- baseline_clin$`Ended by`
# # 
# # data.numcols %>%
# #   group_by(`Ended by`) %>%
# #   summarise_at(vars(c()), list(name = mean))
# # 
# # # Using apply
# # all.means <- apply(data.numcols, 2, function(x) mean(x, na.rm = TRUE))
# # # Using colMeans
# # all.means <- colMeans(data.numcols, na.rm = TRUE)
# # 
# # #View(all.means)
# 
# 

# Statistical Analysis on Ended by ------------------------------------------------------

aggregate(baseline_clin$gad7, list(baseline_clin$`Ended by`), FUN=mean, na.rm = TRUE)
aggregate(baseline_clin$wsas_total, list(baseline_clin$`Ended by`), FUN=mean, na.rm = TRUE)
aggregate(baseline_clin$phq9, list(baseline_clin$`Ended by`), FUN=mean, na.rm = TRUE)

#histogram
hist(baseline_clin$gad7)
hist(baseline_clin$phq9)
hist(baseline_clin$wsas_total)


#anova
forTukey <- baseline_clin           ## I need to change the name of Ended by as it doesnt work on Tukey
names(forTukey)[names(forTukey) == "Ended by"] <- 'ended.by'  

one.way <- aov(gad7 ~ ended.by, data = forTukey)
summary(one.way)

#tukey HSD
TukeyHSD(one.way, conf.level=.95)


#Kruskal-Wallis
gad7.kruskal <- baseline_clin %>% kruskal_test(gad7 ~ `Ended by`)
gad7.kruskal

#effect size
baseline_clin %>% kruskal_effsize(gad7 ~ `Ended by`)


pwc <- baseline_clin %>% 
  wilcox_test(gad7 ~ `Ended by`, p.adjust.method = "bonferroni")
pwc
pwc <- pairwise.wilcox.test(baseline_clin$gad7, baseline_clin$`Ended by`, "bonferroni")
pwc

