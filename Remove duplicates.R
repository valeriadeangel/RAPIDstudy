install.packages("writexl")

library("tidyr")
library("writexl")
library("tidyverse")


Book1 <- read_excel("~/Book1.xlsx")
View(Book1)

data_long <- gather(Book1, Week, Value, Week2:Week40, factor_key=TRUE)
data_long

write_xlsx(data_long,"~/LongBook1.xlsx")
duplicates_df <- read_excel("~/LongBook1.xlsx", 2)

duplicates_df %>% 
  group_by(record_id) %>%
  filter(rowSums(is.na(.)) == min(rowSums(is.na(.))))
View(duplicates_df)


count_na <- function(x) sum(is.na(x))    

duplicates_df %>%
  mutate(means = rowMeans(., na.rm = T),
         count_na = apply(., 1, count_na))

duplicates_df %>% mutate(means = rowMeans(., na.rm = T),
       count_na = rowSums(is.na(.)))

LongBook1$CountNa <- rowSums(apply(is.na(LongBook1), 2, as.numeric))
View(LongBook1)


#first, rank by NA numbers - CountNa
df <- LongBook1[with(LongBook1, order(CountNa, decreasing = FALSE)),]

#then get rid of duplicates (it automatically selects the first ones)
df <- df[!duplicated(df[1:2]),]

#replace nas with 0
dfwithoutNAs <- df %>%
  mutate_all(~replace(., is.na(.), 0))


View(df[df$record_id == 14, ])    #e.g. P14
View(dfwithoutNAs)





