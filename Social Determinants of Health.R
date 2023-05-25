library(tidyverse)
library(rio)

social_data <- read.csv("Data/SOCIAL_DETERMINANTS_HEALTH.csv")
mother_list <- rio::import("Data/Dropped elective csections list.rds")

#Join datasets
mother_list <- mother_list %>% 
  select(SUBJECT_ID)

social_cleaned <- left_join(mother_list, social_data, by="SUBJECT_ID")

#Explore data set
table(social_cleaned$LANGUAGE)
#Most categories missing, 17 have English, few others Cantonese and Mandarin

table(social_cleaned$ETHNICITY)
# Mostly white

table(social_cleaned$INSURANCE)
# 787 private, very few government, are these women mostly foreigners?
                  