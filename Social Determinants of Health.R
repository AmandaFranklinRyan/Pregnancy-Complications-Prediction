library(tidyverse)
library(rio)

social_data <- read.csv("Data/SOCIAL_DETERMINANTS_HEALTH.csv")
mother_list <- rio::import("Data/Dropped elective csections list.rds")

#Join datasets
mother_list <- mother_list %>% 
  select(SUBJECT_ID)

social_cleaned <- left_join(mother_list, social_data, by="SUBJECT_ID")

#Exploratory data analysis
table(social_cleaned$LANGUAGE)
#Most categories missing, 17 have English, few others Cantonese and Mandarin

table(social_cleaned$ETHNICITY)
# Mostly white

table(social_cleaned$INSURANCE)
# 787 private, very few government, are these women mostly foreigners?

#Select and clean important variables

UNKNOWN_OTHER_ETHNICITY <- c("UNABLE TO OBTAIN", "PATIENT DECLINED TO ANSWER","UNKNOWN/NOT SPECIFIED",
                             "OTHER", "MULTI RACE ETHNICITY","MIDDLE EASTERN","AMERICAN INDIAN/ALASKA NATIVE")
WHITE <- c("WHITE - BRAZILIAN", "WHITE - OTHER EUROPEAN","WHITE")
HISPANIC <- c("HISPANIC/LATINO - PUERTO RICAN", "SOUTH AMERICAN",
              "HISPANIC OR LATINO HISPANIC/LATINO - CENTRAL AMERICAN (OTHER)",
              "HISPANIC","HISPANIC/LATINO - CENTRAL AMERICAN (OTHER)","HISPANIC OR LATINO")
BLACK <- c("BLACK/CAPE VERDEAN", "BLACK/HAITIAN","BLACK/AFRICAN AMERICAN","BLACK/AFRICAN")
ASIAN <- c("ASIAN", "ASIAN - VIETNAMESE","MIDDLE EASTERN","ASIAN - CHINESE")

social_cleaned <- social_cleaned %>% 
  select(ETHNICITY, INSURANCE, RELIGION) %>% 
  mutate(Ethnicity_CLEAN=case_when(ETHNICITY %in% UNKNOWN_OTHER_ETHNICITY~ "UNKNOWN/OTHER",
                              ETHNICITY %in% WHITE~ "WHITE",
                              ETHNICITY %in% HISPANIC~ "HISPANIC/LATINO",
                              ETHNICITY %in% BLACK~ "BLACK",
                              ETHNICITY %in% ASIAN~ "ASIAN",
                              TRUE~ETHNICITY
                              ))
  
table(social_cleaned$Ethnicity_CLEAN)

                  