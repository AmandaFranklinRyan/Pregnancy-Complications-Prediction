library(tidyverse)
library(rio)

social_data <- read.csv("Data/SOCIAL_DETERMINANTS_HEALTH.csv")
mother_list <- rio::import("Data/Dropped elective csections list.rds")
maternal_data <- rio::import("Data/Maternal Baby Diagnosis and ICU 1.csv")

#Join datasets
mother_list <- mother_list %>% 
  select(SUBJECT_ID)

important_columns <- c("RELIGION","ETHNICITY", "INSURANCE")

#Duplicate subject IDs in the data so want to select the one with fewest NAs
social_cleaned <- left_join(mother_list, social_data, by="SUBJECT_ID") %>% 
  mutate(number_nas = rowSums(is.na(select(., all_of(important_columns))))) %>%  #create row with number NAs in most important columns
  group_by(SUBJECT_ID) %>% 
  slice_min(n = 1, order_by =number_nas,with_ties = FALSE) %>% #takes rows with fewest NAs, with TIES selects only one row if there qre ties
  ungroup()

#Exploratory data analysis
table(social_cleaned$LANGUAGE)
#Most categories missing, 17 have English, few others Cantonese and Mandarin

table(social_cleaned$ETHNICITY)
# Mostly white

table(social_cleaned$INSURANCE)
# 787 private, very few government, are these women mostly foreigners?

table(social_cleaned$RELIGION)
#Mostly not specified, 275 catholics, 61 Jewish, 67 Protestant Quaker
#With so many missing values does not seem meaningful to include it

#Select and clean important variables

#New categories for ETHNICITY data
UNKNOWN_OTHER_ETHNICITY <- c("UNABLE TO OBTAIN", "PATIENT DECLINED TO ANSWER","UNKNOWN/NOT SPECIFIED",
                             "OTHER", "MULTI RACE ETHNICITY","MIDDLE EASTERN","AMERICAN INDIAN/ALASKA NATIVE")
WHITE <- c("WHITE - BRAZILIAN", "WHITE - OTHER EUROPEAN","WHITE")
HISPANIC <- c("HISPANIC/LATINO - PUERTO RICAN", "SOUTH AMERICAN",
              "HISPANIC OR LATINO HISPANIC/LATINO - CENTRAL AMERICAN (OTHER)",
              "HISPANIC","HISPANIC/LATINO - CENTRAL AMERICAN (OTHER)","HISPANIC OR LATINO")
BLACK <- c("BLACK/CAPE VERDEAN", "BLACK/HAITIAN","BLACK/AFRICAN AMERICAN","BLACK/AFRICAN")
ASIAN <- c("ASIAN", "ASIAN - VIETNAMESE","MIDDLE EASTERN","ASIAN - CHINESE")

#Add new categories to dataset
social_cleaned <- social_cleaned %>% 
  select(SUBJECT_ID, ETHNICITY, INSURANCE) %>% 
  mutate(Ethnicity_CLEAN=case_when(ETHNICITY %in% UNKNOWN_OTHER_ETHNICITY~ "UNKNOWN/OTHER",
                              ETHNICITY %in% WHITE~ "WHITE",
                              ETHNICITY %in% HISPANIC~ "HISPANIC/LATINO",
                              ETHNICITY %in% BLACK~ "BLACK",
                              ETHNICITY %in% ASIAN~ "ASIAN",
                              TRUE~ETHNICITY
                              )) %>% 
  select(-ETHNICITY)
 
table(social_cleaned$Ethnicity_CLEAN)

# Add Social determinants of health to other data -------------------------

data_all_variables <- left_join(maternal_data,social_cleaned, by="SUBJECT_ID")




                  