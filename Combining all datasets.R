library(tidyverse)

#Load datasets
maternal_data <- rio::import("Data/Maternal_data_cleaned.csv")
patient_list <- rio::import("Data/FULL_DELIVERY_LIST_CLEANED.csv")
gestational_age <- rio::import("Data/GESTATIONAL_AGE.csv")
head_circumference <- rio::import("Data/HEAD_CIRCUMFERENCE.csv")
length <- rio::import("Data/LENGTH.csv")
abdominal_girth <- rio::import("Data/ABDOMINAL_GIRTH.csv")
weight <- rio::import("Data/BABY_WEIGHT.csv")


# 1. Prepare datasets for joining --------------------------------------------

###--- 1.1 Prepare Maternal health data
#Some patients have more than one record, drop records with fewest NAs
important_columns <- c("age_cleaned", "Gravida", "Para")
maternal_data_cleaned <- maternal_data %>%
  mutate(number_nas = rowSums(is.na(select(., all_of(important_columns))))) %>%  #create row with number NAs in most important columns
  group_by(SUBJECT_ID) %>% 
  slice_min(n = 1, order_by =number_nas) %>% #takes rows with fewest NAs
  ungroup()
# 150 records dropped so now dataset only contains 1 value per SUBJECT_ID

###--- 1.2 Prepare BABY_WEIGHT

weight_cleaned <- weight %>% 
  group_by(SUBJECT_ID) %>% 
  slice_min(order_by=DIFFERENCE, with_ties = FALSE) %>% 
  ungroup()
  top_n(1, -DIFFERENCE) %>% 
  ungroup()
  slice_min(order_by =DIFFERENCE, ties.method="first") %>% 
  ungroup()
  


  
  




