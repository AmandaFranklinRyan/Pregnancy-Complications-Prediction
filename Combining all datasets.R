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
  slice_min(n = 1, order_by =number_nas,with_ties = FALSE) %>% #takes rows with fewest NAs, with TIES selects only one row if there qre ties
  ungroup()
# 300 records dropped so now dataset only contains 1 value per SUBJECT_ID

###--- 1.2 Prepare BABY_WEIGHT

#Select weight value with the shortest difference between admission and CHARTTIME
weight_cleaned <- weight %>% 
  group_by(SUBJECT_ID) %>% 
  slice_min(order_by=DIFFERENCE, with_ties = FALSE) %>% 
  ungroup()

#Create single weight column
# Choose VALUE_3723 as it has the most values and it it is black use weight from one of the other columns instead
weight_cleaned <- weight_cleaned %>% 
  mutate(across(c(VALUE_3580, VALUE_3723, VALUE_4183), as.numeric)) %>% 
  mutate(weight_CHART= case_when(!is.na(VALUE_3723)~ VALUE_3723,
                                 is.na(VALUE_3723) & !is.na(VALUE_3580) ~ VALUE_3580,
                                 is.na(VALUE_3723) & !is.na(VALUE_4183) ~ VALUE_4183)) 

weight_cleaned <- weight_cleaned %>% 
  select(SUBJECT_ID, weight_CHART)

###--- 1.2 Prepare GESTATIONAL_AGE

#Select entries with smallest DIFFERENCE
gestation_cleaned <- gestational_age %>% 
  group_by(SUBJECT_ID) %>% 
  slice_min(order_by=DIFFERENCE, with_ties = FALSE) %>% 
  ungroup()

#Clean column to get consistent format
words_to_remove <- c("weeks gest","PMA",">","weeks","WK","WKS","(?<=40).*")
gestation_cleaned <- gestation_cleaned %>% 
  mutate(gestation_CHART=str_remove_all(VALUE, paste(words_to_remove, collapse = "|")))

#Select columns
gestation_cleaned <- 

table(gestation_cleaned$gestation_clean)
 


  
  




