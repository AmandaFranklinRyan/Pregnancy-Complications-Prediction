library(tidyverse)

#Load datasets
maternal_data <- rio::import("Data/Maternal Data 2.csv")
patient_list <- rio::import("Data/FULL_DELIVERY_LIST_CLEANED.csv")
gestational_age <- rio::import("Data/GESTATIONAL_AGE.csv")
head_circumference <- rio::import("Data/HEAD_CIRCUMFERENCE.csv")
length <- rio::import("Data/LENGTH.csv")
abdominal_girth <- rio::import("Data/ABDOMINAL_GIRTH.csv")
weight <- rio::import("Data/BABY_WEIGHT.csv")
diagnoses <- rio::import("Data/BABY_DIAGNOSES.csv")


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

###--- 1.3 Prepare GESTATIONAL_AGE

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
gestation_cleaned <- gestation_cleaned %>% 
  select(SUBJECT_ID, gestation_CHART)

table(gestation_cleaned$gestation_CHART)

###--- 1.4 Prepare ABDOMINAL GIRTH

#Select entries with smallest DIFFERENCE
abdominal_cleaned <- abdominal_girth %>% 
  group_by(SUBJECT_ID) %>% 
  slice_min(order_by=DIFFERENCE, with_ties = FALSE) %>% 
  ungroup() %>% 
  select(SUBJECT_ID,abdominal=VALUE_3294)

###--- 1.5 Prepare HEAD CIRCUMFERENCE

#Select entries with smallest DIFFERENCE
circumference_cleaned <- head_circumference%>% 
  group_by(SUBJECT_ID) %>% 
  slice_min(order_by=DIFFERENCE, with_ties = FALSE) %>% 
  ungroup() %>% 
  select(SUBJECT_ID, circumferenceCHART=BABY_LENGTH)

###--- 1.6 Prepare LENGTH
#Select entries with smallest DIFFERENCE
length_cleaned <- length%>% 
  group_by(SUBJECT_ID) %>% 
  slice_min(order_by=DIFFERENCE, with_ties = FALSE) %>% 
  ungroup() %>% 
  select(SUBJECT_ID, lengthCHART=BABY_LENGTH)


# 2. Join datasets together -------------------------------------------- 

combined_baby_data <- maternal_data_cleaned %>% 
  left_join(patient_list, by="SUBJECT_ID") %>% 
  left_join(gestation_cleaned, by="SUBJECT_ID") %>% 
  left_join(length_cleaned, by="SUBJECT_ID") %>% 
  left_join(abdominal_cleaned, by="SUBJECT_ID") %>% 
  left_join(weight_cleaned, by="SUBJECT_ID") %>% 
  left_join(circumference_cleaned, by="SUBJECT_ID")

# 2. Deal with missing values  -------------------------------------------- 

# As there are only 529 values for gestational age, supplement these values with data from DIAGNOSES table
table(combined_baby_data$gestation_CHART)

patients_only <- combined_baby_data %>% #get list of IDS for patients
  select(SUBJECT_ID)

baby_diagnoses <- left_join(patients_only, diagnoses, by="SUBJECT_ID")

#Gestational age data is missing for many babies, try to get from diagnoses table

#Create new data frame containing only IDS and gestational data from DIAGNOSES table
baby_gestational <- baby_diagnoses %>% 
  select(SUBJECT_ID, SHORT_TITLE) %>% 
  mutate(gestational_age=ifelse(str_detect(SHORT_TITLE,"comp wks gestation"), SHORT_TITLE,NA)) %>% #Identify rows with gestation information
  mutate(gestational_DIAGNOSES=str_extract(gestational_age,"-?\\d+(?:\\.\\d+)?\\s*-\\s*-?\\d+(?:\\.\\d+)?")) %>%  #extract numbers only
  select(-gestational_age) %>% 
  drop_na(gestational_DIAGNOSES)

# Join gestational age and main data_frame
gestational_combined <-left_join(combined_baby_data, baby_gestational, by="SUBJECT_ID") 

#Combine gestational age from CHART and DIAGNOSIS together into a single column
gestational_combined <- gestational_combined %>% 
  mutate(gestational_final=ifelse((!is.na(gestational_DIAGNOSES)), gestational_DIAGNOSES, gestation_CHART)) %>% 
  select(SUBJECT_ID,gender_clean, age_cleaned,Gravida, Para, DELIVERY_TYPE, lengthCHART, abdominal, weight_CHART, circumferenceCHART, gestational_final) %>% 
  drop_na()

table(gestational_combined$gestational_final)
table(gestational_combined$DELIVERY_TYPE)

rio::export(gestational_combined,"Data/ML Data Version 3.csv")



  




