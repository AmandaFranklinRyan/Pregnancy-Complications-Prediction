library(tidyverse)

#Load datasets
maternal_data <- read.csv("Data/ML Data Version 3.csv")
discharge_notes <- read.csv("Data/DISCHARGE_SUMMARIES_ORIG.csv")
icu_data <- read.csv("Data/LOS and Diagnosis.csv")

#Join datasets
maternal_discharge <- left_join(maternal_data, discharge_notes, by="SUBJECT_ID")

# Identify women with planned c-sections and remove them from dataset
#Regex finds words planned, elective, scheduled or stat followed by up to any other 4 words then c-section or Cesarean -section
# Reason regex identifies the 15 words before and after c-section/Cesarean section
elective_regex <- "(?i)\\b(?:planned|elective|scheduled|stat)\\b(?:\\W+\\w+){0,4}?\\W*(?:c-section|Cesarean\\s*section)\\b"
reason_regex <- "(?is)(?:(?:\\S+\\s+){0,15})cesarean\\s+section(?:\\s+(?:\\S+\\s+){0,15})?"

csection_reasons <- maternal_discharge %>% 
  select(SUBJECT_ID,TEXT, DELIVERY_TYPE) %>% 
  mutate(clean_text=str_replace_all(TEXT, "[[:punct:]]", "")) %>%
  mutate(reason=str_extract(clean_text, reason_regex)) %>% 
  mutate(elective=str_extract(clean_text, elective_regex)) %>% 
  select(SUBJECT_ID,reason, DELIVERY_TYPE, elective) %>% 
  filter(DELIVERY_TYPE=="CSECTION")

#iIdentify subject IDs of women who chose c-section
#86 women chose c_sections
elective_list <- csection_reasons %>% 
  filter(!is.na(elective)) %>% 
  select(SUBJECT_ID,elective)

rio::export(elective_list, "Data/Elective csections list.rds")

#Identify babies which are breech

breech_regex <- "(?is)(?:(?:\\S+\\s+){0,5})breech(?:\\s+(?:\\S+\\s+){0,5})?"
breech_reasons <- maternal_discharge %>% 
  select(SUBJECT_ID,TEXT, DELIVERY_TYPE) %>% 
  mutate(clean_text=str_replace_all(TEXT, "[[:punct:]]", "")) %>%
  mutate(breech=str_extract(clean_text, breech_regex)) %>% # 1 denotes baby is breech
  filter(!is.na(breech)) %>% #select only breech babies
  mutate(breech_binary=1) %>% 
  select(SUBJECT_ID, breech_binary) %>% 
  distinct()

rio::export(breech_reasons, "Data/Breechlist.rds")

# Combine Information with other variables----------------------------------------------

breech_info <- left_join(maternal_data, breech_reasons, by="SUBJECT_ID")

maternal_total_minus_elective <- breech_info %>% 
  filter(!SUBJECT_ID %in% elective_list$SUBJECT_ID) #selects only IDs not in elective list




