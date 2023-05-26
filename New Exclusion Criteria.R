library(tidyverse)
library(rio)

#Import datasets
pregnancy_data <- rio::import(file = "Data/Cleaned Data for Machine Learning 2.csv")
medical_notes <- rio::import(file = "Data/DISCHARGE_SUMMARIES_ORIG.csv")

# Join datasets together
subject_IDs <- pregnancy_data %>% 
  select(ID, `Delivery Type`,`Gestational Age`)

notes_cleaned <- medical_notes %>% 
  select(SUBJECT_ID, TEXT)

cleaned_data <- left_join(subject_IDs, notes_cleaned, by=c("ID"="SUBJECT_ID"))

# Inclusion Criteria 1: Uncomplicated Pregnancy  and gestation >30 weeks ---------------------------

# Identify wpmen with previously uncomplicated pregnancies
# Identify adjectives "unremarkable" or "uncomplicated" close to words_to_detect
## 226 babies in totel

early_delivery <- c("25-26","27-28","29-30")
gestation_filtered <- cleaned_data %>% 
  filter(!(`Gestational Age`%in% early_delivery))

reasons_regex <- "\\b(?:\\S+\\s+){0,6}(?:uncomplicated|unremarkable)(?:\\s+\\S+){0,15}\\b"
words_to_detect <- ("antepartum|prenatal|mother|pregnancy|history")

reasons_csection <- gestation_filtered %>% 
  mutate(clean_text=str_replace_all(TEXT, "[[:punct:]]", "")) %>% #remove punctuation to make regex matching easier
  mutate(lowercase_text = tolower(clean_text))%>% #prevent confusion with capitalisation
  mutate(uncomplicated=str_extract(lowercase_text,reasons_regex)) %>% 
  mutate(uncomplicated_cleaned=ifelse(str_detect(uncomplicated, words_to_detect), uncomplicated, NA)) %>% 
  mutate(uncomplicated_binary=ifelse(!(is.na(uncomplicated_cleaned)),1, 0)) %>%  # make binary variable
  filter(uncomplicated_binary==1) %>% 
  distinct(ID) #select ids

# Filter all variables so only includes IDS in this shorter list

uncomplicated <- left_join(reasons_csection, pregnancy_data, by=c("ID"))
rio::export(uncomplicated, "Data/ML Data Uncomplicated.csv")


# Exclusion Criteria 2 Gestation >37 weeks ----------------------------------------------------

#Select only babies that were born later than 37 weeks
# 178 in total
normal_delivery <- c("37-40","40")
gestation_filtered_2 <- cleaned_data %>% 
  filter(`Gestational Age`%in% normal_delivery) %>%
  select(ID)

normal_term <- left_join(gestation_filtered_2, pregnancy_data, by=c("ID"))
rio::export(uncomplicated, "Data/ML Data Normal Term.csv")
