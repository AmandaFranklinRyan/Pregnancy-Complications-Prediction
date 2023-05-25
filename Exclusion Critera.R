library(tidyverse)
"\\w+(?:\\s+\\w+){0,2}"
"(?:\\S+\\s+){0,6}\\bsection\\b\\s+\\bperformed\\b\\s+\\bdue\\b\\s+\\bto\\b(?:\\s+\\S+){0,6}"
"(?is)(?:(?:\\S+\\s+){0,15})c\\s+section(?:\\s+(?:\\S+\\s+){0,15})?"
#Load datasets

maternal_data <- read.csv("Data/ML Data Version 3.csv")
discharge_notes <- read.csv("Data/DISCHARGE_SUMMARIES_ORIG.csv")

maternal_discharge <- left_join(maternal_data, discharge_notes, by="SUBJECT_ID")

elective_regex <- "(?i)\\b(?:planned|elective|scheduled|stat)\\b(?:\\W+\\w+){0,4}?\\W*(?:c-section|Cesarean\\s*section)\\b"
reason_regex <- "(?is)(?:(?:\\S+\\s+){0,15})cesarean\\s+section(?:\\s+(?:\\S+\\s+){0,15})?"
csection_reasons <- maternal_discharge %>% 
  select(SUBJECT_ID,TEXT, DELIVERY_TYPE) %>% 
  mutate(clean_text=str_replace_all(TEXT, "[[:punct:]]", "")) %>%
  mutate(reason=str_extract(clean_text, reason_regex)) %>% 
  mutate(elective=str_extract(clean_text, elective_regex)) %>% 
  select(reason, DELIVERY_TYPE, elective) %>% 
  filter(DELIVERY_TYPE=="CSECTION")

"(?i)(planned|elective|scheduled|stat)(\\W+\\w+){0,4}?\\W*(c\\W*s\\W*e\\W*c\\W*t\\W*i\\W*o\\W*n\\W*\\s+\\w+|C\\W*e\\W*s\\W*a\\W*r\\W*e\\W*a\\W*n\\W*\\s+\\w+\\s+section)"
"(?i)\\b(?:planned|elective|scheduled|stat)\\b(?:\\W+\\w+){0,4}?\\W*(?:c-section|Cesarean\\s*section)\\b"

count_na <- sum(is.na(csection_reasons$reason))
elective repeat planned, elective, planned, scheduled, pprevious, repeat stat c_section

section performed due to
(?is)(?:(?:\\S+\\s+){0,6})cesarean\\s+section(?:\\s+(?:\\S+\\s+){0,6})?