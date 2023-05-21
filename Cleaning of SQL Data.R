library(tidyverse)

#Import medical notes to extract maternal data
discharge_notes <- read.csv("Data/DISCHARGE_SUMMARIES_ORIG.csv")

discharge_notes_cleaned <- discharge_notes %>% 
  select(-DESCRIPTION,-CATEGORY)

# Extract key maternal information from infants' medical notes ------------

# Extract mother's age using regular expressions
#From looking at notes need to incorporate newline characters into the Regex

notes_age <- discharge_notes_cleaned %>% 
  mutate(age=str_extract(TEXT,"\\d+[\\n -]*year[\\n -]*old"))

#Check percent of missing age data
# Find ages for 92% of women, quick check of other records shows age data is apparently missing
age_sum <- sum(is.na(notes_age$age))# total number of NAs in the complicated column
age_fraction <- (age_sum/nrow(notes_age))

# Extract number fo pregnancies and parity 

pregnancy_regex <- "(?i)(Gravida|G)\\s*([IVX]+|\\d+)[,]?\\s*(Para|P|p)\\s*([IVX]+|\\d+)(?:,\\s*now\\s*\\d+)?|(primiparous)|(primip)|((?i)G\\d+/P\\d+)|(prima gravida)"
notes_pregnancy <- notes_age %>% 
  mutate(pregnancy=str_extract(TEXT,pregnancy_regex))

pregnancy_sum <- sum(is.na(notes_pregnancy$pregnancy))# total number of NAs in the complicated column
pregnancy_fraction <- (pregnancy_sum/nrow(notes_pregnancy))
#Check percent of missing pregnancy data
# Find ages for 92% of women

weight_regex <- "(\\d+ gram product)"
notes_weight <- notes_pregnancy %>% 
  #mutate(clean_text=str_replace_all(TEXT, "[[:punct:]]", "")) %>%
  mutate(weight=str_extract(TEXT,weight_regex))
weight_sum <- sum(is.na(notes_weight$weight))# total number of NAs in the complicated column
weight_fraction <- (weight_sum/nrow(notes_pregnancy))

length_regex <- "(?i)length(\\s+\\w+){0,4}"
notes_length <- notes_weight %>% 
  mutate(clean_text=str_replace_all(TEXT, "[[:punct:]]", "")) %>%
  mutate(length=str_extract(clean_text,length_regex))
length_sum <- sum(is.na(notes_length$length))# total number of NAs in the complicated column
length_fraction <- (length_sum/nrow(notes_pregnancy))
"(?i)length\\s*\\n?\\s*(?:\\s+was\\s+\\d+\\s+cm|\\d+(?:\\.\\d+)?\\s+(?:cm|centimeters))"
"(?i)length\\s+\\d+(?:\\.\\d+)?\\s+(?:cm|centimeters)|(?i)length\\s+was\\s+\\d+\\s+cm"
(?i)length\\s*\\n?\\s*(?:\\s+was\\s+\\d+\\s+cm|\\d+(?:\\.\\d+)?\\s+(?:cm|centimeters))

circumference_regex <- " (?i)head\\s+circumference(\\s+\\w+){0,4}"
notes_circumference <- notes_length %>% 
  mutate(cirumference=str_extract(clean_text,circumference_regex))
circumference_sum <- sum(is.na(notes_circumference$circumference))# total number of NAs in the complicated column
circumference_fraction <- (length_sum/nrow(notes_circumference))



(?i)length\\s+\\d+(?:\\.\\d+)?\\s+(?:cm|centimeters)
(?i)length\\s+\\d+(?:\\.\\d+)?\\s+cm

"\\b(\\w+\\s+){0,4}\\d{1,3}(?:,\\d{3})*\\s+gram\\s+(\\w+\\s+){0,4}\\b"
"\\d+ gram product"
"\\b(\\w+\\s+){0,4}\\d{1,3}(?:,\\d{3})*\\s+gram\\s+(\\w+\\s+){0,4}\\b"

notes_complicated <- discharge_notes_cleaned %>% 
  mutate(clean_text=str_replace_all(TEXT, "[[:punct:]]", "")) %>% 
  mutate(complicated=str_extract(clean_text,"([^\\s]+\\s+){2}uncomplicated(\\s+[^\\s]+){2}"))

uncomplicated_sum <- sum(is.na(notes_age$complicated))# total number of NAs in the complicated column
uncomplicated <- (1-(uncomplicated_sum/nrow(notes_age))) #20% notes specifically mention uncomplicated