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


#Check percent of missing data
# Find ages for 92% of women, quick check of other records shows age data is apparently missing
age_sum <- sum(is.na(notes_age$age))# total number of NAs in the complicated column
age_fraction <- (age_sum/nrow(notes_age))

# Extract number fo pregnancies and parity 
notes_pregnancy <- notes_age %>% 
  mutate(pregnancy=str_extract(TEXT,"(?i)Gravida\\s*([IVX]+|\\d+)[,]?\\s*Para\\s*([IVX]+|\\d+)"))

notes_complicated <- discharge_notes_cleaned %>% 
  mutate(clean_text=str_replace_all(TEXT, "[[:punct:]]", "")) %>% 
  mutate(complicated=str_extract(clean_text,"([^\\s]+\\s+){2}uncomplicated(\\s+[^\\s]+){2}"))

uncomplicated_sum <- sum(is.na(notes_age$complicated))# total number of NAs in the complicated column
uncomplicated <- (1-(uncomplicated_sum/nrow(notes_age))) #20% notes specifically mention uncomplicated