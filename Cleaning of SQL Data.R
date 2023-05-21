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

# Extract number of pregnancies and parity 

pregnancy_regex <- "(?i)(Gravida|G)\\s*([IVX]+|\\d+)[,]?\\s*(Para|P|p)\\s*([IVX]+|\\d+)(?:,\\s*now\\s*\\d+)?|(primiparous)|(primip)|((?i)G\\d+/P\\d+)|(prima gravida)"
notes_pregnancy <- notes_age %>% 
  mutate(pregnancy=str_extract(TEXT,pregnancy_regex))

pregnancy_sum <- sum(is.na(notes_pregnancy$pregnancy))# total number of NAs in the complicated column
pregnancy_fraction <- (pregnancy_sum/nrow(notes_pregnancy))
#Check percent of missing pregnancy data
# Find ages for 92% of women

words_to_remove <- c("was","is","of","weight")

weight_regex <- " (?i)((birth\\s+weight|weight|birthweight)(\\s+\\w+){0,6})|(\\d+ gram product)"
notes_weight <- notes_pregnancy %>% 
  mutate(clean_text=str_replace_all(TEXT, "[[:punct:]]", "")) %>% #remove punctuation to make regex matching easier
  mutate(weight=str_extract(clean_text,weight_regex)) %>% # extract weight information using regex
  mutate(weight_clean=str_remove_all(weight, paste(words_to_remove, collapse = "|"))) %>% #take out stop words
  mutate(weight_percentile_removed=str_remove_all(weight_clean, "\\b\\d{2}th\\b")) %>% # delete any numbers before th to get rid of percentile information
  mutate(weight_shortened=str_extract(weight_percentile_removed, "\\w+(?:\\s+\\w+){0,2}")) #select first 2 words after the match leaving only the weight not other numbers

#Check percent of missing weight data
# Find weights for 93% of babies
weight_sum <- sum(is.na(notes_weight$weight))
weight_fraction <- (weight_sum/nrow(notes_pregnancy))

# Find the baby length using regex
length_regex <- "(?i)length(\\s+\\w+){0,4}"
notes_length <- notes_weight %>% 
  mutate(clean_text=str_replace_all(TEXT, "[[:punct:]]", "")) %>%
  mutate(length=str_extract(clean_text,length_regex))

# Remove stop words to pinpoint length

notes_length <- notes_length %>% 
  mutate(length_clean=str_remove_all(length, paste(words_to_remove, collapse = "|"))) %>% #take out stop words
  mutate(length_shortened=str_extract(length_clean, "\\w+(?:\\s+\\w+){0,2}")) %>%  #take the first 3 words to get key information 
  select(-length,-length_clean)

#Find the babies' head circumference
circumference_regex <- " (?i)head\\s+circumference(\\s+\\w+){0,4}"
notes_circumference <- notes_length %>% 
  mutate(circumference=str_extract(clean_text,circumference_regex)) %>% 
  mutate(circumference_clean=str_remove_all(circumference, paste(words_to_remove, collapse = "|"))) %>% #take out stop words
  mutate(circumference_shortened=str_extract(circumference_clean, "\\w+(?:\\s+\\w+){0,3}")) %>% 
  select(-circumference,-circumference_clean)
circumference_sum <- sum(is.na(notes_circumference$circumference))# total number of NAs in the complicated column
circumference_fraction <- (circumference_sum/nrow(notes_circumference))

#Find the babies' sex
words_to_remove_2 <- ("Sex:")

gender_regex <- "Sex:\\s+[FM]"
notes_gender <- notes_circumference %>% 
  mutate(gender=str_extract(TEXT,gender_regex)) %>% 
  mutate(gender_clean=str_remove_all(gender, paste(words_to_remove_2, collapse = "|")))

gender_sum <- sum(is.na(notes_gender$gender))# total number of NAs in the complicated column
gender_fraction <- (gender_sum/nrow(notes_gender))

#Clean dataframe and export
maternal_data <- notes_gender %>% 
  select(-clean_text,-gender)



