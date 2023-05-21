library(tidyverse)

#Import datasets

discharge_notes <- read.csv("Data/DISCHARGE_SUMMARIES_ORIG.csv")

discharge_notes_cleaned <- discharge_notes %>% 
  select(-DESCRIPTION,-CATEGORY)

example <- "This is not very complicated This is not"
str_extract(example,"([^\\s]+\\s+){2}complicated(\\s+[^\\s]+){2}")

notes_age <- discharge_notes_cleaned %>% 
  mutate(age=str_extract(TEXT,"\\d+[\\n -]*year[\\n -]*old")) %>% 
  filter(is.na(age))

"\\d+[\\n -]*year[\\n -]*old"
"\\d+[ -]year-old"


age_sum <- sum(is.na(notes_age$age))# total number of NAs in the complicated column
age_fraction <- (age_sum/nrow(notes_age))

notes_complicated <- discharge_notes_cleaned %>% 
  mutate(clean_text=str_replace_all(TEXT, "[[:punct:]]", "")) %>% 
  mutate(complicated=str_extract(clean_text,"([^\\s]+\\s+){2}uncomplicated(\\s+[^\\s]+){2}"))

uncomplicated_sum <- sum(is.na(notes_age$complicated))# total number of NAs in the complicated column
uncomplicated <- (1-(uncomplicated_sum/nrow(notes_age))) #20% notes specifically mention uncomplicated