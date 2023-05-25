library(tidyverse)
library(rio)
library(stringi)

#Import medical notes to extract maternal data
discharge_notes <- read.csv("Data/DISCHARGE_SUMMARIES_ORIG.csv")

discharge_notes_cleaned <- discharge_notes %>% 
  select(-DESCRIPTION,-CATEGORY)

# 1. Extract key maternal information from infants' medical notes ------------

# 1.1 Extract mother's age using regular expressions
#From looking at notes need to incorporate newline characters into the Regex

notes_age <- discharge_notes_cleaned %>% 
  mutate(age=str_extract(TEXT,"\\d+[\\n -]*year[\\n -]*old"))

maternal_data_cleaned_age <- notes_age %>% 
  mutate(age_cleaned=str_extract(age, "\\d+")) %>%  #remove the ages into a separate column
  filter(age_cleaned >= 16 & age_cleaned <=50) %>% #Filter to remove incorrect values
  select(-age)

# 1.1 Extract pregnancy data using regex

pregnancy_regex <- "(?i)(Gravida|G)\\s*([IVX]+|\\d+)[,]?\\s*(Para|P|p)\\s*([IVX]+|\\d+)(?:,\\s*now\\s*\\d+)?|(primiparous)|(primip)|((?i)G\\d+/P\\d+)|(prima gravida)"
notes_pregnancy <- maternal_data_cleaned_age %>% 
  mutate(pregnancy=str_extract(TEXT,pregnancy_regex))

# Convert gravida to numeric instead of roman numerals
pregnancy_parity_cleaned <- notes_pregnancy %>% 
  mutate(clean_roman_numerals=case_when(
    str_detect(pregnancy, "XII") ~ str_replace_all(pregnancy, "XII", "12"),
    str_detect(pregnancy, "X") ~ str_replace_all(pregnancy, "X", "10"),
    str_detect(pregnancy, "VIII") ~ str_replace_all(pregnancy, "VIII", "7"),
    str_detect(pregnancy, "VII") ~ str_replace_all(pregnancy, "VII", "7"),
    str_detect(pregnancy, "VI") ~ str_replace_all(pregnancy, "VI", "6"),
    str_detect(pregnancy, "IV") ~ str_replace_all(pregnancy, "IV", "4"),
    str_detect(pregnancy, "V") ~ str_replace_all(pregnancy, "V", "5"),
    str_detect(pregnancy, "III") ~ str_replace_all(pregnancy, "III", "3"),
    str_detect(pregnancy, "II") ~ str_replace_all(pregnancy, "II", "2"),
    str_detect(pregnancy, "I") ~ str_replace_all(pregnancy, "I", "1"),
    str_detect(pregnancy, "lll") ~ str_replace_all(pregnancy, "lll", "3"), #there seems to be some confusion between I and l in the records
    str_detect(pregnancy, "ll") ~ str_replace_all(pregnancy, "ll", "2"),
    str_detect(pregnancy, "l") ~ str_replace_all(pregnancy, "l", "1"),
    TRUE ~ pregnancy
  ))

# Covert para to numeric instead of roman numerals
parity_cleaned <- pregnancy_parity_cleaned %>% 
  mutate(clean_roman_numerals2=case_when(
    str_detect(clean_roman_numerals, "XII") ~ str_replace_all(clean_roman_numerals, "XII", "12"),
    str_detect(clean_roman_numerals, "X") ~ str_replace_all(clean_roman_numerals, "X", "10"),
    str_detect(clean_roman_numerals, "VIII") ~ str_replace_all(clean_roman_numerals, "VIII", "7"),
    str_detect(clean_roman_numerals, "VII") ~ str_replace_all(clean_roman_numerals, "VII", "7"),
    str_detect(clean_roman_numerals, "VI") ~ str_replace_all(clean_roman_numerals, "VI", "6"),
    str_detect(clean_roman_numerals, "IV") ~ str_replace_all(clean_roman_numerals, "IV", "4"),
    str_detect(clean_roman_numerals, "V") ~ str_replace_all(clean_roman_numerals, "V", "5"),
    str_detect(clean_roman_numerals, "III") ~ str_replace_all(clean_roman_numerals, "III", "3"),
    str_detect(clean_roman_numerals, "II") ~ str_replace_all(clean_roman_numerals, "II", "2"),
    str_detect(clean_roman_numerals, "I") ~ str_replace_all(clean_roman_numerals, "I", "1"),
    str_detect(clean_roman_numerals, "lll") ~ str_replace_all(clean_roman_numerals, "lll", "1"),
    str_detect(clean_roman_numerals, "ll") ~ str_replace_all(clean_roman_numerals, "ll", "1"),
    str_detect(clean_roman_numerals, "l") ~ str_replace_all(clean_roman_numerals, "l", "1"),
    TRUE ~ clean_roman_numerals
  ))

# Convert primiparious/primip/prima gravida to G0 P1 and add to column
parity_cleaned <- parity_cleaned %>% 
  mutate(clean_primip =if_else(clean_roman_numerals2== "primiparous" | clean_roman_numerals2== "primip" | clean_roman_numerals2=="prima gravida", "G0 P1", clean_roman_numerals2))

#Extract the first number into the Gravida column
gravida_data <- parity_cleaned %>% 
  mutate(Gravida=str_extract(clean_primip, "\\d+"))

#Extract the last number into the Para column
# Easier to do this by reversing the string first using the stringi library
para_data <- gravida_data %>% 
  mutate(Para=str_extract(stringi::stri_reverse(clean_primip), "\\d+")) %>% 
  mutate(Para=stri_reverse(Para)) #need to reverse number again, otherwise the number is backwards

#Typos in data in the form of 001, 002 etc. remove these
table(para_data$Para)
para_data <- para_data %>% 
  mutate(Para=ifelse(str_detect(Para,"0(?=[1-9])"), NA, Para))

# Delete unnecessary columns
para_data <- para_data %>% 
  select(-clean_roman_numerals, -clean_roman_numerals2, -clean_primip)

#Check results
table(para_data$Gravida)
table(para_data$Para)

#F 1.3 ind the babies' sex
words_to_remove_2 <- ("Sex:")

gender_regex <- "Sex:\\s+[FM]"
notes_gender <- para_data %>% 
  mutate(clean_text=str_replace_all(TEXT, "[[:punct:]]", "")) %>% #remove punctuation to make regex matching easier
  mutate(gender=str_extract(TEXT,gender_regex)) %>% 
  mutate(gender_clean=str_remove_all(gender, paste(words_to_remove_2, collapse = "|"))) #remove the word "sex" leaving single letter

maternal_data <- notes_gender %>% 
  select(-clean_text,-gender)

rio::export(maternal_data, "Data/Maternal Data 2.csv")








