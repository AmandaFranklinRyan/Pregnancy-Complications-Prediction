library(tidyverse)
library(rio)
library(stringi)

#Import medical notes to extract maternal data
discharge_notes <- read.csv("Data/DISCHARGE_SUMMARIES_ORIG.csv")

discharge_notes_cleaned <- discharge_notes %>% 
  select(-DESCRIPTION,-CATEGORY)

# 1. Extract key maternal information from infants' medical notes ------------

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

pregnancy_sum <- sum(is.na(notes_pregnancy$pregnancy))
pregnancy_fraction <- (pregnancy_sum/nrow(notes_pregnancy))
#Check percent of missing pregnancy data
# Find ages for 92% of women

words_to_remove <- c("was","is","of","weight") # remove these words so that weight and value are closer together and can be easily extracted

weight_regex <- " (?i)((birth\\s+weight|weight|birthweight)(\\s+\\w+){0,6})|(\\d+ gram product)"
notes_weight <- notes_pregnancy %>% 
  mutate(clean_text=str_replace_all(TEXT, "[[:punct:]]", "")) %>% #remove punctuation to make regex matching easier
  mutate(weight=str_extract(clean_text,weight_regex)) %>% # extract weight information using regex
  mutate(weight_clean=str_remove_all(weight, paste(words_to_remove, collapse = "|"))) %>% #take out stop words
  mutate(weight_percentile_removed=str_remove_all(weight_clean, "\\b\\d{2}th\\b")) %>% # delete any numbers before "th" to get rid of percentile information
  mutate(weight_shortened=str_extract(weight_percentile_removed, "\\w+(?:\\s+\\w+){0,2}")) #select first 2 words after the match leaving only the weight not other numbers

#Check percent of missing weight data
# Find weights for 93% of babies
weight_sum <- sum(is.na(notes_weight$weight))
weight_fraction <- (weight_sum/nrow(notes_pregnancy))

# Find the baby length using regex
length_regex <- "(?i)length(\\s+\\w+){0,4}" #select the word "length" and the 4 nearest words after it
notes_length <- notes_weight %>% 
  mutate(clean_text=str_replace_all(TEXT, "[[:punct:]]", "")) %>%
  mutate(length=str_extract(clean_text,length_regex))

notes_length <- notes_length %>% 
  mutate(length_clean=str_remove_all(length, paste(words_to_remove, collapse = "|"))) %>% #take out stop words
  mutate(length_shortened=str_extract(length_clean, "\\w+(?:\\s+\\w+){0,2}")) %>%  #take the first 3 words to get key information 
  select(-length,-length_clean) #remove unnceessary columns

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
  mutate(gender_clean=str_remove_all(gender, paste(words_to_remove_2, collapse = "|"))) #remove the word "sex" leaving single letter

gender_sum <- sum(is.na(notes_gender$gender))
gender_fraction <- (gender_sum/nrow(notes_gender))

maternal_data <- notes_gender %>% 
  select(-clean_text,-gender)

rio::export(maternal_data, 'Data/maternal_data_original.rds')

# 2. Clean Dataframe ---------------------------------------------------------

maternal_data_original <- rio::import('Data/maternal_data_original.rds')

maternal_data_cleaned_age <- maternal_data_original %>% 
  mutate(age_cleaned=str_extract(age, "\\d+")) %>%  #remove the ages into a separate column
  select(-age)

table(maternal_data_cleaned_age$age) #check range of ages, looks reasonable

maternal_data_cleaned_circumference <- maternal_data_cleaned_age %>% 
  mutate(circumference_cleaned=str_extract(circumference_shortened, "\\d+")) %>% 
  mutate(circumference_cleaned=as.numeric(circumference_cleaned)) %>% 
  mutate(circumference_decimal=case_when(circumference_cleaned > 1000 ~ (circumference_cleaned/100),
                                         (circumference_cleaned >100 & circumference_cleaned< 1000) ~ (circumference_cleaned/10),
                                         TRUE ~ circumference_cleaned)) %>%  #correct the decimal points
  select(-circumference_shortened, -circumference_cleaned)

table(maternal_data_cleaned_circumference$circumference_decimal) #check range of circumferences looks reasonable

# Convert gravida to numeric instead of roman numerals
pregnancy_parity_cleaned <- maternal_data_cleaned_circumference %>% 
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
  mutate(clean_primip =if_else(clean_roman_numerals== "primiparous" | clean_roman_numerals== "primip" | clean_roman_numerals=="prima gravida", "G0 P1", clean_roman_numerals))

#Extract the first number into the Gravida column
gravida_data <- parity_cleaned %>% 
  mutate(Gravida=str_extract(clean_primip, "\\d+"))

#Extract the last number into the Para column
# Easier to do this by reversing the string first using the stringi library
para_data <- gravida_data %>% 
  mutate(Para=str_extract(stringi::stri_reverse(clean_primip), "\\d+"))

# Delete unnecessary columns
para_data <- para_data %>% 
  select(-clean_roman_numerals, -clean_roman_numerals2, -clean_primip)

table(para_data$Para) #Check distribution of values makes sense


# 2. Clean and extract the baby weight data ------------------------------------
weight_data <- para_data %>% 
  mutate(weight_grams=str_extract(weight_percentile_removed, "\\d+")) %>%  # extract numeric weights by extracting first mtach
  mutate(weight_grams=as.numeric(weight_grams)) %>% 
  mutate(weight_grams_cleaned=if_else((weight_grams>50 & weight_grams<5000), weight_grams, NA)) %>%  #remove impossible weights 
  mutate(weight_grams_kg=if_else((weight_grams_cleaned>100 & weight_grams_cleaned<500), weight_grams_cleaned*10,weight_grams_cleaned)) #Decimal points lost in processing, these weights are in kg rather than grams    

#Try and enrich weight data with weights in pounds

#Identify weight in pounds
weight_pounds_regex <- "(\\d+(?:\\.\\d+)?)\\s*pounds\\s*(\\d+)"
weight_data <- weight_data %>% 
  mutate(clean_text=str_replace_all(TEXT, "[[:punct:]]", "")) %>% #remove punctuation to make regex matching easier
  mutate(pounds_weight=str_extract(clean_text,weight_pounds_regex)) #extract weight in pounds from medical records

# Use csutom function to convert weights in pounds to weights in kg

## Custom function to convert pounds to grams
convert_kg <- function(weight_string){
  pounds <- as.numeric(str_extract(weight_string, "\\d+"))
  ounces <- as.numeric(str_extract(stringi::stri_reverse(weight_string),"\\d+"))
  
  conversion_factor <- 28.3495
  grams= pounds*conversion_factor*16+ounces*conversion_factor
  
  return(grams)
}
#Covert pounds to kg
weight_data <- weight_data %>% 
  mutate(weight_pounds=convert_kg(pounds_weight))

# If weight field is blank then use pounds instead instead
weight_enhanced <- weight_data %>% 
  mutate(weight_enhanced=if_else(is.na(weight_grams),weight_pounds,weight_grams))

#Check extracted numbers with the original text extracts
check_weights <- weight_enhanced %>% 
  select(weight_grams_cleaned, weight_clean, weight_enhanced, weight_percentile_removed)

#remove unncecessayr columns
weights_cleaned <- weight_enhanced %>% 
  select(-weight, -weight_clean, -weight_percentile_removed, -weight_shortened, -weight_grams, 
         -weight_grams_cleaned, weight_grams_kg, -pounds_weight, -weight_pounds)

# Clean and Extract the baby length data ----------------------------------

#Custom function to convert inches to cm
convert_cm <- function(string_inches){
  
  conversion_factor=2.54
  number <- str_extract(string_inches, "\\d+")
  number_cm <- as.numeric(number)*conversion_factor
  
  return(number_cm)
}

length_cleaned <- weights_cleaned %>% 
  mutate(length=case_when(str_detect(length_shortened, "cm|centimeters") ~ str_extract(length_shortened, "\\d+"), #extrcat values in cm
                          str_detect(length_shortened, "inches") ~ as.character(convert_cm(length_shortened)),#extract values in inches
                          TRUE ~length_shortened)) %>% 
  mutate(length=as.numeric(length)) %>% 
  mutate(length_decimal=case_when((length>100 & length<1000) ~length/10, #correct decimal points
                        length > 1000 ~length/100,
                        TRUE ~length))

# 4. Select important Columns ---------------------------------------------

maternal_data_final <- length_cleaned %>% 
  select(SUBJECT_ID,gender_clean, age_cleaned,circumference_decimal, Gravida, Para, weight_enhanced,length_decimal, TEXT)

summary(maternal_data_final)
export(maternal_data_final, "Data/Maternal_data_cleaned.csv")
  





