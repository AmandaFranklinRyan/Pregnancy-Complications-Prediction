library(tidyverse)
library(tableone)
#install.packages("kableExtra")
library(kableExtra)
library(ggplot2)
library(rio)
library(data.table)
library(formattable)

#Import datsets

pregnancy_data <- rio::import(file = "Data/Final Machine Learning Test Data Version 1.csv")

colnames(pregnancy_data) <- c('ID', 'Gender','Maternal Age','Number of Pregnancies','Number of children','Delivery Type',
                            'Baby length (cm)','Abdominal girth(cm)','Birth weight (kg)','Head circumference (cm)',
                            'Gestational Age', 'Breech','Length of ICU Stay (days)', 'HEP B Vaccination','Insurance',
                            'Ethnicity')

# 1. Clean data and correct types -----------------------------------------

thirty_five_plus <- c("35-36","37-40","40")

cleaned_data <- pregnancy_data %>% 
  mutate(`Length of ICU Stay (days)`=as.numeric(`Length of ICU Stay (days)`)) %>% 
  mutate(`Breech`=ifelse(is.na(`Breech`),0, 1)) %>%  #Change NA to 0 
  mutate(`Birth weight (kg)`=ifelse(`Birth weight (kg)`>1000,`Birth weight (kg)`/1000, `Birth weight (kg)`)) %>%  #some weights still in grams, convert to kg
  filter(`Birth weight (kg)`>0.5 & `Birth weight (kg)`<5) %>%   #filter to remove outliers
  filter(Gender=='F'|Gender=='M') %>% 
  filter(`Gestational Age` %in% thirty_five_plus)

#Export cleaned dataframe
rio::export(cleaned_data,"Data/Cleaned Data 35 Plus.csv")

 
# Visualise data to identify outliers -------------------------------------

# Plot of head circumference
head_circumference_plot <- ggplot(data=cleaned_data, aes(x=circumferenceCHART, fill=DELIVERY_TYPE))+
  geom_density(alpha=0.5)
# csection deliveries seem to be associated with lower head circumferences, is this because they are more
#likely to be premature?

# Plot of abdominal girth
abdominal_plot <- ggplot(data=cleaned_data, aes(x=abdominal, fill=DELIVERY_TYPE))+
  geom_density(alpha=0.5)
# csection deliveries seem to be associated with lower abdominal girth

# Plot of age
age_plot <- ggplot(data=cleaned_data, aes(x=age_cleaned, fill=DELIVERY_TYPE))+
  geom_density(alpha=0.5)

# Plot of weight
weight_plot <- ggplot(data=cleaned_data, aes(x=weight_CHART, fill=DELIVERY_TYPE))+
  geom_density(alpha=0.5) 
#Again c_sections appear to be more common with lighter babies

# Plot of LOS
LOS_plot <- ggplot(data=cleaned_data, aes(x=LOS, fill=DELIVERY_TYPE))+
  geom_density(alpha=0.5) 
#c-section babies stay in the hospital longer



# 3. Create Tableone ---------------------------------------------------------
## ---- 3  Summarising Data using tableone library

# Convert types for table
cleaned_table <- cleaned_data %>% 
  mutate(Gender=as.factor(Gender)) %>% 
  mutate(Breech=ifelse(Breech==1, 'Breech','Not breech')) %>% 
  mutate(Breech=as.factor(Breech))

#Recode gestational age
thirty_five <- c("35-36")
thirty_seven <- c("37-40")
forty_plus <- c("40")

cleaned_table <- cleaned_table %>% 
  mutate(`Gestational Age`=case_when(`Gestational Age` %in% thirty_five~ "35-36 Weeks",
                                       `Gestational Age` %in% thirty_seven~ "37-40 Weeks",
                                       `Gestational Age` %in% forty_plus~ "40+ Weeks",
                                   TRUE~`Gestational Age`))

#Create table using tableone
categorical_variables <- c("Gender",'Delivery Type', "Gestational Age","Ethnicity")
normal_variables <- c("Maternal Age")

pregnancy_table =  CreateTableOne(data=cleaned_table,
                               vars= c('Maternal Age','Number of Pregnancies','Number of children','Breech', 'Baby length (cm)',
                                       'Birth weight (kg)','Gender', 'Abdominal girth(cm)', 'Gestational Age','Length of ICU Stay (days)',
                                       "Ethnicity"),
                               factorVars=categorical_variables,
                               strata = c('Delivery Type'))


# Convert the table to a formatted text representation
pregnancy_table_text <- print(pregnancy_table, showAllLevels = TRUE) #includes both categories for binary categorical variables

#Copy to Excel to delete columns and import back to customise
table_one_formatted <- read.csv("Data/TableOneDataFinal.csv", colClasses = "character", header= TRUE) #reads column names as headers and doesn't include NAs
table_one_data_table <- as.data.frame(table_one_formatted) #convert to dataframe

#Rename columns and convert columns to bold type
colnames(table_one_data_table) <- c("Variable"," ","C-section","Normal Delivery")
formatted_table <- formattable(table_one_data_table,align =c("l","c","c"),
                               list(`Variable` = formatter("span", style = ~ style(color = "black",font.weight = "bold"))))



