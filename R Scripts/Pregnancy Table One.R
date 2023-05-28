library(tidyverse)
library(tableone)
#install.packages("kableExtra")
library(kableExtra)
library(ggplot2)
library(rio)

#Import datsets

pregnancy_data <- rio::import(file = "Data/Final Machine Learning Test Data Version 1.csv")

colnames(pregnancy_data) <- c('ID', 'Gender','Maternal Age','Number of Pregnancies','Number of children','Delivery Type',
                            'Baby length (cm)','Abdominal girth(cm)','Birth weight (kg)','Head circumference (cm)',
                            'Gestational Age', 'Breech','Length of ICU Stay (days)', 'HEP B Vaccination','Insurance',
                            'Ethnicity')

# 1. Clean data and correct types -----------------------------------------

thirty_five_plus <- c("35-36","37-40","33")

cleaned_data <- pregnancy_data %>% 
  mutate(LOS=as.numeric(`Length of ICU Stay (days)`)) %>% 
  mutate(`Breech`=ifelse(is.na(`Breech`),0, 1)) %>%  #Change NA to 0 
  mutate(`Birth weight (kg)`=ifelse(`Birth weight (kg)`>1000,`Birth weight (kg)`/1000, `Birth weight (kg)`)) %>%  #some weights still in grams, convert to kg
  filter(`Birth weight (kg)`>0.5 & `Birth weight (kg)`<5) %>%   #filter to remove outliers
  filter(`Gestational Age` %in% thirty_five_plus)


#Export cleaned dataframe
rio::export(cleaned_data,"Data/Cleaned Data 35 Plus.csv")

summary(cleaned_data)
str(cleaned_data)

#Visualise data to identify outliers

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
  mutate(`HEP B Vaccination`=ifelse(`HEP B Vaccination`==1, 'Vaccinated','Unvaccinated')) %>% 
  mutate(`HEP B Vaccination`=as.factor(`HEP B Vaccination`))

#Recode gestational age
VERY_EARLY <- c("25-26","27-28","29-30")
EARLY <- c("31-32","33-34")
FULL_TERM <- c("35-36","37-40","40")

cleaned_table <- cleaned_table %>% 
  mutate(`Gestational Age`=case_when(`Gestational Age` %in% FULL_TERM~ "35-40 Weeks",
                                       `Gestational Age` %in% EARLY~ "31-34 Weeks",
                                       `Gestational Age` %in% VERY_EARLY~ "25-30 Weeks",
                                   TRUE~`Gestational Age`))


#Rename all columns in the dataframe
colnames(cleaned_data) <- c('Gender','Maternal Age','Number of Pregnancies','Number of children','Delivery Type',
                            'Baby length (cm)','Abdominal girth(cm)','Birth weight (kg)','Head circumference (cm)',
                            'Gestational Age', 'Breech','Length of ICU Stay (days)', 'HEP B Vaccination','Insurance',
                            'Ethnicity')

#Create table using tableone
categorical_variables <- c("Gender",'Delivery Type', "Gestational Age", "Insurance","Ethnicity",
                           'HEP B Vaccination')
normal_variables <- c("age_cleaned")
summary_table <- tableone::CreateTableOne(data=cleaned_table)
pregnancy_table =  CreateTableOne(data=cleaned_table,
                               vars= c('Gender', 'Maternal Age','Number of Pregnancies','Number of children','Baby length (cm)',
                                       'Birth weight (kg)','Gestational Age', 'Length of ICU Stay (days)','HEP B Vaccination',"Insurance" ,"Ethnicity"),
                               factorVars=categorical_variables,
                               strata = c('Delivery Type'))




# Create the table using tableone
pregnancy_table <- CreateTableOne(data = cleaned_table, vars = c('Gender', 'Maternal Age', 'Number of Pregnancies', 'Number of children', 'Baby length (cm)', 'Birth weight (kg)', 'Gestational Age', 'Length of ICU Stay (days)', 'HEP B Vaccination', 'Insurance', 'Ethnicity'), factorVars = categorical_variables, strata = c('Delivery Type'))

# Convert the table to a formatted text representation
pregnancy_table_text <- print(pregnancy_table)



