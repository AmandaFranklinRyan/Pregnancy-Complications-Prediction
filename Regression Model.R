
# Load packages -----------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(rio)

# Load Data ---------------------------------------------------------------

#Rename columns

pregnancy_data <- rio::import(file = "Data/Final Machine Learning Test Data Version 1.csv")

colnames(pregnancy_data) <- c('ID','Gender','Maternal Age','Number of Pregnancies','Number of children','Delivery Type',
                              'Baby length (cm)','Abdominal girth(cm)','Birth weight (kg)','Head circumference (cm)',
                              'Gestational Age', 'Breech','Length of ICU Stay (days)', 'HEP B Vaccination','Insurance',
                              'Ethnicity')

mutated <- pregnancy_data %>% 
  mutate(`Delivery Type`=as.factor(`Delivery Type`))

logregMULTI <- glm(`Delivery Type` ~
                     as.numeric(`Maternal Age`) +
                     as.numeric(`Number of Pregnancies`) +
                     as.numeric(`Number of children`) +
                     as.numeric(`Head circumference (cm)`) +
                     as.numeric(`Abdominal girth(cm)`) +
                     as.factor(Ethnicity) 
                   
                   , data=mutated, family=binomial('logit')
)

levels(mutated$`Delivery Type`)

# Either copy the results from the RStudio console...
summary(logregMULTI)
print(as.data.frame(exp(cbind(OR = coef(logregMULTI), confint(logregMULTI, level=0.95)))))