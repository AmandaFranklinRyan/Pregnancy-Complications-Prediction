
# Load packages -----------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(rio)

# Load Data ---------------------------------------------------------------

#Rename columns

pregnancy_data <- rio::import(file = "Data/Cleaned Data 35 Plus.csv")

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

# Print results
summary(logregMULTI)
print(as.data.frame(exp(cbind(OR = coef(logregMULTI), confint(logregMULTI, level=0.95)))))