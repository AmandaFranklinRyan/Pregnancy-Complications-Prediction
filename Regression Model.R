
# Load packages -----------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(rio)

# Load Data ---------------------------------------------------------------

colnames(pregnancy_data)
pregnancy_data <- rio::import(file = "Data/ML Data Normal Term.csv")

logregMULTI <- glm(as.factor(`Delivery Type`) ~
                     as.factor(Gender) +
                     as.numeric(`Maternal Data`) +
                     as.numeric(`Number of Pregnancies`) +
                     as.numeric(`Number of children`) +
                     as.numeric(`Head circumference (cm)`) +
                     as.numeric(`Abdominal girth(cm)`) +
                     as.factor(Ethnicity) +
                     as.factor(Breech)
                   
                   , data=pregnancy_data, family=binomial('logit')
)

# Either copy the results from the RStudio console...
summary(logregMULTI)
print(as.data.frame(exp(cbind(OR = coef(logregMULTI), confint(logregMULTI, level=0.95)))))