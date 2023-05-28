
# Load packages -----------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(rio)

# Load Data ---------------------------------------------------------------

#Rename columns

pregnancy_data <- rio::import(file = "Data/Cleaned Data 35 Plus.csv")

# Reshape data for plots --------------------------------------------------
plot_data <- pregnancy_data %>%
  pivot_longer(cols = c( -ID, -Gender,-`Delivery Type`,-`Gestational Age`,-Insurance,-Breech,-`HEP B Vaccination`,
                         -Ethnicity), names_to = "key", values_to  = "value")

# Feature Distribution plots 
plot_data %>% 
  ggplot(aes(x = value, fill = `Delivery Type`)) +
  geom_density(alpha=0.6) +
  facet_wrap(~ key, scales = "free")

colnames(pregnancy_data)
mutated <- pregnancy_data %>% 
  mutate(`Delivery Type`=as.factor(`Delivery Type`))

logregMULTI <- glm(`Delivery Type` ~
                     as.numeric(`Maternal Age`) +
                     as.numeric(`Number of Pregnancies`) +
                     as.numeric(`Baby length (cm)`) +
                     as.numeric(`Birth weight (kg)`) +
                     as.numeric(`Number of children`) +
                     as.numeric(`Head circumference (cm)`) +
                     as.numeric(`Abdominal girth(cm)`) +
                     as.factor(`Gestational Age`)+
                     as.factor(Ethnicity)+
                     as.factor(Gender)+
                     as.factor(Breech)
                   , data=mutated, family=binomial('logit')
)

levels(mutated$`Delivery Type`)

# Print results
summary(logregMULTI)
regression_results <- print(as.data.frame(exp(cbind(OR = coef(logregMULTI), confint(logregMULTI, level=0.95)))))