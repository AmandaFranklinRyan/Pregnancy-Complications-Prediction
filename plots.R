# Load libraries ----------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(ggplot2)
library(corrplot)

# Load data ---------------------------------------------------------------

df <- read.csv(file= "data/finaldata1.csv")

# Check data --------------------------------------------------------------
summary(df)
colSums(is.na(df))

# Preprocessing -----------------------------------------------------------
df <- df %>% 
  select(-SUBJECT_ID, -breech_binary) %>% 
  mutate(DELIVERY_TYPE = as.factor(DELIVERY_TYPE))
#View(df)

# Plots -------------------------------------------------------------------

# Explore how the target variable is distributed 
df %>%
  ggplot(aes(x = DELIVERY_TYPE, fill = DELIVERY_TYPE)) +
  geom_bar(position="dodge") +
  labs(title = "Distribution of Delivery Type")

#Baby Gender
df %>%
  drop_na(gender_clean) %>% 
  ggplot(aes(x = DELIVERY_TYPE, fill = gender_clean)) +
  geom_bar(position="dodge") +
  labs(title = "Distribution of Delivery Type by Baby Gender")

#Gestational Age
df %>%
  ggplot(aes(x = DELIVERY_TYPE, fill = gestational_final)) +
  geom_bar(position="dodge") +
  labs(title = "Distribution of Delivery Type by Gestational Age")

#Mother's Age
df %>%
  ggplot(aes(x = DELIVERY_TYPE, y = age_cleaned)) +
  geom_violin(fill = "steelblue", color = "white") +
  labs(title = "Age Distribution by Delivery Type",
       x = "Delivery Type",
       y = "Age")

#Gravida ####Check again
df %>%
  ggplot(aes(x = DELIVERY_TYPE, fill = Gravida)) +
  geom_bar(position = "stack") +
  labs(title = "Gravida Distribution by Delivery Type",
       x = "Delivery Type",
       y = "Count",
       fill = "Gravida")

#Para
df %>%
  ggplot(aes(x = DELIVERY_TYPE, fill = Para)) +
  geom_bar(position="dodge") +
  labs(title = "Distribution of Delivery Type by Para")

#Insurance Type
df %>%
  ggplot(aes(x = DELIVERY_TYPE, fill = INSURANCE)) +
  geom_bar(position="dodge") +
  labs(title = "Distribution of Delivery Type by Insurance Type")

#Ethnicity
df %>%
  ggplot(aes(x = DELIVERY_TYPE, fill = Ethnicity_CLEAN)) +
  geom_bar(position="dodge") +
  labs(title = "Distribution of Delivery Type by Ethnicity")


# Reshape data for plots --------------------------------------------------
plot_data <- df %>%
  pivot_longer(cols = c(-DELIVERY_TYPE, -gender_clean, -age_cleaned, -Gravida, -gestational_final,
                        -Para, -INSURANCE, -lengthCHART, -Ethnicity_CLEAN), names_to = "key", values_to  = "value")

# Feature Distribution plots 
plot_data %>% 
  ggplot(aes(x = value)) +
  geom_density() +
  facet_wrap(~ key, scales = "free")

# Feature Distribution plots by delivery type
plot_data %>% 
  ggplot(aes(x = value, fill = DELIVERY_TYPE)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ key, scales = "free")

# Feature Box plots
plot_data %>% 
  ggplot(aes(y = value)) +
  geom_boxplot() +
  facet_wrap(~ key, scales = "free") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Feature Box plots by delivery type
plot_data %>% 
  ggplot(aes(y = value, fill = DELIVERY_TYPE)) +
  geom_boxplot(alpha = 0.6) +
  facet_wrap(~ key, scales = "free") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Correlation plot (target is class)
df %>% 
  mutate(DELIVERY_TYPE = ifelse(DELIVERY_TYPE == "Normal", 0, 1)) %>% 
  select(-DELIVERY_TYPE) %>% 
  cor() %>% 
  corrplot.mixed(order = "hclust",
                 upper = "circle",
                 lower = "number",
                 tl.pos = 'lt',
                 tl.col = "black",
                 lower.col = "black",
                 number.cex = 1)

# Correlation plot (target is numeric)
df %>% 
  mutate(DELIVERY_TYPE = as.numeric(DELIVERY_TYPE)) %>% 
  cor() %>% 
  corrplot.mixed(upper = "circle",
                 lower = "number",
                 tl.pos = 'lt',
                 tl.col = "black",
                 lower.col = "black",
                 number.cex = 1)


# Correlation Plot --------------------------------------------------------
df %>% 
  mutate(c(gender_clean, age_cleaned, Gravida, gestational_final,
           Para, INSURANCE, lengthCHART, Ethnicity_CLEAN), as.numeric) %>% 
  cor() %>% 
  corrplot.mixed(order = "hclust",
                 upper = "circle", 
                 lower = "number",
                 tl.pos = "lt",
                 tl.col = "black",
                 lower.col = "black",
                 number.cex = 1)



df <- df %>% 
  select(-SUBJECT_ID) %>% 
  mutate(across(c(gender_clean, age_cleaned ,Gravida,Para,lengthCHART, abdominal,
                  weight_CHART, circumferenceCHART,  gestational_final,
                  LOS,  hep_vaccine_total,  INSURANCE, Ethnicity_CLEAN ), as.factor))
View(df)
