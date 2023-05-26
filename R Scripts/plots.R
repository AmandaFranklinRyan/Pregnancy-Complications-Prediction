# Load libraries ----------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(ggplot2)
library(corrplot)

# Load data ---------------------------------------------------------------

df0 <- read.csv(file= "data/FinalML-Testdata1.csv")
df <- read.csv(file= "data/forML.csv")

# Check data --------------------------------------------------------------
summary(df)
colSums(is.na(df))

# Preprocessing -----------------------------------------------------------
df <- df %>% 
  mutate(Delivery.Type = as.factor(Delivery.Type))
#View(df)

# Plots -------------------------------------------------------------------

# Explore how the target variable is distributed 
df %>%
  ggplot(aes(x = Delivery.Type, fill = Delivery.Type)) +
  geom_bar(position="dodge") +
  labs(title = "Distribution of Delivery Type")

#Baby Gender
df %>%
  ggplot(aes(x = Delivery.Type, fill = Gender)) +
  geom_bar(position="dodge") +
  labs(title = "Distribution of Delivery Type by Baby Gender")

#Maternal Age
df %>%
  ggplot(aes(x = Delivery.Type, y = Maternal.Age)) +
  geom_violin(fill = "steelblue", color = "white") +
  labs(title = "Maternal Age Distribution by Delivery Type",
       x = "Delivery Type",
       y = "Age")

#Number.of.Pregnancies
df$Number.of.Pregnancies <- factor(df$Number.of.Pregnancies)
df %>%
  ggplot(aes(x = Delivery.Type)) +
  geom_bar(aes(fill = Number.of.Pregnancies), position = "dodge") +
  labs(title = "Distribution of Delivery Type by Number of Pregnancies")

#Number of children
df$Number.of.children <- factor(df$Number.of.children)
df %>%
  ggplot(aes(x = Delivery.Type, fill = Number.of.children)) +
  geom_bar(position="dodge") +
  labs(title = "Distribution of Delivery Type by Number of Children")

#Baby Length cm
#can be grouped
df$Baby.length..cm. <- factor(df$Baby.length..cm.)
df$Baby.length..cm. <- round(df$Baby.length..cm., 2)

df %>%
  ggplot(aes(x = Delivery.Type, fill = Baby.length..cm.)) +
  geom_bar(position="dodge") +
  labs(title = "Distribution of Delivery Type by Baby Length(cm)")

#Abdominal Girth
#can be grouped
df$Abdominal.girth.cm. <- factor(df$Abdominal.girth.cm.)
df %>%
  ggplot(aes(x = Delivery.Type, fill = Abdominal.girth.cm.)) +
  geom_bar(position="dodge") +
  labs(title = "Distribution of Delivery Type by Abdominal Girth")

################ Birth weight #check again
df$Birth.weight..kg. <- factor(df$Birth.weight..kg.)
df$Birth.weight..kg. <- round(df$Birth.weight..kg., 2)
df %>%
  ggplot(aes(x = Delivery.Type, fill = Birth.weight..kg.)) +
  geom_bar(position="dodge") +
  labs(title = "Distribution of Delivery Type by Birth Weight (kg)")

#Head circumference
df$Head.circumference..cm. <- factor(df$Head.circumference..cm.)
df %>%
  ggplot(aes(x = Delivery.Type, fill = Head.circumference..cm.)) +
  geom_bar(position="dodge") +
  labs(title = "Distribution of Delivery Type by Head Circumference")

#Gestational Age
df %>%
  ggplot(aes(x = Delivery.Type, fill = Gestational.Age)) +
  geom_bar(position="dodge") +
  labs(title = "Distribution of Delivery Type by Gestational Age")

#Breech
df$Breech <- factor(df$Breech)
df %>%
  ggplot(aes(x = Delivery.Type, fill = Breech)) +
  geom_bar(position="dodge") +
  labs(title = "Distribution of Delivery Type by Breech")

#LOS
#can be grouped
df$Length.of.ICU.Stay..days. <- factor(df$Length.of.ICU.Stay..days.)
df %>%
  ggplot(aes(x = Delivery.Type, fill = Length.of.ICU.Stay..days.)) +
  geom_bar(position="dodge") +
  labs(title = "Distribution of Delivery Type by Length of ICU Stay (days)")

#Hepatit-B Vaccination
df$HEP.B.Vaccination <- factor(df$HEP.B.Vaccination)
df %>%
  ggplot(aes(x = Delivery.Type, fill = HEP.B.Vaccination)) +
  geom_bar(position="dodge") +
  labs(title = "Distribution of Delivery Type by Hepatit B Vaccination")

#Insurance Type
df %>%
  ggplot(aes(x = Delivery.Type, fill = Insurance)) +
  geom_bar(position="dodge") +
  labs(title = "Distribution of Delivery Type by Insurance Type")

#Ethnicity
df %>%
  ggplot(aes(x = Delivery.Type, fill = Ethnicity)) +
  geom_bar(position="dodge") +
  labs(title = "Distribution of Delivery Type by Ethnicity")


# Reshape data for plots --------------------------------------------------
plot_data <- df %>%
  pivot_longer(cols = c(-Delivery.Type, -Gender, -Maternal.Age, -Number.of.Pregnancies, -Gestational.Age,
                        -Number.of.children, -Insurance, -Baby.length..cm., -Ethnicity), names_to = "key", values_to  = "value")

# Feature Distribution plots 
plot_data %>% 
  ggplot(aes(x = value)) +
  geom_density() +
  facet_wrap(~ key, scales = "free")

# Feature Distribution plots by delivery type
plot_data %>% 
  ggplot(aes(x = value, fill = Delivery.Type)) +
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
  ggplot(aes(y = value, fill = Delivery.Type)) +
  geom_boxplot(alpha = 0.6) +
  facet_wrap(~ key, scales = "free") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Correlation plot (target is class)
df %>% 
  mutate(Delivery.Type = ifelse(Delivery.Type == "Normal", 0, 1)) %>% 
  select(-Delivery.Type) %>% 
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
  mutate(Delivery.Type = as.numeric(Delivery.Type)) %>% 
  cor() %>% 
  corrplot.mixed(upper = "circle",
                 lower = "number",
                 tl.pos = 'lt',
                 tl.col = "black",
                 lower.col = "black",
                 number.cex = 1)

#trial
# Correlation Plot --------------------------------------------------------
df %>% 
  mutate(c(Gender, Maternal.Age, Number.of.Pregnancies, Gestational.Age,
           Number.of.children, Insurance, Baby.length..cm, Ethnicity), as.numeric) %>% 
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
  mutate(across(c(Gender, Maternal.Age ,Number.of.Pregnancies,Number.of.children,Baby.length..cm., Abdominal.girth.cm,
                  Birth.weight..kg, circumferenceCHART,  Gestational.Age,
                  Length.of.ICU.Stay..days.,  HEP.B.Vaccination,  Insurance, Ethnicity ), as.factor))
View(df)

