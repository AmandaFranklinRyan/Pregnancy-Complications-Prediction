library(tidyverse)
library(plotly)

# Load datasets
baby_data <- read.csv("Data/ML Data Version 2.csv")
diagnoses <- read.csv("Data/BABY_DIAGNOSES.csv")
icu_data <- read.csv("Data/ICU_INFO.csv")

# Join ICU data to cleaned list of patients
patients_only <- baby_data %>% 
  select(SUBJECT_ID, DELIVERY_TYPE)
icu_baby <- left_join(patients_only, icu_data, by="SUBJECT_ID")


# Exploratory Data Analysis on ICU Stays ----------------------------------

#Plot showing distribution of c-sections and normal delivery
delivery_plot <- ggplot(data=icu_baby, aes(x=LOS, fill=DELIVERY_TYPE))+
  geom_density(alpha=0.5)

#Plot showing length of stays in the ICU
length_stays_plot <- ggplot(data=icu_baby, aes(x=LOS))+
  geom_histogram()
length_stays_plotly <- ggplotly(length_stays_plot)
#Highest number of babies stay 5 days and most less than month, some stay up to 6 months

number_admissions <- icu_baby %>% 
  group_by(SUBJECT_ID, HADM_ID) %>% 
  summarise(count =n())

# Select relevant columns from ICU data
icu_baby_cleaned <- icu_baby %>% 
  select(SUBJECT_ID, LOS)

# Exploratory Data Analysis on baby diagnoses -----------------------------

baby_diagnoses <- left_join(patients_only, diagnoses, by="SUBJECT_ID")

#Gives most common diagnosis of babies, in this case monitoring for infectious disease
# Observation for suspected infectious condition
# Neonatal jaundice associated with preterm delivery
# Need for prophylactic vaccination and inoculation against viral hepatitis

# Clean data to remove conditions adding no new information i.e. gestation, birth type)
remove_condition <- "comp wks gestation|Preterm NEC|Single lb in-hosp"
diagnoses_clean <- baby_diagnoses %>% 
  filter(!grepl(remove_condition, SHORT_TITLE, ignore.case = TRUE)) %>% 
  mutate(SHORT_TITLE=ifelse(ICD9_CODE==7793,"Feeding problems in newborns", SHORT_TITLE)) %>% 
  mutate(LONG_TITLE=ifelse(ICD9_CODE==7793,"Feeding problems in newborns", LONG_TITLE)) #add missing ICD9 code

common_diagnoses <- diagnoses_clean %>% 
  group_by(SHORT_TITLE, LONG_TITLE, ICD9_CODE) %>% 
  summarise(count =n()) %>% 
  arrange(desc(count)) %>% 
  head(10)

#Number of admissions
number_admissions <- diagnoses_clean %>% 
  select(SUBJECT_ID, HADM_ID) %>% 
  distinct() %>% 
  group_by(SUBJECT_ID) %>% 
  summarise(count =n())

table(number_admissions$count)
#Most babies are admitted only once

#Calculate number of conditions each baby has
number_conditions <- diagnoses_clean %>% 
  select(SUBJECT_ID, SHORT_TITLE) %>% 
  group_by(SUBJECT_ID) %>% 
  summarise(count =n())

#Calculate number of patients with hepatitis vaccine and being monitored for infection
number_infections <- diagnoses_clean %>% 
  select(SUBJECT_ID, SHORT_TITLE,DELIVERY_TYPE) %>% 
  mutate(hep_vaccine=ifelse(SHORT_TITLE=="Need prphyl vc vrl hepat", 1, 0)) %>% 
  mutate(monitor_infection=ifelse(SHORT_TITLE=="NB obsrv suspct infect", 1, 0)) %>%
  group_by(SUBJECT_ID, DELIVERY_TYPE)%>%
  summarize(hep_vaccine_total=sum(hep_vaccine),monitor_infection_total=sum(monitor_infection)) %>% 
  mutate(total=hep_vaccine_total+monitor_infection_total)

table(number_infections$total) #627 patients were given hep B injection and monitored for infectious disease
table(number_infections$hep_vaccine_total)

# Calculate number of c-sections/normal deliveries based on hepatitis vaccination status
los_hepB <- left_join(icu_baby,number_infections, by="SUBJECT_ID")

vaccine_delivery <- los_hepB %>% 
  select(hep_vaccine_total, DELIVERY_TYPE) %>% 
  group_by(hep_vaccine_total, DELIVERY_TYPE) %>% 
  summarise(count =n())

#Extract key columns and export dataframe

final_los_diagnoses <- los_hepB %>% 
  select(-DELIVERY_TYPE.x,-DELIVERY_TYPE.y,-HADM_ID, -ICUSTAY_ID)

rio::export(final_los_diagnoses, "Data/LOS and Diagnosis.csv")



