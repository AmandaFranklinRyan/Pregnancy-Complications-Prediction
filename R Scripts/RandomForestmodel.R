# Load packages -----------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(rio)
library(randomForest)
library(vip)

# Load Data ---------------------------------------------------------------

pregnancy_data <- rio::import(file = "Data/Cleaned Data 35 Plus.csv")

# Recode hepatitis vaccination to make it a factor for later steps
pregnancy_data <- pregnancy_data %>% 
  mutate(Breech = as.factor(Breech)) %>% 
  mutate(`Delivery Type` = as.factor(`Delivery Type`)) %>% 
  mutate(`Insurance` = as.factor(`Insurance`)) %>% 
  select(-'HEP B Vaccination',-'Length of ICU Stay (days)',-ID,-Ethnicity,-Insurance)

# Split the dataset -------------------------------------------------------

set.seed(42)
pregnancy_split <- pregnancy_data %>% 
  initial_split(prop = 0.80, strata = `Delivery Type`)

train_data <- training(pregnancy_split)
test_data <- testing(pregnancy_split)

set.seed(42)
train_folds <- vfold_cv(data = train_data, v = 10)

# Data pre-processing -----------------------------------------------------

df_rec <- recipe(`Delivery Type` ~ ., data = train_data) %>% 
  step_normalize(`Maternal Age`,`Number of Pregnancies`,`Number of children`,
                 `Baby length (cm)`,`Abdominal girth(cm)`,`Birth weight (kg)`,`Head circumference (cm)`) %>% 
  step_dummy(`Gender`, Breech, `Gestational Age`, one_hot = TRUE)

# Specify model type and computational engine -----------------------------

rf_model <-
  rand_forest() %>% # Model type: Random Forest
  set_engine("randomForest") %>% # Computational engine: randomForest
  set_mode("classification") %>% # Specify model mode
  set_args(mtry = tune(), trees = tune(),min_n=tune()) # Specify model arguments


gbt_grid <- grid_regular(range_set(mtry(), c(3,10)),
                         range_set(trees(), c(1000,2000)),
                         range_set(min_n(), c(2,15)))

# Create model workflow ---------------------------------------------------

rf_wflow <- 
  workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(df_rec)

# Analyze resamples ------------------------------------------------------

set.seed(42)
rf_res <- 
  rf_wflow %>% 
  tune_grid(resamples = train_folds, grid = 10)

# Collect metrics  --------------------------------------------------------

collect_metrics(rf_res)

# Finalize model workflow ------------------------------------------------

best_rf <- select_best(rf_res, metric = "accuracy")

final_wflow <- 
  rf_wflow %>% 
  finalize_workflow(best_rf)

# Fit and predict ---------------------------------------------------------

rf_fit <- fit(final_wflow, train_data)

rf_pred_class <- predict(rf_fit, new_data = test_data, type = "class")

rf_pred_prob <- predict(rf_fit, new_data = test_data, type = "prob")

# Data frame from test set with model predictions
predictions <- test_data %>% 
  select(`Delivery Type`) %>% 
  bind_cols(., rf_pred_class, rf_pred_prob)

# Calculate performance metrics -------------------------------------------

metrics(predictions, truth = `Delivery Type`, estimate = .pred_class)

# Confusion matrix --------------------------------------------------------

conf_mat(predictions, truth = `Delivery Type`, estimate = .pred_class)

# Receiver Operating Characteristic (ROC) curve ---------------------------

two_class_curve <- roc_curve(predictions, `Delivery Type`, .pred_CSECTION, event_level = "second")

autoplot(two_class_curve)

# identify most important variables-----------------------------------------
vi_df <- rf_fit %>%
  extract_fit_parsnip() %>% # extract the fit object
  vi(scale = TRUE) # scale the variable importance scores so that the largest is 100

ggplot(vi_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "#0072B2") +
  coord_flip() +
  labs(title = "Variable Importance Plot",
       x = "Variable",
       y = "Importance")

