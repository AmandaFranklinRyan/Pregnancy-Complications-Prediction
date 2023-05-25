
# Load packages -----------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(rio)

# Load Data ---------------------------------------------------------------

pregnancy_data <- rio::import(file = "Data/Cleaned Data for Machine Learning.rds")

#Rename columns
colnames(pregnancy_data) <- c('Gender','Maternal Age','Number of Pregnancies','Number of children','Delivery Type',
                            'Baby length (cm)','Abdominal girth(cm)','Birth weight (kg)','Head circumference (cm)',
                            'Gestational Age', 'Breech','Length of ICU Stay (days)', 'HEP B Vaccination','Insurance',
                            'Ethnicity')

#Recode hepatitis vaccination to make it a factor for later setpes
pregnancy_data <- pregnancy_data %>% 
  mutate(`HEP B Vaccination`=as.factor(`HEP B Vaccination`)) %>% 
  mutate(Breech=as.factor(Breech)) %>% 
  mutate(`Delivery Type`=as.factor(`Delivery Type`))

# Split the dataset -------------------------------------------------------

set.seed(42)
pregnancy_split <- pregnancy_data%>% 
  initial_split(prop = 0.80, strata=`Delivery Type`)

train_data <- training(pregnancy_split)

test_data <- testing(pregnancy_split)

set.seed(42)
train_folds <- vfold_cv(data = train_data, v = 10)

# Data pre-processing -----------------------------------------------------

df_rec <- recipe(`Delivery Type` ~ ., data = train_data) %>% 
  step_normalize(`Maternal Age`,`Number of Pregnancies`,`Number of children`,
                 `Baby length (cm)`,`Abdominal girth(cm)`,`Birth weight (kg)`,`Head circumference (cm)`,
                 `Length of ICU Stay (days)`) %>% 
  step_dummy(`Gender`, Breech, `Gestational Age`,`HEP B Vaccination`,Insurance, Ethnicity, one_hot = TRUE)


# Specify model type and computational engine -----------------------------

gbt_model <- 
  boost_tree() %>%
  set_engine("xgboost") %>%
  set_mode("classification") %>%
  set_args(mtry = 4, trees = tune(), learn_rate = tune(), tree_depth = 6, min_n = 1)

# Create a regular tune grid ----------------------------------------------

gbt_grid <- grid_regular(range_set(trees(), c(300,1000)),
                         range_set(learn_rate(trans = NULL), c(0.005, 0.02)),
                         levels = 4)

# Create model workflow ---------------------------------------------------

gbt_wflow <- 
  workflow() %>% 
  add_model(gbt_model) %>% 
  add_recipe(df_rec)

# Analyse resamples with hyperparameter tuning ----------------------------

set.seed(42)
gbt_res <- 
  gbt_wflow %>% 
  tune_grid(resamples = train_folds, grid = gbt_grid)

# Collect metrics  ---------------------------------------------

collect_metrics(gbt_res)

# Finalise model workflow -------------------------------------------------

best_gbt <- select_best(x = gbt_res, metric = "accuracy")

final_wflow <- 
  gbt_wflow %>% 
  finalize_workflow(best_gbt)

# Fit and predict ---------------------------------------------------------

gbt_fit <- fit(final_wflow, train_data)

gbt_pred_class <- predict(gbt_fit, new_data = test_data, type = "class")

gbt_pred_prob <- predict(gbt_fit, new_data = test_data, type = "prob")

# Data frame from test set with “attached” the model predictions
predictions <- test_data %>% 
  select(`Delivery Type`) %>% # keep target variable (also known as the truth)
  bind_cols(., gbt_pred_class, gbt_pred_prob)

# Calculate performance metrics -------------------------------------------

metrics(predictions, truth = `Delivery Type`, estimate = .pred_class)

# Confusion matrix --------------------------------------------------------

conf_mat(predictions, truth = `Delivery Type`, estimate = .pred_class)

# Receiver Operating Characteristic (ROC) curve ---------------------------

two_class_curve <- roc_curve(predictions, `Delivery Type`, .pred_CSECTION, event_level = "second")

autoplot(two_class_curve)

