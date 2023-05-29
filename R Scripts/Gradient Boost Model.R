
# Load packages -----------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(rio)

# Load Data ---------------------------------------------------------------

pregnancy_data <- rio::import(file = "Data/Cleaned Data 35 Plus.csv")


#Recode variuables and remove unnecessary ones
pregnancy_data <- pregnancy_data %>% 
  mutate(Breech=as.factor(Breech)) %>% 
  mutate(`Delivery Type`=as.factor(`Delivery Type`)) %>% 
  select(-ID,`HEP B Vaccination`)

# Split the dataset for training and testing -------------------------------------------------------

set.seed(42)
pregnancy_split <- pregnancy_data%>% 
  initial_split(prop = 0.80, strata=`Delivery Type`)

train_data <- training(pregnancy_split)

test_data <- testing(pregnancy_split)

set.seed(42)
train_folds <- vfold_cv(data = train_data, v = 10) #10 fold cross validation

# Data pre-processing -----------------------------------------------------

df_rec <- recipe(`Delivery Type` ~ ., data = train_data) %>% 
  step_normalize(`Maternal Age`,`Number of Pregnancies`,`Number of children`,
                 `Baby length (cm)`,`Abdominal girth(cm)`,`Birth weight (kg)`,`Head circumference (cm)`) %>% 
  step_dummy(`Gender`, Breech, `Gestational Age`,Insurance, Ethnicity, one_hot = TRUE)


# Specify model type and computational engine -----------------------------

gbt_model <- 
  boost_tree() %>%
  set_engine("xgboost") %>%
  set_mode("classification") %>%
  set_args(mtry = tune(), trees = tune(), learn_rate = tune(), tree_depth = tune(), min_n = tune())

# Create a regular tune grid ----------------------------------------------

gbt_grid <- grid_regular(range_set(trees(), c(300,1000)),
                         range_set(mtry(), c(3,10)),#Only 13 variables in the dataset
                         range_set(tree_depth(), c(3,12)),#default is 6
                         range_set(learn_rate(trans = NULL), c(0.005, 0.02)),
                         range_set(min_n(), c(2,15)),#Default is 6
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
show_gbt <- show_best(x = gbt_res, metric = "accuracy")
show_gbt <- as.data.frame(show_gbt)

rio::export(show_gbt,"Visualisations and Tables/XGBoost Best models.csv")

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

rio::export(predictions,"Visualisations and Tables/XGBoost Predictions.csv")

# Calculate performance metrics -------------------------------------------

matrics <- metrics(predictions, truth = `Delivery Type`, estimate = .pred_class)
rio::export(metrics,"Visualisations and Tables/XGBoost Metrics.rds")

specificity <- spec(predictions, truth = `Delivery Type`, estimate = .pred_class)
sensitivity <- sens(predictions, truth = `Delivery Type`, estimate = .pred_class)

# Confusion matrix --------------------------------------------------------

confusion_matrix <- conf_mat(predictions, truth = `Delivery Type`, estimate = .pred_class)
rio::export(confusion_matrix,"Visualisations and Tables/XGBoost Confusion Matrix.rds")

# Receiver Operating Characteristic (ROC) curve ---------------------------

two_class_curve <- roc_curve(predictions, `Delivery Type`, .pred_CSECTION, event_level = "second")

autoplot(two_class_curve)

