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
  mutate(DELIVERY_TYPE = ifelse(DELIVERY_TYPE == "NORMAL", 0, 1)) %>% 
  select(-SUBJECT_ID, -breech_binary) %>% 
  mutate(across(c(gender_clean, age_cleaned ,Gravida,Para,lengthCHART, abdominal,
                  weight_CHART, circumferenceCHART,  gestational_final,
                  LOS,  hep_vaccine_total,  INSURANCE, Ethnicity_CLEAN ), as.factor))


# Split the dataset -------------------------------------------------------

set.seed(42) #
df_split <- df %>% 
  initial_split(prop = 0.80, strata = DELIVERY_TYPE)

train_data <- training(df_split)

test_data <- testing(df_split)

# Inspect train and test datasets -----------------------------------------

# Target variable distribution
train_data %>% 
  ggplot(aes(x = DELIVERY_TYPE)) +
  geom_bar() +
  ggtitle("Train dataset")

test_data %>% 
  ggplot(aes(x = DELIVERY_TYPE)) +
  geom_bar() +
  ggtitle("Test dataset")

# Specify model type and computational engine -----------------------------


tree_model <- 
  decision_tree() %>% # Model type: Decision Tree 
  set_engine("rpart") %>% # Computational engine: rpart
  set_mode("classification") %>%  # Specify model mode
  set_args(min_n = 2, tree_depth = 5) # Specify model arguments

#lm_model <- 
 # linear_reg() %>% # Model type: Linear Regression
  #set_engine("lm") # Computational engine: lm


#glmnet_model <- 
 # linear_reg(penalty = 1) %>% # Model type: Linear Regression
  #set_engine("glmnet") # Computational engine: glmnet

# Fit models --------------------------------------------------------------

tree_fit <- 
 tree_model %>% 
  fit(as.factor(DELIVERY_TYPE) ~ ., data = train_data)

#lm_fit <- 
 # lm_model %>% 
  #fit(DELIVERY_TYPE ~ ., data = train_data)

#glmnet_fit <- 
 # glmnet_model %>% 
  #fit(DELIVERY_TYPE ~ ., data = train_data)

# Inspect coefficients for lm

#lm_fit$fit %>% coef()

#lm_fit$fit %>% summary()


# Generate predictions ----------------------------------------------------

tree_pred <- predict(tree_fit, new_data = test_data)

# Data frame from test set with “attached” the model predictions
predictions <- test_data %>% 
  select(DELIVERY_TYPE) %>% # keep target variable (also known as the truth)
  mutate(tree_pred = tree_pred$.pred_class) # glmnet model predictions

# Calculate performance metrics -------------------------------------------

metrics(predictions, truth = as.numeric(DELIVERY_TYPE), estimate = as.numeric(tree_pred))

# Confusion matrix --------------------------------------------------------

conf_mat(predictions, truth = as.factor(DELIVERY_TYPE), estimate = tree_pred)


# SECOND PART  ------------------------------------------------------------
# Data pre-processing -----------------------------------------------------

df_rec <- recipe(DELIVERY_TYPE ~ ., data = train_data) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(gender_clean, age_cleaned, Gravida, Para, lengthCHART, abdominal, 
             weight_CHART, circumferenceCHART, gestational_final, LOS, hep_vaccine_total,  
             INSURANCE, Ethnicity_CLEAN, one_hot = TRUE)

# Specify model type and computational engine -----------------------------
rf_model <- 
  rand_forest() %>% 
  set_engine("randomForest") %>% 
  set_mode("classification") %>% 
  set_args(mtry = tune(), trees = tune())

gbt_model <- 
  boost_tree() %>%
  set_engine("xgboost") %>%
  set_mode("classification") %>%
  set_args(mtry = 4, trees = tune(), learn_rate = tune(), tree_depth = 6, min_n = 1)

# Create a regular tune grid ----------------------------------------------
rf_grid <- grid_regular(range_set(mtry(), c(3,5)),
                        range_set(trees(), c(100, 300))) 

gbt_grid <- grid_regular(range_set(trees(), c(300,600)),
                         range_set(learn_rate(trans = NULL), c(0.005, 0.02)),
                         levels = 4)

# Create model workflow ---------------------------------------------------
rf_wflow <- 
  workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(df_rec)

gbt_wflow <- 
  workflow() %>% 
  add_model(gbt_model) %>% 
  add_recipe(df_rec)

# Analyse resamples with hyperparameter tuning ----------------------------
system.time({
  set.seed(42)
  rf_res <- 
    rf_wflow %>% 
    tune_grid(resamples = train_folds, grid = rf_grid)
})

system.time({
  gbt_res <- 
    gbt_wflow %>% 
    tune_grid(resamples = train_folds, grid = gbt_grid, control = control_grid(verbose = TRUE))
})

# Collect metrics and compare ---------------------------------------------
# RF
collect_metrics(rf_res)

rf_res %>%
  collect_metrics() %>%
  mutate(learn_rate = factor(learn_rate)) %>%
  ggplot(aes(trees, mean, color = learn_rate)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) +
  scale_x_log10(labels = scales::label_number()) +
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0)

autoplot(rf_res)

show_best(x = rf_res, metric = "accuracy")

select_best(x = rf_res, metric = "accuracy")

# GBT

collect_metrics(gbt_res)

gbt_res %>%
  collect_metrics() %>%
  mutate(learn_rate = factor(learn_rate)) %>%
  ggplot(aes(trees, mean, color = learn_rate)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) +
  scale_x_log10(labels = scales::label_number()) +
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0)

autoplot(gbt_res)

show_best(x = gbt_res, metric = "accuracy")

select_best(x = gbt_res, metric = "accuracy")

# Finalise model workflow -------------------------------------------------
#RF
best_rf <- select_best(x = rf_res, metric = "accuracy")

final_wf_rf <- 
  rf_wflow %>% 
  finalize_workflow(best_rf)

#GBT
best_gbt <- select_best(x = gbt_res, metric = "accuracy")

final_wf_gbt <- 
  gbt_wflow %>% 
  finalize_workflow(best_gbt)

# Fit and predict ---------------------------------------------------------
#RF
rf_fit <- fit(final_wf_rf, train_data)

rf_pred_class <- predict(rf_fit, new_data = test_data, type = "class")

rf_pred_prob <- predict(rf_fit, new_data = test_data, type = "prob")

#GBT
gbt_fit <- fit(final_wf_gbt, train_data)

gbt_pred_class <- predict(gbt_fit, new_data = test_data, type = "class")

gbt_pred_prob <- predict(gbt_fit, new_data = test_data, type = "prob")

# Data frame from test set with “attached” the model predictions
#RF
predictions_rf <- test_data %>% 
  select(DELIVERY_TYPE) %>% # keep target variable (also known as the truth)
  bind_cols(., rf_pred_class, rf_pred_prob)

#GBT
predictions_gbt <- test_data %>% 
  select(DELIVERY_TYPE) %>% # keep target variable (also known as the truth)
  bind_cols(., gbt_pred_class, gbt_pred_prob)

# Calculate performance metrics -------------------------------------------

metrics(predictions_rf, truth = DELIVERY_TYPE, estimate = .pred_class)
metrics(predictions_gbt, truth = DELIVERY_TYPE, estimate = .pred_class)

# Confusion matrix --------------------------------------------------------

conf_mat(predictions_rf, truth = DELIVERY_TYPE, estimate = .pred_class)
conf_mat(predictions_gbt, truth = DELIVERY_TYPE, estimate = .pred_class)

# Receiver Operating Characteristic (ROC) curve ---------------------------
#RF

two_class_curve_rf <- roc_curve(predictions_rf, DELIVERY_TYPE, .pred_1, event_level = "second")

autoplot(two_class_curve_rf)

roc_auc(predictions_rf, DELIVERY_TYPE, .pred_1, event_level = "second")

#GBT
two_class_curve_gbt <- roc_curve(predictions_gbt, DELIVERY_TYPE, .pred_1, event_level = "second")

autoplot(two_class_curve_gbt)

roc_auc(predictions_gbt, DELIVERY_TYPE, .pred_1, event_level = "second")

#again


