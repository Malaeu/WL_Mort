library(tidyverse)
library(tidymodels)
library(themis)
library(doParallel)
library(gtsummary) 
library(gt)
library(bonsai) 
library(discrim)
library(finetune)
library(patchwork)
library(vip)
library(DALEXtra) 
library(censored)
library(censored)
# prepare parallel processing: 
cores <- parallel::detectCores(logical = TRUE)


# Create a data frame
data <- data.frame(death, dialysis_cat, ventilation_cat, mortality_increase, imat_normalized,
                   visceral_to_subcutaneous_fat_index_extended, known_mortality, muscle_normalized)

# Fit the linear model
model <- lm(death ~ dialysis_cat + ventilation_cat + mortality_increase + imat_normalized +
              visceral_to_subcutaneous_fat_index_extended + known_mortality +
              ventilation_cat:dialysis_cat + muscle_normalized:mortality_increase +
              known_mortality:muscle_normalized + known_mortality:visceral_to_subcutaneous_fat_index_extended +
              dialysis_cat:mortality_increase + dialysis_cat:visceral_to_subcutaneous_fat_index_extended +
              dialysis_cat:known_mortality + ventilation_cat:mortality_increase +
              ventilation_cat:imat_normalized + ventilation_cat:known_mortality, data = df)

# Print the model
print(model)
plot(model)

df[, "death"] <- as.factor(df[, "death"]) 
set.seed(1)
ad_split <- initial_split(df, strata = "death")
ad_train <- training(ad_split)
ad_test <- testing(ad_split)

set.seed(2)
ad_folds <- vfold_cv(ad_train, v = 20,strata = death)

library(bonsai)


bt_spec <-
  boost_tree(learn_rate = tune(), stop_iter = tune(), trees = 1000) %>%
  set_engine("lightgbm", num_leaves = tune()) %>%
  set_mode("classification")

# try out different ml-approaches: 
lr_mod <- logistic_reg() %>%
  set_engine("glm") %>% 
  set_mode("classification")

svm_mod <- svm_linear(cost = tune(), margin = tune()) %>% 
  set_engine("kernlab") %>% 
  set_mode("classification")

xgb_mod <- boost_tree(tree_depth = tune(), learn_rate = tune(), loss_reduction = tune(), 
                      min_n = tune(), sample_size = tune(), trees = tune()) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

nb_mod <- naive_Bayes(smoothness = tune(), Laplace = tune()) %>% 
  set_engine("naivebayes") %>% 
  set_mode("classification")

cit_mod <- decision_tree(tree_depth=tune(), min_n=tune()) %>%
  set_engine(engine = "partykit") %>%
  set_mode(mode = "classification") 

rf_spec <-
  rand_forest(trees = 200) %>%
  set_engine("aorsf") %>%
  set_mode("censored regression")



bt_rec <- 
  recipe(death ~ dialysis_cat + ventilation_cat + mortality_increase + imat_normalized +
           visceral_to_subcutaneous_fat_index_extended + known_mortality, data = ad_train) %>%
  # Add preprocessing steps relevant to your variables
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_interact(terms = ~ dialysis_cat_yes:ventilation_cat_yes) %>%
  step_interact(terms = ~ known_mortality:visceral_to_subcutaneous_fat_index_extended) %>%
  step_interact(terms = ~ dialysis_cat_yes:mortality_increase) %>%
  step_interact(terms = ~ dialysis_cat_yes:visceral_to_subcutaneous_fat_index_extended) %>%
  step_interact(terms = ~ dialysis_cat_yes:known_mortality) %>%
  step_interact(terms = ~ ventilation_cat_yes:mortality_increase) %>%
  step_interact(terms = ~ ventilation_cat_yes:imat_normalized) %>%
  #step_interact(terms = ~ known_mortality * muscle_normalized) %>%
  step_interact(terms = ~ ventilation_cat_yes:known_mortality) |> 
  step_downsample(death) 

  # step_log(Gr_Liv_Area, base = 10) %>% 
  # step_other(Neighborhood, threshold = 0.05) %>% 
  # 
  # step_ns(Latitude, Longitude, deg_free = tune())
  # 
# bt_wflow <- workflow(bt_rec, bt_spec)
# extract_parameter_set_dials(bt_wflow)
# set.seed(3)
# registerDoMC(cores)
# bt_time_grid <- system.time(
#   bt_res_grid <- tune_grid(bt_wflow, ad_folds, grid = 50)
# )
# 
# autoplot(bt_res_grid)
# 
# collect_metrics(bt_res_grid) %>%
#   filter(.metric == "roc_auc") %>%
#   arrange(mean)
# 
# 


# prepare workflow
wf_set <- workflow_set(
  preproc = list(mod = bt_rec), 
  models = list(gbm=bt_spec,log_reg=lr_mod, svm_linear = svm_mod, xgboost=xgb_mod, naiveBayes=nb_mod, tree=cit_mod)) 

# prepare grid: 
grid_ctrl <-
  control_grid(
    save_pred = TRUE,
    parallel_over = "everything",
    save_workflow = TRUE , 
    event_level = "second"
  )

# Start hyperparameter tuning:
train_results <- wf_set %>%
  workflow_map(
    fn = 'tune_grid', 
    metrics = metric_set(roc_auc), 
    seed = 1503,
    resamples = ad_folds, 
    grid = 25, 
    control = grid_ctrl 
  )


#Create a cluster object and then register: 
cl <- makePSOCKcluster(cores) 
registerDoParallel(cl)
registerDoMC(cores = 24)
set.seed(6)

# Start hyperparameter tuning:
train_results <- wf_set %>%
  workflow_map(
    fn = 'tune_grid', 
    metrics = metric_set(roc_auc), 
    seed = 1503,
    resamples = ad_folds, 
    grid = 25, 
    control = grid_ctrl 
  )
stopCluster(cl)