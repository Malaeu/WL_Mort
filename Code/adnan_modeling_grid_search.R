library(tidyverse)
library(tidymodels)
library(pins)
library(vetiver)
library(plumber)
library(gt)
library(conflicted)
library(car)        # extracts model results
library(MASS)       # provides "birthwt" dataset
library(ISLR)       # provides "Wage" dataset
library(tictoc)     # checks running time
library(sjPlot)     # visualizes model results
library(glmulti)    # finds the BEST model
library(flextable)  # beautifies tables
library(tidyverse)  # provides a lot of useful stuff !!! 
library(performance)# checks and compares quality of models
tidymodels_prefer()
conflict_prefer("penguins", "palmerpenguins")
theme_set(theme_bw())
options(tidymodels.dark = TRUE)






set.seed(1234)
adnan_split <- initial_split(adnan_df, strata = death)
adnan_train <- training(adnan_split)
adnan_test <- testing(adnan_split)

adnan_rec<-
  recipe(death ~ ., data = adnan_train) |>
  step_YeoJohnson(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors()) |> 
  step_zv(all_predictors()) %>%
  step_downsample(death) 



# Define a custom fit function
fit_cox <- function(x, y, strata, ...) {
  survival::coxph(survival::Surv(y[[1]], y[[2]]) ~ x + strata(strata), data = x)
}

# Add the fit function to the model specification
cox_spec$fit <- fit_cox



glm_spec <-
  logistic_reg() |>
  set_engine("glm")

svm_spec <- svm_linear(cost = tune(), margin = tune()) %>% 
  set_engine("kernlab") %>% 
  set_mode("classification")


xgb_spec <- boost_tree(tree_depth = tune(), learn_rate = tune(), loss_reduction = tune(), 
                       min_n = tune(), sample_size = tune(), trees = tune()) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")


nb_spec <- naive_Bayes(smoothness = tune(), Laplace = tune()) %>% 
  set_engine("naivebayes") %>% 
  set_mode("classification")

cit_spec <- decision_tree(tree_depth=tune(), min_n=tune()) %>%
  set_engine(engine = "partykit") %>%
  set_mode(mode = "classification") 

# prepare grid: 
grid_ctrl <-
  control_grid(
    save_pred = TRUE,
    parallel_over = "everything",
    save_workflow = TRUE , 
    event_level = "second"
  )


set.seed(1234)
adnan_folds <- vfold_cv(adnan_train, v=10, strata = death)


# prepare parallel processing: 
cores <- parallel::detectCores(logical = TRUE)


# Create a cluster object and then register: 
cl <- makePSOCKcluster(cores) 
registerDoParallel(cl)


workflow_set <-
  workflow_set(
    preproc = list(adnan_rec),
    models = list(
      glm = glm_spec,
      svm_linear = svm_spec, 
      xgboost=xgb_spec, 
      naiveBayes=nb_spec, 
      tree_party=cit_spec
    )
  ) |>
  workflow_map("tune_grid",
               metrics = metric_set(roc_auc), 
               seed = 1234,
               resamples = adnan_folds,
               grid = 100, 
               control = grid_ctrl
  )

stopCluster(cl)


rank_results(workflow_set,
             rank_metric = "roc_auc",
             select_best = TRUE
) |>
  gt()

#plot results of hyperparameter tuning:
p1_adnan <- workflow_set %>%
  autoplot() +
  theme_minimal() +
  labs(title='Figure 1: Results Hyperparameter Tuning')
p1_adnan

