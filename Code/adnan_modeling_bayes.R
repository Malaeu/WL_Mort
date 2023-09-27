library(tidyverse)
library(tidymodels)
library(pins)
library(vetiver)
library(plumber)
library(gt)
library(conflicted)
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
  step_normalize(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors()) |> 
  step_zv(all_predictors()) %>%
  step_downsample(death) 
  
  
  
glm_spec <-
  logistic_reg() |>
  set_engine("glm")

svm_spec <- svm_linear(cost = tune(), margin = tune()) %>% 
  set_engine("kernlab") %>% 
  set_mode("classification")

tree_spec <-
  rand_forest(min_n = tune()) |>
  set_engine("ranger") |>
  set_mode("classification")

xgb_spec <- boost_tree(tree_depth = tune(), learn_rate = tune(), loss_reduction = tune(), 
                      min_n = tune(), sample_size = tune(), trees = tune()) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

mlp_brulee_spec <-
  mlp(
    hidden_units = tune(), epochs = tune(),
    penalty = tune(), learn_rate = tune()
  ) %>%
  set_engine("brulee") %>%
  set_mode("classification")

nb_spec <- naive_Bayes(smoothness = tune(), Laplace = tune()) %>% 
  set_engine("naivebayes") %>% 
  set_mode("classification")

cit_spec <- decision_tree(tree_depth=tune(), min_n=tune()) %>%
  set_engine(engine = "partykit") %>%
  set_mode(mode = "classification") 




set.seed(1234)
adnan_folds <- vfold_cv(adnan_train, v=10, strata = death)

bayes_control <-
  control_bayes(no_improve = 30L, time_limit = 40, save_pred = TRUE)

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
      tree = tree_spec,
      torch = mlp_brulee_spec,
      svm_linear = svm_spec, 
      xgboost=xgb_spec, 
      naiveBayes=nb_spec, 
      tree_party=cit_spec
    )
  ) |>
  workflow_map("tune_bayes",
               iter = 100L,
               resamples = adnan_folds,
               control = bayes_control
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

