
saveRDS(data_backup, "data_backup_20_09_2023.rds")
saveRDS(final_df, "final_df_20_09_2023.rds")
saveRDS(ordered_models_by_aic, "ordered_models_by_aic_20_09_2023.rds")

write_xlsx(list(data_backup = data_backup, final_df = final_df), "combined_20_09_2023.xlsx")

## Strategies to handle imbalanced classes
## 

library(writexl)
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
library(funModeling)
library(Boruta)
library(h2o)
library(tictoc)     # checks running time
library(sjPlot)     # visualizes model results
library(glmulti)    # finds the BEST model
library(flextable)  # beautifies tables
library(tidyverse)  # provides a lot of useful stuff !!! 
library(performance)# checks and compares quality of models
library(caret)
library(mRMRe)
library(DMwR)
h2o.init()



df_status(data_backup)
data <- data_backup

# Split dataset into training/testing sets
index <- createDataPartition(data[["death"]], p = 0.75, list = FALSE)
train_set <- data[index, ]
test_set <- data[-index, ]
# For methods that can handle both numeric and factor variables
train_set_mixed <- train_set

# For methods that require all variables to be numeric
train_set_numeric <- train_set %>%
  mutate(across(where(is.factor), ~ as.numeric(.) - 1))


train_set <- train_set |> select(-time)
test_set <- test_set |> select(-time)

# Apply SMOTE to the training set
train_set$death <- as.factor(train_set$death)

training_balanced <- SMOTE(death ~ ., train_set, k = 10, perc.under = 100, perc.over = 8000)


set.seed(123)
vb_df <- data
vb_df$death <- as.factor(vb_df$death)
df_status(vb_df)


vb_df <- vb_df |> select(-time)
vb_df <- vb_df |> select(-known_mortality)
vb_df <- vb_df |> select(-patient_number_cut)
vb_df <- vb_df |> select(-blood_type)

perc_under_calc <- (200 / 88) * 100
vb_df_balanced <- SMOTE(death ~ ., vb_df, k = 7, perc.under = perc_under_calc, perc.over = 100)
table(vb_df_balanced$death)
vb_df <- SMOTE(death ~ ., vb_df, k = 7, perc.under = 100, perc.over = 200)
table(vb_df$death)

vb_df <- data %>%
  mutate(across(where(is.factor), ~ as.numeric(.) - 1))


vb_split <- initial_split(vb_df, strata = death)
vb_train <- training(vb_split)
vb_test <- testing(vb_split)




xgb_spec <- boost_tree(
  trees = 5000,
  tree_depth = tune(), min_n = tune(),
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune()                          ## step size
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

xgb_spec

xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), vb_train),
  learn_rate(),
  size = 50
)

xgb_grid

xgb_wf <- workflow() %>%
  add_formula(death ~ .) %>%
  add_model(xgb_spec)

xgb_wf

set.seed(123)
vb_folds <- vfold_cv(vb_train, strata = death)

vb_folds

# prepare parallel processing: 
cores <- parallel::detectCores(logical = TRUE)

# Create a cluster object and then register: 
cl <- makePSOCKcluster(cores) 
registerDoParallel(cl)

set.seed(234)
tic()
xgb_res <- tune_grid(
  xgb_wf,
  resamples = vb_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)
toc() # 

xgb_res
stopCluster(cl)

collect_metrics(xgb_res)

xgb_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, mtry:sample_size) %>%
  pivot_longer(mtry:sample_size,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")
show_best(xgb_res, "roc_auc",n = 10)
best_auc <- select_best(xgb_res, "roc_auc")
best_auc

final_xgb <- finalize_workflow(
  xgb_wf,
  best_auc
)

final_xgb

final_xgb %>%
  fit(data = vb_train) %>%
  extract_fit_parsnip() %>%
  vip(geom = "point")
final_xgb

final_res <- last_fit(final_xgb, vb_split)



collect_metrics(final_res)
final_res %>%
  collect_predictions() %>%
  roc_curve(death, .pred_1) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(linewidth = 1.5, color = "midnightblue") +
  geom_abline(
    lty = 2, alpha = 0.5,
    color = "gray50",
    size = 1.2
  )


#Use racing to tune xgboost
#
#
#
#

# prepare parallel processing:
cores <- parallel::detectCores(logical = TRUE)

# Create a cluster object and then register:
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)

set.seed(234)
tic()
xgb_res <- tune_race_anova(
  xgb_wf,
  resamples = vb_folds,
  metrics = metric_set(mn_log_loss),
  control = control_race(verbose_elim = TRUE)
)
toc() # 

xgb_res
stopCluster(cl)

plot_race(xgb_res)
show_best(xgb_res)

xgb_last <- xgb_wf %>%
  finalize_workflow(select_best(xgb_res, "mn_log_loss")) %>%
  last_fit(vb_split)

xgb_last

collect_predictions(xgb_last) %>%
  mn_log_loss(is_home_run, .pred_HR)

library(vip)
extract_workflow(xgb_last) %>%
  extract_fit_parsnip() %>%
  vip(geom = "point", num_features = 15)

collect_metrics(xgb_last)
xgb_last %>%
  collect_predictions() %>%
  roc_curve(death, .pred_0) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(linewidth = 1.5, color = "midnightblue") +
  geom_abline(
    lty = 2, alpha = 0.5,
    color = "gray50",
    size = 1.2
  )





# Convert 'death' column to factor
# data.hex$death <- h2o.asfactor(data.hex$death)

# Define predictor variables and response variable

vb_df <- vb_df |> select(-time)
vb_df <- vb_df |> select(-known_mortality)
vb_df <- vb_df |> select(-patient_number_cut)
vb_df <- vb_df |> select(-blood_type)

X <- names(vb_df[,-which(names(vb_df) == "death")])
y <- "death"

data.hex <- as.h2o(vb_df)
h2o.describe(data.hex)

# Split your data into 3 and save into variable "splits"
data.hex$death <- h2o.asfactor(data.hex$death)
splits <- h2o.splitFrame(data.hex, c(0.7), seed = 42)
data.hex.train <- splits[[1]]
data.hex.test <- splits[[2]]


aml <- h2o.automl(x = X,
                  y = y,
                  training_frame = data.hex.train,
                  
                  nfolds = 10,
                  max_models = 50,
                  #exclude_algos = c("GBM"),
                  balance_classes = TRUE,
                  
                  project_name = "classification",
                  sort_metric = "AUC",
                  seed = 1)

# View the AutoML Leaderboard
lb <- h2o.get_leaderboard(aml)
h2o.head(lb, n = 25)

# Get leaderboard with 'extra_columns = 'ALL'
lb2 <- h2o.get_leaderboard(aml, extra_columns = "ALL")
h2o.head(lb2, n = 25)

# Save the test performance of the leader model
aml_cl_test_perf <- h2o.performance(aml@leader, data.hex.test)
aml_cl_test_perf

# Make predictions
aml_cl_pred <- h2o.predict(aml@leader, data.hex.test)
h2o.head(aml_cl_pred, n=10)

h2o.get_best_model(aml)
# Get model ids for all models in the AutoML Leaderboard
model_ids <- as.data.frame(aml@leaderboard$model_id)[,1]
# Get the "All Models" Stacked Ensemble model
se <- h2o.getModel(grep("StackedEnsemble_AllModels", model_ids, value = TRUE)[1])
# Get the Stacked Ensemble metalearner model
metalearner <- h2o.getModel(se@model$metalearner$name)
h2o.varimp(metalearner)
h2o.varimp_plot(metalearner)

Explain an AutoML object
exa <- h2o.explain(aml, data.hex.test)
exa

# Explain a single H2O model (e.g. leader model from AutoML)
exm <- h2o.explain(aml@leader, data.hex.test)
exm

shapr_plot <- h2o.shap_explain_row_plot(aml@leader, data.hex.test, row_index = 1)
shapr_plot

x <- vb_df |> select(all_of(X))
y <- vb_df |> select(death)
y <- data %>% select(death) |> 
  mutate(across(where(is.factor), ~ as.numeric(.) - 1))

vb_df$death <- as.numeric(vb_df$death)

# 10-Fold Kreuzvalidierung
niter <- 10

# 269 Beobachtungen
n <- 269

# 20% der Daten für das Testset, also etwa 54 Beobachtungen
ntest <- round(n * 0.2)

# Aufruf der Funktion
my.split <- generate.split(niter, n, ntest)



my.split<-generate.split(niter=10,n=269,ntest=54)



spl <- wilcox.selection.split(x,y,my.split,algo="new",pvalue=T) 

# Run the wilcox.selection.split function
results <- wilcox.selection.split(x, y, split, algo = "new", pvalue = TRUE)

# Durchschnittlicher Rang und P-Wert für jede Variable
avg_rank <- rowMeans(spl$ordering.split)
avg_pvalue <- rowMeans(spl$pvalue.split)

# Kombinieren der Durchschnittswerte in einem Dataframe
result_df <- data.frame(
  Variable = colnames(spl$pvalue.split),
  Avg_Rank = avg_rank,
  Avg_PValue = avg_pvalue
)

# Sortieren des Dataframes nach dem Durchschnittsrang
result_df <- result_df[order(result_df$Avg_Rank),]

# Auswahl der Top-10-Variablen
top_10_vars <- head(result_df, 10)

# Ausgabe der Top-10-Variablen
print(top_10_vars)

