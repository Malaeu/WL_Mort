#Preliminaries:
knitr::opts_chunk$set(message=FALSE, warning=FALSE, eval = TRUE) #set eval = TRUE when run first

tibble::as_tibble(df, .name_repair = janitor::make_clean_names)

saveRDS(df_selected_vars_borut, file = "adnan_df_selected_vars_03_08_23.rds")
save(bca_acsn, file = "bca_acsn.Rds")
save(WL, file = "WL.Rds")
write_sav(df_selected_vars, "df_selected_vars_borut.sav")
write.xlsx(df_log, "df_log.xlsx")
save.image(file = "df_selected_vars_borut_07_08_23.RData")
rm(list = ls())

rm(list=ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  # data manipulation and visualization
  tidymodels,
  # modelling and machine learning
  themis,
  # deal with imbalanced datasets
  doParallel,
  # support for parallel computation
  gtsummary,
  # summarize statistical models
  gt,
  # create tables for reporting models
  bonsai,
  # decision trees
  discrim,
  # model discrimination
  finetune,
  # tuning tidymodels
  patchwork,
  # combine multiple ggplots
  vip,
  # variable importance
  DALEXtra,
  # explain machine learning models
  Hmisc,
  # Harrell Miscellaneous
  ROCR,
  # Visualizing classifier performance in R
  pander,
  # An R Pandoc Writer
  reshape2,
  # Flexibly Reshape Data
  lazyeval,
  # Lazy (Non-Standard) Evaluation
  moments,
  # Moments, cumulants, skewness, kurtosis, etc.
  entropy,
  # Estimation of Entropy, Mutual Information and Related Quantities
  funModeling,
  # contains heart_disease data
  minerva,
  # contains MIC statistic
  ggplot2,
  dplyr,
  gridExtra,
  # allow us to plot two plots in a row
  rio,
  # File import
  here,
  # File locator
  skimr,
  # get overview of data
  janitor,
  # adding totals and percents to tables
  scales,
  # easily convert proportions to percents
  flextable,
  # converting tables to pretty images
  devtools,
  finalfit,
  kableExtra,
  haven,
  openxlsx,
  forcats,
  survival,
  survminer,
  kernlab,
  xgboost,
  naivebayes,
  partykit,
  rminer,
  Boruta,
  randomForestExplainer,
  reticulate,
  e1071,
  agua,
  randomForest,
  scorecard,
  knitr,
  lubridate,
  modelsummary,
  opendatatoronto,
  pdftools,
  pointblank,
  readxl,
  mRMRe,
  stringi,
  testthat,
  moments,
  validate,
  listviewer,
  parttree
)

df <- df_s_nonzero_indx_voll

# dir.create("cl_diabetes", showWarnings = FALSE)
df_s_nonzero_indx_voll <- df_s_nonzero_indx_voll %>% janitor::clean_names()
df_with_timeS <- df_with_timeS %>% clean_names()
df_with_timeS <- df_with_timeS %>% make_clean_names()
df <- df %>% janitor::clean_names()


tidymodels_prefer() 

old_max <- getOption("max.print")
options(max.print = 5000)  # replace 'n' with the desired number. In this case, it can be 999999 or similar.
df_status(df)
glimpse(df)

# df <- df %>%
#   relocate(cutpoint_rounded, .after = nth(names(df), 326))
# df <- df %>%
#   relocate(time, .after = death)
df <- df %>%
  relocate(c(time, death), .after = patient_number)


df_status(adnan_df)





adnan_df <- adnan_df %>%
  mutate(across(where(is.character), as.factor))

# df <- df[,-c(1,2,5:9,43,317:319)]
# df <- df[,-c(306,307)]
# df <- df[,-c(217)]
# # # Assume df is your data frame
# # variable_names <- names(df_truncate_column_names)
# # Assume df is your data frame
# variable_names <- names(df)
# # Calculate the length of each variable name
# name_lengths <- sapply(variable_names, nchar)
# # Names with more than 50 characters
# long_names <- names(name_lengths)[name_lengths > 60]
# 
# # Print the long names
# print(long_names)
# 
# 
# # Print the lengths
# print(name_lengths)
# # Lengths of names with more than 50 characters
# long_name_lengths <- name_lengths[name_lengths > 59]
# 
# # Print the long names and their lengths
# print(long_name_lengths)
# 
# #df <- truncate_column_names(df)
# df <- df %>% janitor::clean_names()
# # 
# # # Rename the variable using base R
# # names(df_with_timeS)[names(df_with_timeS) == "death_on_the_list_w/o_ltx"] <- "death_on_the_list_w_o_ltx"
# 
# # Find the names of all categorical variables
# categorical_cols <- names(which(sapply(df, is.factor)))
# 
# df <- df %>%
#   mutate(across(all_of(categorical_cols), ~fct_relevel(., "Missing", after = Inf)))

variables_to_remove <-
  c(
    'log_epicardial_paracardial_fat_index',
    'creatinin_bilirubin_ratio',
    'log_epicardial_paracardial_fat_index_normalized',
    'creatinin_bilirubin_ratio_normalized',
    'creatinin_bilirubin_ratio_normalized_category',
    'bone_to_visceral_fat_index_normalized_cat',
    "exp_myosteatotic_fat_index_normalized_cat",
    "exp_myosteatotic_fat_index",
    "cutpoint_rounded",
    "cutpoint_vs_median",
    "cutpoint_median_diff",
    "exp_sarcopenia_index_normalized_cat",
    "exp_sarcopenia_index_cat",
    "cause_of_death",
    "lap_meld_death",
    "lab_meld_nt",
    "lap_meld_ltx",
    "ltx",
    "survival_days_until_nt",
    "survival_days_until_ltx",
    "survival_days_from_nt_until_last_contact",
    "death_on_the_list_without_ltx",
    "survival_days_from_wl_until_death",
    "inr_removal",
    "bili_removal",
    "crea_removal",
    "natrium_removal",
    "platelets_removal"
  )

# Use the select function with the - sign to deselect matching variables
df <- df_s_nonzero_indx_voll %>% select(-one_of(variables_to_remove))
# Use the select function with the - sign to deselect matching variables


# get list of categorical columns
categorical_cols <- names(which(sapply(df, is.factor)))
# print number of categorical columns
cat("Number of categorical columns: ", length(categorical_cols))

# Find the columns with more than 5 unique levels
cols_with_many_levels <- names(df)[sapply(df, function(x) is.factor(x) && length(unique(x)) > 5)]

# Function to reclassify categories
reclassify <- function(x) {
  count_level <- sort(table(x), decreasing = TRUE)
  x <- as.character(x)
  x[!(x %in% names(count_level)[1:4])] <- "Other"
  return(as.factor(x))
}

# Apply function to each column
df[cols_with_many_levels] <- lapply(df[cols_with_many_levels], reclassify)

head(df)

# Convert the 'death' variable to a factor
df$death <- as.factor(df$death)
df$death <- factor(df$death, levels = c(0, 1), labels = c("nein", "ja"))
df

df_status(df)

# Descriptive Statistics Overall Tabelle 287 Pat.

hd_tab1 <- df_selected_vars %>%
  tbl_summary(
    by = death,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2
  ) %>%
  add_p(test = list(
    all_continuous() ~ "kruskal.test", 
    all_categorical() ~ "fisher.test"
  )) %>% 
  add_overall() %>%
  modify_table_body(
    ~.x %>% 
      dplyr::filter(!is.na(p.value) & p.value < 0.05)
  ) %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**death**") %>%
  modify_caption("**Table 1: Descriptive Statistics Overall**")

hd_tab1

#save summary statistics:
hd_tab1 %>%
  as_gt() %>%
  gtsave("adnan_death_vs_all_significant.png", expand = 10)

# Extract the variable names from the hd_tab1 object
selected_vars <- hd_tab1$table_body %>%
  dplyr::filter(!is.na(p.value) & p.value < 0.05) %>%
  dplyr::pull(variable) %>%
  unique()

# Create a new dataset with only the selected variables and status Variable also all wanted variable
df_selected_vars <- df %>% select(c(patient_number,death),all_of(selected_vars),-time)
# Create a new dataset with only the selected variables and status Variable also all wanted variable
df_selected_vars <- df %>% select(c(patient_number,death),all_of(selected_vars))
# df_selected_vars <- df %>% select(all_of(selected_vars), -time)
df_status(df_selected_vars)

# Reproducibility 
set.seed(1005)

# Divide data into training and test: 
df_split <- df_selected_vars %>%
  initial_split(prop = 0.75, strata=death)

adnan_train_df <- training(df_split)
adnan_test_df  <-  testing(df_split)

# # Find variables in the dataframe with only one level
# to_drop <- sapply(df, function(x) length(unique(x)) > 5)
# 
# # Drop these variables
# df <- df[, !to_drop]
# # Count columns marked to be dropped
# drop_count <- sum(to_drop)
# 
# print(drop_count)
# 
# # Remove 'exp_myosteatotic_fat_index' from df
# df <- df %>% select(-creat_bil_ratio_norm)

# #Finden Sie die gemeinsamen Spaltennamen in beiden DataFrames
# common_columns <- intersect(colnames(df), colnames(df_selected_vars))
# common_columns



# # Überprüfen ob lap_meld_death erhalten ist
# "lap_meld_death" %in% names(df_selected_vars_train_df)
# # Remove the 'lap_meld_death' variable using subset()
# df_selected_vars_train_df <- subset(df_selected_vars_train_df, select = -lap_meld_death)
# 
# df_status(df_selected_vars_train_df)

# Update your adnan_recipe
adnan_recipe <- recipe(death ~ ., data = adnan_train_df) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_corr(all_predictors()) %>% 
  step_nzv(all_predictors()) %>% 
  step_downsample(death)

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

auto_ml_mod <- auto_ml() %>%  
  set_engine("h2o") %>% 
  set_mode("classification") %>% 
  translate()


# prepare cross validation 
set.seed(1001)
adnan_train_df_folds <- vfold_cv(adnan_train_df, v=5, strata = death)

# prepare workflow
wf_set <- workflow_set(
  preproc = list(mod = adnan_recipe), 
  models = list(log_reg=lr_mod, svm_linear = svm_mod, xgboost=xgb_mod, naiveBayes=nb_mod, tree=cit_mod)) 

# prepare grid: 
grid_ctrl <-
  control_grid(
    save_pred = TRUE,
    parallel_over = "everything",
    save_workflow = TRUE , 
    event_level = "second"
  )


# prepare parallel processing: 
cores <- parallel::detectCores(logical = TRUE)


# Create a cluster object and then register: 
cl <- makePSOCKcluster(cores) 
registerDoParallel(cl)

# Start hyperparameter tuning:
train_results <- wf_set %>%
  workflow_map(
    fn = 'tune_grid', 
    metrics = metric_set(roc_auc), 
    seed = 1503,
    resamples = adnan_train_df_folds, 
    grid = 160, 
    control = grid_ctrl 
  )

stopCluster(cl)

#plot results of hyperparameter tuning:
p1_adnan <- train_results %>%
  autoplot() +
  theme_minimal() +
  labs(title='Figure 1: Results Hyperparameter Tuning')
p1_adnan
ggsave(p1_adnan, file="cl_adnan/p1_adnan_death_all_boruta_MNMR.png")


xgb_results <- train_results %>% 
  extract_workflow_set_result("mod_xgboost") 

xgb_wf <- train_results %>% 
  extract_workflow("mod_xgboost")

svm_results <- train_results %>% 
  extract_workflow_set_result("mod_svm_linear") 

svm_wf <- train_results %>% 
  extract_workflow("mod_svm_linear")



cl <- makePSOCKcluster(cores) 
registerDoParallel(cl)

#Increase performance with simulated annealing

set.seed(1005)
xgb_sa <- xgb_wf %>%
  tune_sim_anneal(
    resamples =adnan_train_df_folds,
    metrics = metric_set(roc_auc), 
    initial = xgb_results,
    iter = 4000, 
    control = control_sim_anneal(verbose = TRUE, 
                                 no_improve = 20L, event_level = "second", cooling_coef = 0.1))

stopCluster(cl)

# save max auc:
auc_out <- xgb_sa  %>% 
  collect_metrics() %>% 
  slice_max(mean) %>%
  pull(mean)


# visualize sim annealing:
p2_adnan <- autoplot(xgb_sa, type = "performance", metric = 'roc_auc') +
  geom_hline(yintercept=auc_out, linetype="dashed", color = 'red') +
  labs(title='Figure 2: Performance Improvement by Simulated Annealing ') +
  theme_minimal()

ggsave(p2_adnan, file="cl_adnan/p2_adnan_ohne_lap_meld_death_all.png")

# extract model fit after simulated annealing: 
xgb_fit <- xgb_sa %>% 
  extract_workflow() %>%
  finalize_workflow(xgb_sa %>% select_best())  %>%
  fit(data = adnan_train_df) %>%
  extract_fit_parsnip()

# Variable importance plot:
p3_diab <- xgb_fit %>%
  vip() +
  theme_minimal() +
  labs(title="Figure 3: Variable Importance")

ggsave(p3_diab, file="cl_adnan/p3_adnan_ohne_lap_meld_death.png")


# Besseres model ist svm_linear
#Increase performance with simulated annealing

cl <- makePSOCKcluster(cores) 
registerDoParallel(cl)

set.seed(1005)
svm_sa <- svm_wf %>%
  tune_sim_anneal(
    resamples =adnan_train_df_folds,
    metrics = metric_set(roc_auc), 
    initial = svm_results,
    iter = 400, 
    control = control_sim_anneal(verbose = TRUE, 
                                 no_improve = 20L, event_level = "second", cooling_coef = 0.1))

stopCluster(cl)

# save max auc:
auc_out <- svm_sa  %>% 
  collect_metrics() %>% 
  slice_max(mean) %>%
  pull(mean)


# visualize sim annealing:
p2_adnan <- autoplot(svm_sa, type = "performance", metric = 'roc_auc') +
  geom_hline(yintercept=auc_out, linetype="dashed", color = 'red') +
  labs(title='Figure 2: Performance Improvement by Simulated Annealing ') +
  theme_minimal()

ggsave(p2_adnan, file="cl_adnan/p2_adnan_ohne_lap_meld_death_svm_all.png")

# extract model fit after simulated annealing: 
svm_fit <- svm_sa %>% 
  extract_workflow() %>%
  finalize_workflow(svm_sa %>% select_best())  %>%
  fit(data = adnan_train_df) %>%
  extract_fit_parsnip()

# Variable importance plot:
p3_diab <- svm_fit %>%
  vip() +
  theme_minimal() +
  labs(title="Figure 3: Variable Importance")

# Installieren und laden Sie das rminer-Paket, falls noch nicht geschehen
# install.packages("rminer")
library(rminer)

# Modifizierte Importance-Funktion
Importance_mod <- function(M, data) {
  if (any(class(M) == "model")) {
    M <- M$model
  }
  return(Importance(M, data))
}
# Berechnen Sie die Variablenwichtigkeit für das SVM-Modell
svm_importance <- Importance_mod(svm_fit, data = df_selected_vars_train_df)

# Erstellen Sie einen Variablenwichtigkeitsplot


plot(svm_importance)



# Calculate Permutation Feature Importance
pfi <- vip::vip(svm_fit, method = "permute", data = df_selected_vars_train_df)
plot(pfi)


ggsave(p3_diab, file="cl_adnan/p3_adnan_ohne_lap_meld_death_svm.png")
