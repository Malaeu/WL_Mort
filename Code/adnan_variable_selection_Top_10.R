# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload = TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
pkgs <- c("RCurl","jsonlite")
for (pkg in pkgs) {
  if (!(pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

# Now we download, install and initialize the H2O package for R.
install.packages("h2o", type = "source", repos = "https://h2o-release.s3.amazonaws.com/h2o/rel-3.42.0/3/R")



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
library(DALEX) 
library("iml")
library(pre)
library(mRMRe) # Minimum Redundancy, Maximum Relevance
library(xgboost) # Gradient boosting framework
library(aorsf) # Adaptive Outlier Rejection Subset Feature selection
library(glmnet) # Generalized Linear Models with L1 and L2 regularization
library(funModeling) # Data profiling, variable importance, and more
library(h2o)
library(ROSE) # für SMOTE
h2o.init()


df_status(data)
data <- data[,-42]

#create summary statistics:
hd_tab1 <- data[,c(boruta_result_final_vec,"death")] %>% 
  tbl_summary(by = death,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)"),
              digits = all_continuous() ~ 2) %>%
  add_p(test = list(all_continuous() ~ "t.test", 
                  all_categorical() ~ "chisq.test.no.correct")) %>%
  add_overall() %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Waitinglist Mortality**") %>%
  modify_caption("**Table 1: Descriptive Statistics Data**") 

#save summary statistics:
hd_tab1 %>%
  as_gt() %>%
  gtsave("Results/Diskriptiv_Boruta_tab1.png", expand = 900)

funMod_Vars <- fun_modeling_result_filter$var
#create summary statistics:
hd_tab1 <- data[,c(funMod_Vars,"death")] %>% 
  tbl_summary(by = death,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)"),
              digits = all_continuous() ~ 2) %>%
  add_p(test = list(all_continuous() ~ "t.test", 
                    all_categorical() ~ "chisq.test.no.correct")) %>%
  add_overall() %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Waitinglist Mortality**") %>%
  modify_caption("**Table 1: Descriptive Statistics Data**") 

#save summary statistics:
hd_tab1 %>%
  as_gt() %>%
  gtsave("Results/Diskriptiv_FunMod_tab1.png", expand = 900)



# Split dataset into training/testing sets
index <- createDataPartition(data[["death"]], p = 0.75, list = FALSE)
train_set <- data[index, ]
test_set <- data[-index, ]
# For methods that can handle both numeric and factor variables
train_set_mixed <- train_set

# For methods that require all variables to be numeric
train_set_numeric <- train_set %>%
  mutate(across(where(is.factor), ~ as.numeric(.) - 1))

X <-  names(data[which(names(data) != "death")])
y <- data$death

# Calculate variable importance using each method
boruta_result <- Boruta(as.formula(paste("death", "~.", sep = "")), data = train_set, doTrace = 0)
print(boruta_result)
boruta_result_final <- TentativeRoughFix(boruta_result)
plot(boruta_result_final, xlab = "", xaxt = "n")
lz <- lapply(1:ncol(boruta_result_final$ImpHistory),function(i)
  boruta_result_final$ImpHistory[is.finite(boruta_result_final$ImpHistory[,i]),i])
 names(lz) <- colnames(boruta_result_final$ImpHistory)
 Labels <- sort(sapply(lz,median))
 axis(side = 1,las = 2,labels = names(Labels),
       at = 1:ncol(boruta_result_final$ImpHistory), cex.axis = 0.7)

boruta_result_final_vec <- getSelectedAttributes(boruta_result_final, withTentative = F)

attStats(boruta_result_final) %>%
  filter(decision == "Confirmed") %>%
  filter(medianImp > 4.04) |> 
  arrange(desc(medianImp))

boruta_result_final_vec <- getSelectedAttributes(boruta_result_final, withTentative = F)



ranger_result <- ranger(as.formula(paste("death", "~.", sep = "")), data = train_set, importance = "impurity")
model <-  Predictor$new(ranger_result, data = X, y = y)
effect = FeatureEffects$new(model)
effect$plot(funMod_Vars)
imp <- FeatureImp$new(model, loss = "mae")
library("ggplot2")
plot(imp)
pre_result <- pre(death ~ ., data = data,family = "binomial")
pre_result_imps <- importance(pre_result, round = 4,global = T)
par(mfrow = c(1, 2))
expl <- explain(pre_result, newdata = data[1:2, ], cex = .8)

# prepare parallel processing: 
cores <- parallel::detectCores(logical = TRUE)

# Create a cluster object and then register: 
cl <- makePSOCKcluster(cores) 
registerDoParallel(cl)
set.seed(44)
nullmods <- bsnullinteract(pre_result)
int <- interact(pre_result, nullmods = nullmods)
corplot(pre_result)
stopCluster(cl)
plot(pre_result, nterms = 9, cex = .5)

caret_result <- train(as.formula(paste("death", "~.", sep = "")), data = train_set, method = "ranger", trControl = trainControl(method = "cv"))

mrmre_data <- mRMR.data(data = train_set_numeric)
mrmre_result <- mRMR.classic("mRMRe.Filter", data = mrmre_data, target_indices = 1, feature_count = ncol(train_set) - 1)



cols_to_keep <- setdiff(names(train_set_numeric), "death")
train_set_subset <- train_set_numeric[, cols_to_keep]

dtrain <- xgb.DMatrix(data = as.matrix(train_set_subset), label = train_set_numeric[["death"]])
xgb_result <- xgboost(data = dtrain, nrounds = 50, objective = "binary:logistic")

#aorsf_result <- aorsf(df_train, target_var, ntree = 100)



glmnet_result <- cv.glmnet(x = as.matrix(train_set_subset), y = train_set_numeric[["death"]], alpha = 1)

vip_result <- vip::vip(ranger_result, num_features = 15)
fun_modeling_result <- var_rank_info(train_set, "death")
fun_modeling_result_filter <-
  fun_modeling_result %>% select(var, gr) %>% filter(gr > 0.074) %>% arrange(desc(gr))



# Plotting 
ggplot(fun_modeling_result, 
       aes(x = reorder(var, gr), 
           y = gr, fill = var)
) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  theme_bw() + 
  xlab("") + 
  ylab("Variable Importance 
       (based on Information Gain)"
  ) + 
  guides(fill = FALSE)



# Combine results into one dataframe
result_df <- bind_cols(boruta_result, ranger_result, caret_result, mrmre_result, xgb_result, glmnet_result,vip_result,fun_modeling_result)



set.seed(123)


vb_df <- data %>%
  mutate(across(where(is.factor), ~ as.numeric(.) - 1))

vb_df$death <- as.factor(vb_df$death)

vb_split <- initial_split(vb_df, strata = death)
vb_train <- training(vb_split)
vb_test <- testing(vb_split)




xgb_spec <- boost_tree(
  trees = 1000,
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
  size = 30
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
xgb_res <- tune_grid(
  xgb_wf,
  resamples = vb_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)

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
show_best(xgb_res, "roc_auc")
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
  roc_curve(death, .pred_0) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(linewidth = 1.5, color = "midnightblue") +
  geom_abline(
    lty = 2, alpha = 0.5,
    color = "gray50",
    size = 1.2
  )

data.hex <- as.h2o(data)
# Splits data in prostate data frame with a ratio of 0.75
data.hex.split <- h2o.splitFrame(data = data.hex ,
                                   ratios = 0.75)
# Creates training set from 1st data set in split
data.hex.train <- data.hex.split[[1]]
# Creates testing set from 2st data set in split
data.hex.test <- data.hex.split[[2]]

X <-  names(data[which(names(data) != "death")])
y <- "death"


aml <- h2o.automl(x = X, 
                  y = y,
                  training_frame = data.hex.train,
                  validation_frame = data.hex.test,
                  max_models = 100,
                  exclude_algos = c("GBM"),
                  seed = 1)
aml@leaderboard
