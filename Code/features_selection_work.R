library(tidyfit)
library("faux")
library("DataExplorer")
library("caret")
library("randomForest")
library(mRMRe)
showClass("mRMRe.Filter")

df_numeric_only <- rename(df_numeric_only, target = death)
# Setze die Anzahl der Threads
set.thread.count(2)

# Erstelle ein mRMRe.Data Objekt mit Ihren Daten
data_mrmr <- mRMR.data(data = df_numeric_only[, predictors_numeric])

# Führe die mRMR Feature Selection durch
# In diesem Fall nehmen wir an, dass "death" die erste Spalte in Ihren Prädiktoren ist
fs <- new("mRMRe.Filter", data = data_mrmr, target_indices = 1, levels = c(8, 2, 1, 1, 1, 1))

# Gebe die Indizes der ausgewählten Merkmale aus
print(solutions(fs)[[1]])

# Gebe die Namen der ausgewählten Merkmale aus
print(apply(solutions(fs)[[1]], 2, function(x, y) { return(y[x]) }, y=featureNames(data_mrmr)))



#Filter Methods: Correlation-based Feature Selection
#This method uses the correlation between each predictor and the response
#variable to select features. Note that you may want to use a different measure
#of association if your predictors are not continuous or if their relationship
#with the response is not linear.

# Correlation-based Feature Selection
correlation <- cor(df_numeric_only)
# Get absolute value of correlation with the response
correlation_with_death <- abs(correlation[, "death"])
# Order predictors by correlation and select the top 10
top_correlation <- sort(correlation_with_death, decreasing = TRUE)[1:10]

#Wrapper Methods: Recursive Feature Elimination (RFE)
#Recursive Feature Elimination (RFE) is a type of wrapper feature selection
#method. This method uses a machine learning algorithm as a black box to rank
#features by importance and recursively eliminates less important features.

# Recursive Feature Elimination (RFE)
library(tidymodels)

# Create a model specification
spec <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

# Create a recipe
rec <- recipe(death ~ ., data = df_numeric_only)

# Create the workflow
wflow <- workflow() %>% 
  add_recipe(rec) %>% 
  add_model(spec)

# Perform RFE
ctrl <- rfe_control(functions = lrFuncs, verbose = FALSE)
results <- rfe(df_numeric_only[, -1], df_numeric_only$death, sizes = c(1:ncol(df_numeric_only)-1), rfeControl = ctrl)

# Get the top 10 predictors
predictors(results, n = 10)



# Install and load the finetune package
install.packages("finetune")
library(finetune)

# Create a recipe
rec <- recipe(target ~ ., data = df_numeric_only[,-32]) %>% 
  step_corr(all_predictors()) %>% 
  step_nzv(all_predictors()) %>% 
  step(all_predictors(), outcome = "target")

# Create a model specification
model <- logistic_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")

# Create a workflow
workflow <- workflow() %>% 
  add_recipe(rec) %>% 
  add_model(model)

# Perform tuning
tune_results <- tune_bayes(
  workflow,
  resamples = cv(df_numeric_only[,-32], strata = "target"),
  initial = 10, 
  total = 50,
  control = control_bayes(verbose = TRUE)
)

# Print the results
print(tune_results)

# Get the best parameters
best_params <- select_best(tune_results)


# Install and load the caret package
#install.packages("caret")
library(caret)

# Set up the control parameters
ctrl <- rfeControl(functions = rfFuncs, method = "cv", number = 10)

# Perform the RFE
result <- rfe(df_numeric_only[, -c(1,32)], df_numeric_only$death, sizes = c(2:10), rfeControl = ctrl)

# Print the result
print(result)

# Get the optimal predictors
optimal_predictors <- predictors(result)

# Lasso Regression
library(glmnet)

# Prepare the data
x <- as.matrix(df_numeric_only[, -c(1,32)])
y <- df_numeric_only$death

# Run Lasso regression
fit <- glmnet(x, y, alpha = 1, lambda = cv.glmnet(x, y, alpha = 1)$lambda.min)

# Get coefficients
coefficients <- coef(fit, s = cv.glmnet(x, y, alpha = 1)$lambda.min)

# Convert to a vector
coef_vector <- as.vector(coefficients)

# Get the names of the variables
variable_names <- rownames(coefficients)

# Combine the variable names and coefficients into a data frame
coef_df <- data.frame(variable = variable_names, coefficient = coef_vector)

# Remove the first row (this is the intercept)
coef_df <- coef_df[-1, ]

# Order by the absolute value of the coefficient
coef_df <- coef_df[order(abs(coef_df$coefficient), decreasing = TRUE), ]

# Get the names of the top 10 variables
top_10_predictors <- coef_df$variable[1:10]

# Print the variable names
print(top_10_predictors)






# Get the top 10 variable names
top_10_predictors <- names(coef(fit)[order(coef(fit), decreasing = TRUE)])[1:10]

# Print the variable names
print(top_10_predictors)
plot_intro(df_numeric_only)
plot_bar(df_numeric_only)
plot_correlation(df_numeric_only)



# Define the control using a random forest selection function
control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 10) # number of folds

# Features
x <- df_numeric_only[, predictors_numeric]

# Target variable
y <- df_numeric_only$target

# Training: 80%; Test: 20%
set.seed(2021)
inTrain <- createDataPartition(y, p = .80, list = FALSE)[,1]

x_train <- x[ inTrain, ]
x_test  <- x[-inTrain, ]

y_train <- y[ inTrain]
y_test  <- y[-inTrain]

# Run RFE
result_rfe1 <- rfe(x = x_train, 
                   y = y_train, 
                   sizes = c(2:10),
                   rfeControl = control)

# Print the results
result_rfe1

# Print the selected features
predictors(result_rfe1)

# Print the results visually
ggplot(data = result_rfe1, metric = "RMSE") + theme_bw()
ggplot(data = result_rfe1, metric = "Kappa") + theme_bw()

varimp_data <- data.frame(feature = row.names(varImp(result_rfe1))[1:8],
                          importance = varImp(result_rfe1)[1:8, 1])

ggplot(data = varimp_data, 
       aes(x = reorder(feature, -importance), y = importance, fill = feature)) +
  geom_bar(stat="identity") + labs(x = "Features", y = "Variable Importance") + 
  geom_text(aes(label = round(importance, 2)), vjust=1.6, color="white", size=4) + 
  theme_bw() + theme(legend.position = "none")


# Post prediction
postResample(predict(result_rfe1, x_test), y_test)

# Max model size
MODEL_SIZE <- 10

# Correlation
algorithms_df <- df_numeric_only %>% 
  regress(target ~ ., Correlation = m("cor"))

# RReliefF
algorithms_df <- algorithms_df %>% 
  bind_rows(
    df_numeric_only %>% 
      regress(target ~ ., RReliefF = m("relief"))
  )
# Information Gain
algorithms_df <- algorithms_df %>% 
  bind_rows(
    df_numeric_only %>% 
      # Split target into buckets
      mutate(target = as.factor(ntile(target, 10))) %>% 
      regress(target ~ ., 
              `Information Gain` = m("relief", estimator = "InfGain"))
  )

# Forward Selection
algorithms_df <- algorithms_df %>% 
  bind_rows(
    df_numeric_only %>% 
      regress(target ~ ., 
              `Forward Selection` = m("subset", method = "forward", nvmax = MODEL_SIZE))
  )
# Backward Elimination
algorithms_df <- algorithms_df %>% 
  bind_rows(
    df_numeric_only %>% 
      regress(target ~ ., 
              `Backward Elimination` = m("subset", method = "backward", nvmax = MODEL_SIZE))
  )
# MRMR
algorithms_df <- algorithms_df %>% 
  bind_rows(
    df_numeric_only %>% 
      regress(target ~ ., MRMR = m("mrmr", feature_count = MODEL_SIZE))
  )
# LASSO
algorithms_df <- algorithms_df %>% 
  bind_rows(
    df_numeric_only %>% 
      regress(target ~ ., 
              `LASSO` = m("lasso", pmax = MODEL_SIZE + 1),
              .cv = "rolling_origin", 
              .cv_args = list(initial = 120, assess = 24, skip = 23)
      )
  )
# BMA
algorithms_df <- algorithms_df %>% 
  bind_rows(
    df_numeric_only %>% 
      regress(target ~ ., 
              BMA = m("bma", burn = 10000, iter = 100000, 
                      mprior.size = MODEL_SIZE, mcmc = "rev.jump"))
  )

# Random Forest Importance
algorithms_df <- algorithms_df %>% 
  bind_rows(
    df_numeric_only %>% 
      regress(target ~ ., `RF Importance` = m("rf"))
  )

coef_df <- coef(algorithms_df) %>% 
  unnest(model_info)

model_df <- coef_df %>% 
  # Always remove the intercept
  filter(term != "(Intercept)") %>% 
  
  mutate(selected = case_when(
    # Extract top 10 largest scores
    model %in% c("Correlation", "RReliefF", "Information Gain") ~ 
      rank(-abs(estimate)) <= MODEL_SIZE,
    # BMA features are selected using the posterior inclusion probability
    model == "BMA" ~ rank(-pip) <= MODEL_SIZE,
    # The RF importance is stored in a separate column (%IncMSE)
    model == "RF Importance" ~ rank(-`%IncMSE`) <= MODEL_SIZE,
    # For all other methods keep all features
    TRUE ~ TRUE
  )) %>% 
  
  # Keep only included terms
  filter(selected) %>% 
  select(model, term)

model_df %>% 
  # Add 'FALSE' entries, when a feature is not selected
  mutate(selected = TRUE) %>% 
  spread(term, selected) %>% 
  gather("term", "selected", -model) %>% 
  # Plotting color
  mutate(selected = ifelse(is.na(selected), "white", "darkblue")) %>% 
  # Fix plotting order
  group_by(term) %>% 
  mutate(selected_sum = sum(selected=="darkblue")) %>% 
  ungroup %>% 
  arrange(desc(selected_sum)) %>% 
  mutate(term = factor(term, levels = unique(term))) %>% 
  mutate(model = factor(model, levels = unique(model_df$model))) %>% 
  ggplot(aes(term, model)) +
  geom_tile(aes(fill = selected)) +
  theme_bw(8, "Arial") +
  scale_fill_identity() +
  xlab(element_blank()) + ylab(element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



model_names <- unique(model_df$model)

# Retrieve selected variables
selected_vars_list <- model_names %>% 
  map(function(mod) {
    model_df %>% 
      filter(model == mod) %>% 
      pull(term)
  })
names(selected_vars_list) <- model_names

# Bootstrap resampling & regression
boot_models_df <- selected_vars_list %>% 
  map_dfr(function(selected_vars) {
    df_numeric_only %>% 
      select(all_of(c("target", selected_vars))) %>% 
      
      regress(target ~ ., 
              # Use linear regression
              m("lm"), 
              # Bootstrap settings (see ?rsample::bootstraps)
              .cv = "bootstraps", .cv_args = list(times = 100), 
              # Make sure the results for each slice are returned
              .force_cv = T, .return_slices = T)
  }, .id = "model")

# Finally, extract R2 from the model results
boot_df <- boot_models_df %>% 
  mutate(R2 = map_dbl(model_object, function(obj) summary(obj)$r.squared)) %>% 
  select(model, R2)

boot_df %>% 
  group_by(model) %>% 
  mutate(upper = mean(R2) + 2 * sd(R2) / sqrt(n()),
         lower = mean(R2) - 2 * sd(R2) / sqrt(n())) %>% 
  mutate(model = str_wrap(model, 10)) %>% 
  mutate(model = factor(model, levels = str_wrap(unique(model_df$model), 10))) %>% 
  ggplot(aes(model)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), linewidth = 0.25, width = 0.25) +
  theme_bw(8, "Arial") +
  xlab(element_blank()) + ylab("R2 statistic")




# For reproducibility
set.seed(42)
ix_tst <- sample(1:nrow(df_numeric_only), round(nrow(df_numeric_only)*0.2))

data_trn <- df_numeric_only[-ix_tst,]
data_tst <- df_numeric_only[ix_tst,]

as_tibble(iris)

fit <- data_trn %>% 
  classify(target ~ ., 
           LASSO = m("lasso"), 
           Ridge = m("ridge"), 
           ElasticNet = m("enet"), 
           AdaLASSO = m("adalasso"),
           SVM = m("svm"),
           `Random Forest` = m("rf"),
           `Least Squares` = m("ridge", lambda = 1e-5), 
           .cv = "vfold_cv")

pred <- fit %>% 
  predict(data_tst)

metrics <- pred %>% 
  group_by(model) %>% 
  mutate(row_n = row_number()) %>% 
  group_by(model, .add = TRUE) %>% 
  yardstick::rsq_trad(truth, prediction) 

metrics %>% 
  mutate(model = str_wrap(model, 11)) %>% 
  ggplot(aes(model, .estimate)) +
  geom_col(fill = "darkblue") +
  theme_bw() +
  theme(axis.title.x = element_blank())
