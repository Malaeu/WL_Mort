# Load required packages
library(earth)
library(relaimpo)
library(party)
library(rpart)
library(FSelector)
library(varSelRF)
library(VSURF)
library(RRF)
library(evtree)
library(gbm)
library(randomForest)
library(earth)
library(relaimpo)
library(party)
library(rpart)
library(FSelector)
library(varSelRF)
library(VSURF)
library(RRF)
library(evtree)
library(gbm)
library(Boruta)
library(caret)
library(ranger)
library(vip)
library(funModeling)
library(dplyr)



# Remove "time" variable from the dataset
data <- data %>% select(-time)
# Move the columns "time", "death", and "year" to the front
data <- data %>% relocate(c("death"))
data <- data %>% relocate(c("bone"),.after = "weight")


# Convert factor variables to numeric
data <- data %>%
  mutate(across(where(is.factor), as.numeric))

# Convert target variable into a factor
data[, "death"] <- as.factor(data[, "death"])


# Calculate variable importance using the mRMRe method
mrmre_data <- mRMR.data(data = data)
mrmre_filter <- mRMR.classic("mRMRe.Filter", data = mrmre_data, target_indices = 1, feature_count = ncol(data) - 1)
varimp_mrmre <- data.frame(Importance = mrmre_filter@mim)

# Calculate variable importance using the xgboost method
fit_xgboost <- xgboost(data = as.matrix(data[, -1]), label = as.matrix(data[, 1]), nrounds = 100, objective = "binary:logistic")
varimp_xgboost <- xgb.importance(model = fit_xgboost)

# Calculate variable importance using the aorsf method
fit_aorsf <- aorsf(death ~ ., data = data)
varimp_aorsf <- fit_aorsf$variable.importance

# Calculate variable importance using the glmnet method
fit_glmnet <- glmnet(as.matrix(data[, -1]), as.vector(data[, 1]), family = "binomial")
varimp_glmnet <- fit_glmnet$beta

# Calculate variable importance using the Boruta method
boruta_output <- Boruta(death ~ ., data = data, doTrace = 0)
boruta_importance <- data.frame(Importance = boruta_output$ImpHistory)


# Calculate variable importance using the caret, ranger, vip, and funModeling methods
fit_caret <- train(death ~ ., data = data, method = "ranger", importance = "impurity")
varimp_caret <- varImp(fit_caret)

fit_ranger <- ranger(death ~ ., data = data, importance = "impurity")
varimp_ranger <- data.frame(Importance = fit_ranger$variable.importance)

fit_vip <- vip::vip(fit_caret, num_features = ncol(data) - 1)
varimp_vip <- data.frame(Importance = fit_vip$importance)

fit_var_rank_info <- var_rank_info(data, target = "death")
varimp_var_rank_info <- data.frame(Importance = fit_var_rank_info$gr)


# Calculate variable importance using the earth method
fit_earth <- earth(death ~ ., data = data)
varimp_earth <- evimp(fit_earth)

# Calculate variable importance using the relaimpo method
fit_relaimpo <- calc.relimp(death ~ ., data = data, type = c("lmg"))
varimp_relaimpo <- fit_relaimpo$lmg

# Calculate variable importance using the party method
fit_party <- cforest(death ~ ., data = data)
varimp_party <- varimp(fit_party)

# Calculate variable importance using the rpart method
fit_rpart <- rpart(death ~ ., data = data)
varimp_rpart <- fit_rpart$variable.importance

# Calculate variable importance using the FSelector method
fit_FSelector <- information.gain(death ~ ., data = data)
varimp_FSelector <- fit_FSelector$attr_importance

# Calculate variable importance using the varSelRF method
fit_varSelRF <- varSelRF(death ~ ., data = data)
varimp_varSelRF <- fit_varSelRF$importance

# Calculate variable importance using the VSURF method
fit_VSURF <- VSURF(death ~ ., data = data)
varimp_VSURF <- fit_VSURF$varselect.threshold

# Calculate variable importance using the RRF method
fit_RRF <- RRF(death ~ ., data = data)
varimp_RRF <- fit_RRF$importance

# Calculate variable importance using the evtree method
fit_evtree <- evtree(death ~ ., data = data)
varimp_evtree <- fit_evtree$variable.importance

# Calculate variable importance using the gbm method
fit_gbm <- gbm(death ~ ., data = data, distribution = "bernoulli")
varimp_gbm <- summary(fit_gbm)

# Extract the top 10 variables from each method
top10_boruta <- rownames(head(boruta_importance, 10))
top10_caret <- rownames(head(varimp_caret, 10))
top10_ranger <- rownames(head(varimp_ranger, 10))
top10_vip <- rownames(head(varimp_vip, 10))
top10_var_rank_info <- rownames(head(varimp_var_rank_info, 10))

# Find the intersection of the top 10 variables
intersection <- Reduce(intersect, list(top10_boruta, top10_caret, top10_ranger, top10_vip, top10_var_rank_info))


# Extract the top 10 variables from each method
top10_earth <- rownames(head(varimp_earth, 10))
top10_relaimpo <- rownames(head(varimp_relaimpo, 10))
top10_party <- rownames(head(varimp_party, 10))
top10_rpart <- rownames(head(varimp_rpart, 10))
top10_FSelector <- rownames(head(varimp_FSelector, 10))
top10_varSelRF <- rownames(head(varimp_varSelRF, 10))
top10_VSURF <- rownames(head(varimp_VSURF, 10))
top10_RRF <- rownames(head(varimp_RRF, 10))
top10_evtree <- rownames(head(varimp_evtree, 10))
top10_gbm <- rownames(head(varimp_gbm, 10))

# Find the intersection of the top 10 variables
intersection <- Reduce(intersect, list(top10_boruta, top10_caret, top10_ranger, top10_vip, top10_var_rank_info,
                                       top10_earth, top10_relaimpo, top10_party, top10_rpart, top10_FSelector, 
                                       top10_varSelRF, top10_VSURF, top10_RRF, top10_evtree, top10_gbm))

# Present the results in a table
result_table <- data.frame(
  Method = rep(c("Boruta", "Caret", "Ranger", "Vip", "VarRankInfo", "Earth", "Relaimpo", "Party", "Rpart", "FSelector", "VarSelRF", "VSURF", "RRF", "Evtree", "Gbm"), each = 10),
  Top10 = c(top10_boruta, top10_caret, top10_ranger, top10_vip, top10_var_rank_info, top10_earth, top10_relaimpo, top10_party, top10_rpart, top10_FSelector, top10_varSelRF, top10_VSURF, top10_RRF, top10_evtree, top10_gbm),
  Intersection = c(rep(intersection, length.out = 150))
)
result_table

result_table <- data.frame(
  Method = rep(c("Boruta", "Caret", "Ranger", "Vip", "VarRankInfo"), each = 10),
  Top10 = c(top10_boruta, top10_caret, top10_ranger, top10_vip, top10_var_rank_info),
  Intersection = c(rep(intersection, length.out = 10))
)
result_table
