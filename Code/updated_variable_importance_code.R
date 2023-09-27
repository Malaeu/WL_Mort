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
df_status(data)

# Convert factor variables to numeric
data <- data %>%
  mutate(across(where(is.factor), as.numeric))

# Convert target variable into a factor
data[, "death"] <- as.factor(data[, "death"])

# Calculate variable importance using the Boruta method
boruta_output <- Boruta(death ~ ., data = data, doTrace = 0)
boruta_importance <- data.frame(Importance = boruta_output$ImpHistory)


# Calculate variable importance using the caret "death" should be factor, ranger, vip, and funModeling methods
cl <- as.factor(data[, "death"])
fit_caret <- train(as.factor(data[, "death"]) ~ ., data = data, method = "ranger", importance = "impurity")
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

# Calculate variable importance using the party method no objects of class ‘POSIXct’‘POSIXt’
data_cforest <- data[,-64]
fit_party <- cforest(death ~ ., data = data_cforest)
varimp_party <- varimp(fit_party)

# Calculate variable importance using the rpart method
fit_rpart <- rpart(death ~ ., data = data)
varimp_rpart <- fit_rpart$variable.importance

# Calculate variable importance using the FSelector method
fit_FSelector <- information.gain(death ~ ., data = data)
varimp_FSelector <- fit_FSelector$initialOrderedImportances

# Calculate variable importance using the varSelRF method
cl <- as.factor(data[, "death"])

fit_varSelRF <- varSelRF(data,cl)
varimp_varSelRF <- fit_varSelRF$importance

# Calculate variable importance using the VSURF method

# Convert target variable into a factor
cl <- as.factor(data[, "death"])

fit_VSURF <- VSURF(data,cl)
varimp_VSURF <- fit_VSURF$varselect.pred

# Calculate variable importance using the RRF method
rrf_data <- data[,-1]
cl <- as.factor(data[, "death"])
fit_RRF <- RRF(rrf_data, cl)
varimp_RRF <- fit_RRF$importance

# Calculate variable importance using the evtree method
fit_evtree <- evtree(death ~ ., data = data)
varimp_evtree <- fit_evtree$variable.importance

# Calculate variable importance using the gbm method
data_gbm <- data[,-64]
fit_gbm <- gbm(death ~ ., data = data_gbm, distribution = "bernoulli")
varimp_gbm <- summary(fit_gbm)
varimp_gbm
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
