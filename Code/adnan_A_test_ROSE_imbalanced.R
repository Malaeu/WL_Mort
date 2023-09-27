
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

vb_df <- data_backup
vb_df <- data
vb_df <- data %>%
  mutate(across(where(is.factor), ~ as.numeric(.) - 1))

vb_df$death <- as.factor(vb_df$death)

vb_split <- initial_split(vb_df, strata = death)
vb_train <- training(vb_split)
vb_test <- testing(vb_split)

library(rpart)
treeimb <- rpart(death ~ ., data = vb_train)
pred.treeimb <- predict(treeimb, newdata = vb_test)
accuracy.meas(vb_test$death, pred.treeimb[,2])
roc.curve(vb_test$death, pred.treeimb[,2], plotit = F)

data_balanced_over <- ovun.sample(death ~ ., data = vb_train, method = "over",N = 255)$data

data.rose <- ROSE(death ~ ., data = vb_train, seed = 1)$data
ROSE.holdout <- ROSE.eval(death ~ ., data = vb_train, learner = rpart, method.assess = "holdout", extr.pred = function(obj)obj[,2], seed = 1)

ROSE.LKOCV <- ROSE.eval(death~., data=vb_train, glm, 
          method.assess="LKOCV", K=5,
          control.learner=list(family=binomial), seed=1)
