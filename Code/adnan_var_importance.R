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
library(Boruta)
library(caret)
library(ranger)
library(vip) 
library(conflicted)
# Add missing libraries
# Assuming 'aorsf' function comes from randomForestSRC package
# Please replace it with the correct package if my assumption is wrong.
# Also, please install these packages before loading them.
install.packages(c("mRMRe", "xgboost", "randomForestSRC", "funModeling"))
require(mRMRe) 
require(xgboost) 
require(randomForestSRC) 
require(funModeling)
# install.packages("devtools")
devtools::install_github("r-lib/conflicted")

data_backup <- data
data <- data %>% select(-time)

data <- data %>%
  mutate(across(where(is.factor), as.numeric))

data[, "death"] <- as.factor(data[, "death"])

mrmre_data <- mRMR.data(data = data)

fit_xgboost <- xgboost(data = as.matrix(data[, -1]), label = as.matrix(data[, 1]), nrounds = 100, objective = "binary:logistic")
varimp_xgboost <- xgb.importance(model = fit_xgboost)

fit_aorsf <- aorsf(death ~ ., data = data)

fit_glmnet <- glmnet(as.matrix(data[, -1]), as.vector(data[, 1]), family = "binomial")

boruta_output <- Boruta(death ~ ., data = data, doTrace = 0)

fit_caret <- train(death ~ ., data = data, method = "ranger", importance = "impurity")

fit_ranger <- ranger(death ~ ., data = data, importance = 'impurity')

fit_vip <- vip::vip(fit_caret, num_features=ncol(data)-1)

fit_var_rank_info<- var_rank_info(data,target='death')

fit_earth<- earth(death~.,data=data)

fit_relaimpo<- calc.relimp(death~.,data=data,type=c('lmg'))

fit_party<- cforest(death~.,data=data)

fit_rpart<- rpart(death~.,data=data)

fit_FSelector<- information.gain(death~.,data=data)

fit_varSelRF<- varSelRF(death~.,data=data)

fit_VSURF<- VSURF(death~.,data=data)

fit_RRF<- RRF(death~.,data=data)

fit_evtree<- evtree(death~.,data=data)


methods_list=list(boruta=boruta_output,caret= fit_caret,ranger= fit_ranger,vip= fit_vip,var_rank_info= fit_var_rank_info,
                  earth= fit_earth ,relaimpo= fit_relaimpo ,party= fit_party ,rpart= fit_rpart ,
                  FSelector= fit_FSelector ,varSelRF= fit_varSelRF ,VSURF= fit_VSURF ,
                  RRF= fit_RRF,evtree= fit_evtree )

top10_list=lapply(methods_list,function(x){
  imp=NULL
  if(class(x)[1]=="Boruta") imp=getImpHistory(x)[,"meanImp"]
  else if(class(x)[1]=="train") imp=vip::importance(x)$Importance
  else if(class(x)[1]=="ranger") imp=x$variable.importance
  else if(class(x)[1]=="evtree") imp=x$variable.importance
  else if(class(x)[1]=="glmnet" || class(x)[1]=="cv.glmnet") imp=x$beta@i # assuming glmnet or cv.glmnet object here. Please replace it with the correct one.
  
  names(sort(imp[!is.na(imp)],decreasing=T))[1:10]
})

intersection_names=top10_list[[1]]
for(i in 2:length(top10_list)){
  intersection_names=c(intersection_names,top10_list[[i]])
}

result_table=NULL

for(i in 1:length(top10_list)){
  result_table=rbind(result_table,data.frame(Method=i,
                                             Top10=top10_list[[i]],
                                             Intersection=(top10_list[[i]] %in% intersection_names)))
}

names(result_table)=c("Method","Top10","Intersection")
result_table$Method=sapply(result_table$Method,function(i){names(top10_list[i])})
result_table

