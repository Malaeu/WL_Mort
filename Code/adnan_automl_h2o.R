auto_ml() %>%  
  set_engine("h2o") %>% 
  set_mode("classification") %>% 
  translate()




splits <- h2o.splitFrame(data = adnan.hex,
                         ratios = c(0.8),  #partition data into 80% and 20% chunks
                         seed = 198)

train <- splits[[1]]
test <- splits[[2]]

# Identify predictors and response
y <- "death"
x <- setdiff(names(train), y)


# For binary classification, response should be a factor
train[, y] <- as.factor(train[, y])
test[, y] <- as.factor(test[, y])

# Run AutoML for 20 base models
aml <- h2o.automl(x = x, y = y,
                  training_frame = train,
                  max_models = 200,
                  seed = 1)

# View the AutoML Leaderboard
lb <- aml@leaderboard
print(lb, n = nrow(lb))  # Print all rows instead of default (6 rows)


# To generate predictions on a test set, you can make predictions
# directly on the `H2OAutoML` object or on the leader model
# object directly
pred <- h2o.predict(aml, test)  # predict(aml, test) also works
# Get leaderboard with all possible columns
lb <- h2o.get_leaderboard(object = aml, extra_columns = "ALL")
lb
# Explain an AutoML object
exa <- h2o.explain(aml, test)
exa

