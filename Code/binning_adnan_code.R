# Generates optimal binning for numerical, factor and categorical variables: 
bins_var <- woebin(df_train_aic, y = "death", no_cores = 24, positive = "death|0")

# Creates a data frame of binned variables for Logistic Regression: 
df_train_woe <- woebin_ply(df_train_aic, bins_var)

# Logistic Regression:
my_logistic <- glm(death ~ ., family = binomial, data = df_train_woe)

# # Show results: 
# my_logistic %>% summary()

# Parameter grid
points_range <- seq(20, 900, by = 5)
odds_range <- seq(1,30, by = 1)
pdo_range <- seq(10,100, by = 2)

# prepare parallel processing: 
cores <- parallel::detectCores(logical = TRUE)


# Create a cluster object and then register: 
cl <- makePSOCKcluster(cores) 
registerDoParallel(cl)

# Generate scorecard for each parameter set
model_results <- expand.grid(points0 = points_range, odds0 = odds_range, pdo = pdo_range) %>% 
  rowwise() %>% 
  mutate(scorecard = list(scorecard(bins = bins_var, model = my_logistic, points0 = points0, odds0 = odds0, pdo = pdo))) %>% 
  ungroup()

stopCluster(cl)
summary(model_results)

# Data frame for training Logistic Regression: 
df_test_aic <- df_test %>% select(all_of(predictors_selected), "death")

#-------------------------------------------------------------------------------
#  Develop a scorecard Model for variables based on the results from WOE and IV 
#-------------------------------------------------------------------------------
library(scorecard)

# Generates optimal binning for numerical, factor and categorical variables: 
bins_var <- woebin(df_test_aic, y = "death", no_cores = 24, positive = "death|0")

# Creates a data frame of binned variables for Logistic Regression: 
df_test_woe <- woebin_ply(df_test_aic, bins_var)

# Entfernen Sie das "_woe" Suffix von den Spaltennamen in df_test_woe
names(df_test_woe) <- gsub("_woe", "", names(df_test_woe))

# apply each scorecard to the test data
model_results <- model_results %>% 
  rowwise() %>%
  mutate(my_points_test = list(scorecard_ply(df_test_woe, scorecard_obj, only_total_score = FALSE, print_step = 0) %>% as.data.frame()))



