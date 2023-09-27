# Load purrr package for looping: 
library(purrr)

df_status(df_selected_vars_borut)
df_selected_vars_borut <- df_selected_vars_borut %>% 
  mutate(death = recode(death, `1` = 0, `2` = 1))
# Assuming df_log is your data frame
df_log[] <- lapply(df_log, function(x) if(is.character(x)) as.numeric(as.character(x)) else x)

# All potential variables can be used for modelling Logistic Regression:  
#variables <- df_selected_vars_borut %>% select(-death) %>% names()
variables <- selected_feature_names


# Split our data: 
df_train <- df_selected_vars_borut %>% 
  group_by(death) %>% 
  sample_frac(0.8) %>% 
  ungroup() # Use 80% data set for training model. 

df_test <- dplyr::setdiff(df_selected_vars_borut, df_train) # Use 50% data set for validation. 



# Function lists all combinations of variables: 

all_combinations <- function(your_predictors) {
  
  n <- length(your_predictors)
  map(1:n, function(x) {combn(variables, x)}) %>%  
    map(as.data.frame) -> k
  
  all_vec <- c()
  
  for (i in 1:n) {
    df <- k[[i]]
    n_col <- ncol(df)
    
    for (j in 1:n_col) {
      my_vec <- df[, j] %>% as.character() %>% list()
      all_vec <- c(all_vec, my_vec)
    }
  }
  
  return(all_vec)
  
}

all_combinations_opt <- function(your_predictors) {
  n <- length(your_predictors)
  combinations_list <- purrr::map(1:n, function(x) combn(your_predictors, x, simplify = FALSE))
  all_vec <- unlist(combinations_list, recursive = FALSE)
  return(all_vec)
}

# AICs for all models. Note that there will be 4095 Logistic Models thus 
# training all models may be a time-consuming process: 

# prepare parallel processing: 
cores <- parallel::detectCores(logical = TRUE)


# Create a cluster object and then register: 
cl <- makePSOCKcluster(cores) 
registerDoParallel(cl)

system.time(
  all_combinations_opt(variables) %>% 
    map(function(x) {as.formula(paste("death ~", paste(x, collapse = " + ")))}) %>% 
    map(function(formular) {glm(formular, family = binomial, data = df_train)}) %>% 
    map_dbl("aic") -> aic_values
)

stopCluster(cl)



#  The conbination of Predictors that results min AIC: 

all_combination_of_vars <- variables %>% all_combinations_opt()
predictors_selected <- all_combination_of_vars[[which.min(aic_values)]] 

# Thus there are 10 predictors selected based on AIC criterion:  
predictors_selected
x <- 10
best_models <- get_best_models(all_combination_of_vars, aic_values, x,df_train,df_test)

best_models



# Data frame for training Logistic Regression: 
df_train_aic <- df_train %>% select(all_of(predictors_selected), "death")

#-------------------------------------------------------------------------------
#  Develop a scorecard Model for variables based on the results from WOE and IV 
#-------------------------------------------------------------------------------
library(scorecard)

# Generates optimal binning for numerical, factor and categorical variables: 
bins_var <- woebin(df_train_aic, y = "death", no_cores = 24, positive = "death|0")

# Creates a data frame of binned variables for Logistic Regression: 
df_train_woe <- woebin_ply(df_train_aic, bins_var)

# Logistic Regression:
my_logistic <- glm(death ~ ., family = binomial, data = df_train_woe)

# Show results: 
my_logistic %>% summary()



# Calculate scorecard scores for variables based on the results from woebin and glm: 
my_card <- scorecard(bins_var, my_logistic, points0 = 600, odds0 = 1/19, pdo = 50)
my_card <- lapply(my_card, as.data.frame)
# Show Results: 

library(dplyr)

my_card <- lapply(my_card, as.data.frame)
do.call("bind_rows", my_card) %>% 
  filter(row_number() != 1) %>% 
  select(-breaks, -is_special_values, -count, -count_distr, -neg, -pos, -posprob) %>% 
  mutate_if(is.numeric, function(x) {round(x, 3)}) %>% 
  mutate(bin = bin %>% 
           str_replace_all("\\[", "From ") %>% 
           str_replace_all("\\,", " to ") %>% 
           str_replace_all("\\)", "")) -> iv_for_predictors_point

 iv_for_predictors_point %>% 
  knitr::kable(col.names = c("Predictor", "Group", "WOE", "Scorecard", "Bin IV", "Total IV"))

 print.table(iv_for_predictors_point)
 View(iv_for_predictors_point)


# Information Values for predictors: 

iv_for_predictors_point %>% 
  group_by(variable) %>% 
  summarise(iv_var = mean(total_iv)) %>% 
  ungroup() %>% 
  arrange(iv_var) %>% 
  mutate(variable = factor(variable, levels = variable)) -> iv_values


theme_set(theme_minimal())
iv_values %>% 
  ggplot(aes(variable, iv_var)) + 
  geom_col(fill = "#377eb8") + 
  coord_flip() + 
  geom_col(data = iv_values %>% filter(iv_var < 0.1), aes(variable, iv_var), fill = "grey60") + 
  geom_text(data = iv_values %>% filter(iv_var < 0.1), aes(label = round(iv_var, 3)), 
            hjust = -0.1, size = 5, color = "grey40") + 
  geom_text(data = iv_values %>% filter(iv_var >= 0.1), aes(label = round(iv_var, 3)), 
            hjust = -.1, size = 5, color = "#377eb8") + 
  labs(title = "Figure 1: Information Value (IV) for Variables", 
       x = NULL, y = "Information Value (IV)") + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2)) + 
  theme(panel.grid.major.y = element_blank()) + 
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))

```

```{r}


# Scorecard point for all observations from train data set: 
my_points_train <- scorecard_ply(df_train_aic, my_card, only_total_score = FALSE, print_step = 0) %>% as.data.frame()

# Some statistics scorecard by group: 
df_scored_train <- df_train_aic %>% 
  mutate(SCORE = my_points_train$score) %>% 
  mutate(BAD = case_when(BAD == 1 ~ "Default", TRUE ~ "NonDefault")) 

df_scored_train %>% 
  group_by(BAD) %>% 
  summarise_each(funs(min, max, median, mean, n()), SCORE) %>% 
  mutate_if(is.numeric, function(x) {round(x, 0)}) %>% 
  knitr::kable(caption = "Table 1: Scorecad Points by Group for Train Data")

```

```{r}


df_scored_train %>% 
  group_by(BAD) %>% 
  summarise(tb = mean(SCORE)) %>% 
  ungroup() -> mean_score_train

df_scored_train %>% 
  ggplot(aes(SCORE, color = BAD, fill = BAD)) + 
  geom_density(alpha = 0.3) + 
  geom_vline(aes(xintercept = mean_score_train$tb[1]), linetype = "dashed", color = "red") + 
  geom_vline(aes(xintercept = mean_score_train$tb[2]), linetype = "dashed", color = "blue") + 
  geom_text(aes(x = 400 - 15, y = 0.0042, label = mean_score_train$tb[1] %>% round(0)), color = "red", size = 4) + 
  geom_text(aes(x = 565, y = 0.0042, label = mean_score_train$tb[2] %>% round(0)), color = "blue", size = 4) + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = c(0.2, 0.8)) + 
  labs(x = NULL, y = NULL, title = "Figure 2: Scorecard Distribution by two Credit Groups for Train Data", 
       subtitle = "The scorecard point is a numeric expression measuring creditworthiness. Commercial Banks\nusually utilize it as a method to support the decision-making about credit applications.")

```

```{r}


# Scorecard Points for test data set: 

df_test_aic <- df_test %>% select(predictors_selected, "BAD")

my_points_test <- scorecard_ply(df_test_aic, my_card, print_step = 0, 
                                only_total_score = FALSE) %>% as.data.frame()


df_scored_test <- df_test_aic %>% 
  mutate(SCORE = my_points_test$score) %>% 
  mutate(BAD = case_when(BAD == 1 ~ "Default", TRUE ~ "NonDefault")) 

df_scored_test %>% 
  group_by(BAD) %>% 
  summarise_each(funs(min, max, median, mean, n()), SCORE) %>% 
  mutate_if(is.numeric, function(x) {round(x, 0)}) %>% 
  knitr::kable(caption = "Table 2: Scorecad Points by Group for Test Data")


```

```{r}



df_scored_test %>% 
  group_by(BAD) %>% 
  summarise(tb = mean(SCORE)) %>% 
  ungroup() -> mean_score_test

df_scored_test %>% 
  ggplot(aes(SCORE, color = BAD, fill = BAD)) + 
  geom_density(alpha = 0.3) + 
  geom_vline(aes(xintercept = mean_score_test$tb[1]), linetype = "dashed", color = "red") + 
  geom_vline(aes(xintercept = mean_score_test$tb[2]), linetype = "dashed", color = "blue") + 
  geom_text(aes(x = 412, y = 0.0042, label = mean_score_test$tb[1] %>% round(0)), color = "red", size = 4) + 
  geom_text(aes(x = 570, y = 0.0042, label = mean_score_test$tb[2] %>% round(0)), color = "blue", size = 4) + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = c(0.2, 0.8)) + 
  labs(x = NULL, y = NULL, title = "Figure 3: Scorecard Distribution by two Credit Groups for Test Data", 
       subtitle = "The scorecard point is a numeric expression measuring creditworthiness. Commercial Banks\nusually utilize it as a method to support the decision-making about credit applications.")
```

### Some Criteria for Model Evaluation in Context of Cresit Scoring
