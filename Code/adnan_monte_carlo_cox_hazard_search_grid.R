save.image("data_25_09_23.RData")


library(survival)


df_status(data_backup)

# Load required library
library(dplyr)

# Load the data
data_backup <- readRDS("data_backup_20_09_2023.rds")

# Remove the 'patient_number_cut' column
#data_backup <- data_backup |> select(-patient_number_cut)

# Select all numeric variables
numerical_vars <- data_backup %>% 
  select_if(is.numeric) %>%
  names()

# Exclude variables that you don't want to normalize
exclude_vars <- c("time", "death")
numerical_vars_to_normalize <- setdiff(numerical_vars, exclude_vars)

# Find columns that have "_normalized" in their name
normalized_vars <- grep("_normalized$", names(data_backup), value = TRUE)

# Combine both lists of variables to exclude from normalization
all_exclude_vars <- c(exclude_vars, normalized_vars)

# Extract only the variables that should be normalized
numerical_vars_to_normalize <- setdiff(numerical_vars_to_normalize, all_exclude_vars)

# Function to normalize using Min-Max scaling
normalize_min_max <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Normalize selected numerical variables
for (var in numerical_vars_to_normalize) {
  data_backup[[var]] <- normalize_min_max(data_backup[[var]])
}

# Create the final_data by updating only the normalized columns in data_backup
final_data <- data_backup

# Check the number of variables
print(paste("Number of variables in final_data: ", length(names(final_data))))

# View the first few rows of the final dataset
head(final_data)

# Save the final_data if needed
# saveRDS(final_data, "final_data.rds")




fit <- coxph(Surv(time, death) ~ hu_listung + sodium_listing_cut + muscle_cut + eat_cut + pat_cut + muscle_to_paracardial_fat_index_normalized_cut + age_by_listung + bmi + bone_muscle_index + height + sodium_listing + bone + eat , data = data_backup)


# Extract the coefficients, standard errors, and p-values
coefficients <- coef(fit)
standard_errors <- sqrt(diag(vcov(fit)))
p_values <- summary(fit)$coefficients[, "Pr(>|z|)"]

# Calculate the 95% confidence intervals
lower_95_ci <- coefficients - 1.96 * standard_errors
upper_95_ci <- coefficients + 1.96 * standard_errors

# Create a DataFrame to display the results
results <- data.frame(Coefficient = coefficients, Standard_Error = standard_errors, p_value = p_values, Lower_95_CI = lower_95_ci, Upper_95_CI = upper_95_ci)
results


# Number of main effects
main_effects <- 7

# Number of 2-way interactions
two_way_interactions <- choose(main_effects, 2)

# Total number of terms including main effects and 2-way interactions
total_terms <- main_effects + two_way_interactions

# Total number of all possible combinations (excluding the empty set)
total_combinations <- 2^total_terms - 1

list(total_combinations = total_combinations, two_way_interactions = two_way_interactions, total_terms = total_terms)



# Load necessary libraries
library(survival)

# Initialize or load the checked combinations
checked_combinations_filepath <- "checked_combinations.rds"

if (file.exists(checked_combinations_filepath)) {
  checked_combinations <- readRDS(checked_combinations_filepath)
} else {
  checked_combinations <- data.frame(terms = character(), stringsAsFactors = FALSE)
}

# Initialize or load the best_models list
best_models_filepath <- "best_models.rds"

if (file.exists(best_models_filepath)) {
  best_models <- readRDS(best_models_filepath)
} else {
  best_models <- list()
}

# Current time
start_time <- Sys.time()

# Max working time in seconds
max_time <- 14 * 60 * 60  # 15 hours

# Time at which we last saved
last_save_time <- start_time
# List of main effects
main_effects <- c("hu_listung", "sodium_listing_cut", "muscle_cut", "eat_cut", "pat_cut", 
                  "muscle_to_paracardial_fat_index_normalized_cut", "age_by_listung", "bmi", 
                  "bone_muscle_index", "height", "sodium_listing", "bone", "eat")

# Generate all 2-way interactions
two_way_interactions <- combn(main_effects, 2, simplify = FALSE)
two_way_interactions <- sapply(two_way_interactions, function(x) paste(x, collapse = ":"))

# Combine main effects and 2-way interactions
all_terms <- c(main_effects, two_way_interactions)

# prepare parallel processing: 
cores <- parallel::detectCores(logical = TRUE)

# Create a cluster object and then register: 
cl <- makePSOCKcluster(cores) 
registerDoParallel(cl)


# Main loop for Monte Carlo resampling
while (difftime(Sys.time(), start_time, units = "secs") < max_time) {
  
  # Randomly select terms (main effects and interactions)
  selected_terms <- sample(all_terms, size = sample(1:length(all_terms), 1), replace = FALSE)
  
  # Ensure main effects are included for any selected interaction terms
  for (term in selected_terms) {
    if (grepl(":", term)) {
      main_effects_for_term <- unlist(strsplit(term, ":"))
      selected_terms <- unique(c(selected_terms, main_effects_for_term))
    }
  }
  
  selected_terms_str <- paste(sort(selected_terms), collapse = " + ")
  
  # Skip this iteration if the combination was already checked
  if (selected_terms_str %in% checked_combinations$terms) next
  
  # Construct the formula for `coxph`
  formula_str <- paste("Surv(time, death) ~", selected_terms_str)
  
  # Fit the model
  fit <- try(coxph(as.formula(formula_str), data = new_data), silent = TRUE)
  
  # Skip this iteration if the model did not converge
  if (inherits(fit, "try-error")) next
  
  # Evaluate the model
  p_values <- summary(fit)$coefficients[, 5]
  valid_CI <- all(summary(fit)$conf.int[, 1] > 0 & summary(fit)$conf.int[, 2] < Inf)
  
  # Check for NA or NaN in p_values and confidence intervals
  if (any(is.na(p_values) | is.nan(p_values) | is.na(valid_CI) | is.nan(valid_CI))) next
  
  if (sum(p_values < 0.05) >= (length(p_values) / 2) && valid_CI) {
    aic_value <- AIC(fit)
    best_models[[length(best_models) + 1]] <- list("model" = fit, "terms" = selected_terms, "AIC" = aic_value)
  }
  
  
  # Limit the number of stored models to 50 and AIC < 191
  if (length(best_models) > 50) {
    # Remove models with AIC >= 191
    best_models <- best_models[sapply(best_models, function(x) AIC(x$model) < 172)]
    
    # If still more than 50 models, remove the worst ones
    if (length(best_models) > 50) {
      aic_values <- sapply(best_models, function(x) AIC(x$model))
      best_model_indices <- order(aic_values)[1:50]
      best_models <- best_models[best_model_indices]
    }
  }
  # Add the selected terms to the checked_combinations dataframe
  checked_combinations <- rbind(checked_combinations, data.frame(terms = selected_terms_str, stringsAsFactors = FALSE))
  
  # Save if 10 minutes have passed since the last save
  if (difftime(Sys.time(), last_save_time, units = "mins") >= 10) {
    saveRDS(checked_combinations, checked_combinations_filepath)
    saveRDS(best_models, best_models_filepath)
    last_save_time <- Sys.time()
  }
}
  
stopCluster(cl)

#unregister parallelcomuting
unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list = ls(name = env), pos = env)
}

# Save the current state at the end of the run, regardless of when we last saved
saveRDS(checked_combinations, checked_combinations_filepath)
saveRDS(best_models, best_models_filepath)
best_models


# Initialize variables to keep track of the smallest AIC and corresponding model index
smallest_AIC <- Inf  # Set to infinity initially so that any AIC value will be smaller
best_model_index <- NA  # Not available initially

# Iterate through the list of models to find the one with the smallest AIC
for (i in 1:length(best_models)) {
  current_AIC <- best_models[[i]][["AIC"]]
  if (current_AIC < smallest_AIC) {
    smallest_AIC <- current_AIC
    best_model_index <- i
  }
}

# Output the index of the model with the smallest AIC and its AIC value
cat("The best model is at index:", best_model_index, "with an AIC of:", smallest_AIC)

