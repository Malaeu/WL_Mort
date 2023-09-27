# Load necessary libraries
library(aorsf)

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
max_time <- 20 * 60   # 14 hours  14 * 60 *60

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

f_rando <- function(x_node, y_node, w_node){
  matrix(runif(ncol(x_node)), ncol=1) 
}

f_pca <- function(x_node, y_node, w_node) { 
  
  # estimate two principal components.
  pca <- stats::prcomp(x_node, rank. = 2)
  # use the second principal component to split the node
  pca$rotation[, 2L, drop = FALSE]
  
}

# prepare parallel processing: 
#cores <- parallel::detectCores(logical = TRUE)

# Create a cluster object and then register: 
#cl <- makePSOCKcluster(cores) 
#registerDoParallel(cl)

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
  
  # Construct the formula for `orsf`
  formula_str <- paste("Surv(time, death) ~", selected_terms_str)
  
  # Fit the model using orsf with net control instead of coxph
  fit_net <- try(orsf(data_backup,
                      control = orsf_control_net(df_target = length(selected_terms)),
                      formula = as.formula(formula_str),
                      n_tree = 1000,
                      tree_seeds = sample.int(1000, size=1000, replace=TRUE)), silent=TRUE)
  
  fit_cph <- try(orsf(data_backup, 
                  control = orsf_control_cph(),
                  formula = as.formula(formula_str),
                  n_tree = 2000,
                  tree_seeds = sample.int(2000, size=2000, replace=TRUE)), silent=TRUE)
  
  fit_accel <- try(orsf(data_backup, 
                    control = orsf_control_fast(),
                    formula = as.formula(formula_str),
                    n_tree = 2000,
                    tree_seeds = sample.int(2000, size=2000, replace=TRUE)), silent=TRUE)
  
  
  
  fit_rando <- try(orsf(data_backup,
                    as.formula(formula_str),
                    control = orsf_control_custom(beta_fun = f_rando),
                    n_tree = 2000,
                    tree_seeds = sample.int(2000, size=2000, replace=TRUE)), silent=TRUE)
  
  fit_pca <- try(orsf(data_backup,
                  as.formula(formula_str),
                  control = orsf_control_custom(beta_fun = f_pca),
                  n_tree = 2000,
                  tree_seeds = sample.int(2000, size=2000, replace=TRUE)), silent=TRUE)
  
  
  # Skip this iteration if any of the models did not converge
  if (inherits(fit_net, "try-error") ||
      inherits(fit_cph, "try-error") ||
      inherits(fit_accel, "try-error") ||
      inherits(fit_rando, "try-error") ||
      inherits(fit_pca, "try-error"))
    next
  
  
  # Evaluate the model
  risk_preds <- list(
    accel = 1 - fit_accel$pred_oobag,
    cph   = 1 - fit_cph$pred_oobag,
    net   = 1 - fit_net$pred_oobag,
    rando = 1 - fit_rando$pred_oobag,
    pca   = 1 - fit_pca$pred_oobag
  )
  
  sc <- Score(object = risk_preds, 
              formula = as.formula(formula_str), 
              data = data_backup,
              summary = c("IPA"),
              times = fit_net$pred_horizon)
  
  #AUC_value <- sc$AUC$score[order(-sc$AUC)]
  AUC_value <-sc$AUC$score[, "AUC"]
  IPA_value <- sc$Brier$score[, "IPA"]
  
  best_models[[length(best_models) + 1]] <- list("model" = fit_net, "terms" = selected_terms_str, "AUC" = AUC_value, "IPA" = IPA_value)
  
  # Limit the number of stored models to 50 and AUC > some_value
  if (length(best_models) > 50) {
    # Remove models with AUC <= some_value
    auc_values <- sapply(best_models, function(x) x$AUC)
    auc_values_numeric <- as.numeric(unlist(auc_values)) # Convert the list to a numeric vector
    best_model_indices <- which(auc_values_numeric > 0.76)
    best_models <- best_models[best_model_indices]
    
    # If still more than 50 models, remove the worst ones
    if (length(best_models) > 50) {
      best_model_indices <- order(auc_values_numeric[best_model_indices], decreasing = TRUE)[1:50]
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

# Save the current state at the end of the run as before...

# Iterate through all models in best_models list to find the one with biggest AUC and IPA values
# Find the best model based on AUC
auc_values <- sapply(best_models, function(x) x$AUC)
auc_values_numeric <- as.numeric(unlist(auc_values)) # Convert the list to a numeric vector
best_model_index_AUC <- which.max(auc_values_numeric)
best_model_AUC <- max(auc_values_numeric)

# Find the best model based on IPA
ipa_values <- sapply(best_models, function(x) x$IPA)
ipa_values_numeric <- as.numeric(unlist(ipa_values)) # Convert the list to a numeric vector
best_model_index_IPA <- which.max(ipa_values_numeric)
best_model_IPA <- max(ipa_values_numeric)

# Display the best model based on AUC
cat("The best model based on AUC is at index:", best_model_index_AUC,
    "with an AUC of:", best_model_AUC, "and an IPA of:", best_models[[best_model_index_AUC]]$IPA[2,]$IPA,
    "and terms:", best_models[[best_model_index_AUC]]$terms, "\n")

# Display the best model based on IPA
cat("The best model based on IPA is at index:", best_model_index_IPA,
    "with an IPA of:", best_model_IPA, "and an AUC of:", best_models[[best_model_index_IPA]]$AUC[1,]$AUC,
    "and terms:", best_models[[best_model_index_IPA]]$terms, "\n")


# Extract the numeric AUC values from the best_models list
auc_values_numeric <- sapply(best_models, function(x) as.numeric(x$AUC))

# Sort the best_models list by AUC in descending order
sorted_best_models <- best_models[order(auc_values_numeric, decreasing = TRUE)]

# Display the AUC and IPA values for each model in the sorted list
for (i in 1:length(sorted_best_models)) {
  cat("Model", i, "with AUC:", sorted_best_models[[i]][["AUC"]]$AUC,
      "and IPA:", sorted_best_models[[i]][["IPA"]]$IPA,
      "and terms:", sorted_best_models[[i]]$terms, "\n")
}