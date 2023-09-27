remotes::install_github("ropensci/aorsf")
install.packages("remotes")
remotes::install_github("easystats/report") # You only need to do that once
remotes::install_github("pablo14/funModeling")
remotes::install_github("pablo14/genetic-algorithm-feature-selection")


library(Boruta)
library(survival)
library(report) # Load the package every time you start R
library(aorsf)
library(funModeling)

remotes::install_github("pablo14/funModeling")


final_df <- final_df_1_09_2023

freq_primary_diagnosis <- freq(final_df,"primary_diagnosis",plot = F) 
freq_primary_diagnosis[1:10,]

# Get the 10 most common levels
top_10_levels <- names(sort(table(final_df$primary_diagnosis), decreasing = TRUE))[1:10]

# Create the new column
final_df$primary_diagnosis_short <- ifelse(final_df$primary_diagnosis %in% top_10_levels, 
                                           as.character(final_df$primary_diagnosis), 
                                           'Others specify')

# Check if 'Others specify' is already a level
if ('Others specify' %in% levels(final_df$primary_diagnosis)) {
  # If it is, just convert the new column to a factor
  final_df$primary_diagnosis_short <- factor(final_df$primary_diagnosis_short)
} else {
  # If it's not, add 'Others specify' as an additional level
  final_df$primary_diagnosis_short <- factor(final_df$primary_diagnosis_short, 
                                             levels = c(levels(final_df$primary_diagnosis), 'Others specify'))
}

freq(final_df, 'primary_diagnosis_short')
status(final_df)

# Get the column names of final_df
col_names <- names(final_df)

# Separate the column names by their type
time_cols <- col_names %in% c("time", "death", "year")
factor_cols <- sapply(final_df, is.factor) & !time_cols
numeric_cols <- sapply(final_df, is.numeric) & !time_cols & !factor_cols
date_cols <- sapply(final_df, function(x) inherits(x, "POSIXct") | inherits(x, "POSIXt")) & !time_cols & !factor_cols & !numeric_cols

# Combine the column names in the desired order
new_order <- c(col_names[time_cols], col_names[factor_cols], col_names[numeric_cols], col_names[date_cols])

# Rearrange the columns of final_df
final_df <- final_df[, new_order]




variable_boruta <- names(final_df[,c(4:23,25:26,34:48,57:81)])

all_models <- list()

# Determine the minimum and maximum values for time_of_ct
min_time <- min(final_df$time_of_ct)
max_time <- max(final_df$time_of_ct)

df_selected_vars_borut <- final_df %>% 
  filter(between(time_of_ct, -90, 60)) %>%
  select(all_of(variable_boruta),death)

df_selected_vars_cox <- final_df %>% 
  filter(between(time_of_ct, -90, 60)) %>%
  select(all_of(variable_boruta),time,death)



for(i in seq(-90, 60)){
  # Filter data based on current time_of_ct value
  
  # Run Boruta feature selection
  boruta.bank_train <- Boruta(death~., data = df_selected_vars_borut, doTrace = 0)
  
  # Get significant features from Boruta
  significant_features <- getSelectedAttributes(boruta.bank_train, withTentative = TRUE)
  
  # Fit Cox model using only significant features from Boruta
  cox_model <- coxph(as.formula(paste("Surv(time, death) ~", paste(significant_features, collapse = " + "))), data = df_selected_vars_cox)
  
  # Check if all variables are significant in the Cox model.
  summary_cox <- summary(cox_model)
  
  p_values <- summary_cox$coefficients[,5] 
  
  if(all(p_values < .05)){
    print(paste("All variables are significant for time_of_ct =", i))
  } else {
    print(paste("Not all variables are significant for time_of_ct =", i))
  }
  
  # Add the model and its associated information to the list of all models.
  all_models[[paste("model_time_of_ct", i)]] <- list(
    "Model" = cox_model,
    "Summary" = summary_cox,
    "Significant Features" = significant_features,
    "P-values" = p_values
  )
}

aic_values <- sapply(all_models, function(x) {extractAIC(x$Model)[2]})
ordered_models_by_aic <- all_models[order(aic_values)]

# Initialize an empty data frame
results_df <- data.frame()

# Loop through ordered_models_by_aic
for(i in seq_along(ordered_models_by_aic)){
  
  # Extract information for current model
  current_model_info <- ordered_models_by_aic[[i]]
  
  # Check if Significant_Features is not NULL and not empty
  if(!is.null(current_model_info$Significant_Features) && length(current_model_info$Significant_Features) > 0){
    sig_features <- paste(current_model_info$Significant_Features, collapse = ", ")
  } else {
    sig_features <- NA
  }
  
  # Check if P_values is not NULL and not empty
  if(!is.null(current_model_info$P_values) && length(current_model_info$P_values) > 0){
    p_values <- paste(current_model_info$P_values, collapse = ", ")
  } else {
    p_values <- NA
  }
  
  # Create a one-row data frame with this information
  row_df <- data.frame(
    Model = i,
    Significant_Features = sig_features,
    P_values = p_values,
    Log_Likelihood = current_model_info$Summary$loglik[2],   # log-likelihood at convergence
    AIC = current_model_info$Summary$aic,                    # Akaike Information Criterion
    Num_Df = current_model_info$Summary$numdf                # number of degrees of freedom for terms in model
  )
  
  # Add this row to results_df
  results_df <- rbind(results_df, row_df)
}

# Sie können eine Schleife verwenden, um nur die Namen anzuzeigen:
for (model in ordered_models_by_aic) {
  print(model$name)
}


