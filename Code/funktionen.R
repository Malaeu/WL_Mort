
# Funktion count_key_matches akzeptiert drei Argumente: df ist der DataFrame,
# der die Schlüsselspalte enthält, key_column ist der Name der Schlüsselspalte
# und df_to_match ist der DataFrame, den Sie auf Übereinstimmungen überprüfen
# möchten.


count_key_matches <- function(df, key_column, df_to_match) {
  # Eindeutige Werte in der Schlüsselspalte von df
  unique_values <- unique(df[[key_column]])
  
  # Funktion zum Zählen der Übereinstimmungen in einer Spalte
  count_matches <- function(column) {
    sum(column %in% unique_values)
  }
  
  # Anwendung der Funktion auf jede Spalte von df_to_match
  matches <- sapply(df_to_match, count_matches)
  
  # Gibt den Namen der Spalte mit den meisten Übereinstimmungen zurück
  return(names(matches)[which.max(matches)])
}
# Verwendung der Funktion

key_matches <- count_key_matches(df, "first", max_last_joint_all_a)
print(key_matches)



library(dplyr)
library(tidyr)
library(stringr)

# Funktion, die Dateinamen in einen DataFrame aufteilt und nur Ziffern in "first" und "second" behält
process_filenames <- function(df) {
  df %>%
    mutate(filename = basename(V2)) %>%  # Erstellen Sie eine neue Spalte "filename" mit nur dem Dateinamen.
    separate(filename, into = c("first", "second", "extension"), sep = "_", remove = FALSE) %>%  # Trennen Sie den Dateinamen in "first", "second" und "extension"
    mutate(first = str_extract(first, "\\d+"),  # Behalten Sie nur die Ziffern im "first"
           second = str_extract(second, "\\d+"))  # Behalten Sie nur die Ziffern im "second"
}

# Verwendung der Funktion
df_to_surv_cut.cut <- process_filenames(df)

#Spalten umbenennen
rename_column <- function(df, old_name, new_name) {
  df %>%
    rename(!!new_name := !!old_name)
}

# Verwendung der Funktion

merged_df <- rename_column(merged_df, "V6", "maV_KI_new")

#Spalten umbenennen mehrere

rename_columns <- function(df, name_list) {
  for(i in 1:length(name_list)) {
    df <- df %>%
      rename(!!name_list[[i]][2] := !!name_list[[i]][1])
  }
  return(df)
}

name_list <- list(c("V3", "Width_new"), c("V4", "Height_new"), c("V5", "Ma_Fat_Droplets_new"), c("V7", "Mi_Fat_Droplets_new"), c("V8", "miV_KI_new"))
merged_df <- rename_columns(merged_df, name_list)

# get list of categorical columns
categorical_cols <- names(which(sapply(df, is.factor)))
# print number of categorical columns
cat("Number of categorical columns: ", length(categorical_cols))

# function to reclassify categories
reclassify <- function(x) {
  order_counts <- sort(table(x), decreasing = TRUE)
  x <- as.character(x)
  x[!(x %in% names(order_counts)[1:4])] <- "Other"
  return(as.factor(x))
}

count_single_level_vars <- function(df) {
  # Find variables with only one unique level (excluding NA values)
  to_drop <- sapply(df, function(x) length(unique(na.omit(x))) < 2)
  
  # Count the number of variables with only one unique level
  drop_count <- sum(to_drop)
  
  return(drop_count)
}

# # Count columns marked to be dropped
# drop_count <- count_single_level_vars(df)
# print(drop_count)

# Find the columns with more than 5 unique levels
cols_with_many_levels <- names(df)[sapply(df, function(x) is.factor(x) && length(unique(x)) > 5)]

# Function to reclassify categories
reclassify <- function(x) {
  count_level <- sort(table(x), decreasing = TRUE)
  x <- as.character(x)
  x[!(x %in% names(count_level)[1:4])] <- "Other"
  return(as.factor(x))
}

# Apply function to each column
df[cols_with_many_levels] <- lapply(df[cols_with_many_levels], reclassify)

head(df)

truncate_column_names <- function(df, max_length = 55) {
  # Define the abbreviations
  abbreviations <- c(
    "abdominal" = "abd",
    "abdominal_fat_index" = "abd_fat_index",
    "adjusted" = "adj",
    "age_by_listung" = "age_by_lstg",
    "amount" = "amt",
    "amount_of_catecholamine" = "amt_of_noradr",
    "bili_listing" = "bili_lstg",
    "bili_removal" = "bili_rem",
    "bilirubin" = "bil",
    "blood_type" = "bt",
    "bmi" = "bmi",
    "bone" = "bone",
    "bone_fat_index" = "bone_fat_index",
    "bone_normalized" = "bone_norm",
    "cardiac" = "card",
    "catecholamine_cat" = "noradr_cat",
    "category" = "cat",
    "cause" = "cause",
    "cause_of_death" = "cause_of_death",
    "child_pugh_score" = "cp_score",
    "combined" = "combined",
    "contact" = "contact",
    "crea_listing" = "crea_lstg",
    "crea_removal" = "crea_rem",
    "creatinin" = "creat",
    "cutpoint" = "cutpt",
    "days" = "d",
    "days_of_dialysis" = "d_of_dial",
    "days_of_icu" = "d_of_icu",
    "days_of_ventilation" = "d_of_vent",
    "death" = "death",
    "death_on_the_list_without_ltx" = "death_on_list_w_o_ltx",
    "difference" = "diff",
    "dialysis" = "dial",
    "dialysis_cat" = "dial_cat",
    "eat" = "eat",
    "eat_normalized" = "eat_norm",
    "epicardial" = "epicard",
    "exp" = "exp",
    "extended" = "ext",
    "fat" = "fat",
    "from" = "from",
    "function" = "function",
    "hcc" = "hcc",
    "height" = "ht",
    "hepatic" = "hep",
    "hu_listung" = "hu_lstg",
    "icu_cat" = "icu_cat",
    "imat" = "intra_muscle_adip_tiss",
    "imat_normalized" = "intra_muscle_adip_tiss_norm",
    "index" = "index",
    "inr_listing" = "inr_lstg",
    "inr_removal" = "inr_rem",
    "lab_meld_listing" = "lab_meld_lstg",
    "lab_meld_nt" = "lab_meld_nt",
    "lap_meld_death" = "lap_meld_death",
    "lap_meld_ltx" = "lap_meld_ltx",
    "last" = "last",
    "listing" = "listing",
    "log" = "log",
    "ltx" = "ltx",
    "median" = "med",
    "muscle" = "muscle",
    "muscle_normalized" = "muscle_norm",
    "myosteatotic" = "myost",
    "myosteatotic_fat_index" = "myost_fat_index",
    "natrium_removal" = "natrium_rem",
    "normalized" = "norm",
    "number" = "num",
    "paracardial" = "paracard",
    "pat" = "paracard_adip_tiss",
    "pat_normalized" = "paracard_adip_tiss_norm",
    "patient" = "patient",
    "patient_number" = "patient_num",
    "platelets_listing" = "platelets_lstg",
    "platelets_removal" = "platelets_rem",
    "portal_vein_thrombosis" = "v_portae_thromb",
    "primary_diagnosis" = "primary_diag",
    "ratio" = "ratio",
    "removal" = "rem",
    "rounded" = "rounded",
    "sarcopenia" = "sarcopenia",
    "sarcopenia_index_2" = "sarcopenia_index_2",
    "sat" = "skelet_adip_tiss",
    "sat_normalized" = "skelet_adip_tiss_norm",
    "sex" = "sex",
    "sodium_listing" = "sodium_lstg",
    "subcutaneous" = "subcut",
    "survival" = "surv",
    "survival_days_from_nt_until_last_contact" = "surv_d_fr_nt_until_lst_contact",
    "survival_days_from_wl_until_death" = "surv_d_fr_wl_until_death",
    "survival_days_until_ltx" = "surv_d_until_ltx",
    "survival_days_until_nt" = "surv_d_until_nt",
    "tat" = "tot_adip_tiss",
    "tat_normalized" = "tot_adip_tiss_norm",
    "time" = "time",
    "total" = "tot",
    "until" = "until",
    "vat" = "visceral_adip_tiss",
    "vat_normalized" = "visceral_adip_tiss_norm",
    "ventilation" = "vent",
    "ventilation_cat" = "vent_cat",
    "visceral" = "visceral",
    "weight" = "wt",
    "without" = "w_o"
  )
  
  
  
  # Get the column names
  col_names <- colnames(df)
  
  # Function to truncate a single column name
  truncate_name <- function(name) {
    # Replace specific parts of the name with their abbreviations
    for (original in names(abbreviations)) {
      name <- gsub(original, abbreviations[original], name)
    }
    # If the name is still too long, truncate it
    if (nchar(name) > max_length) {
      name <- substr(name, 1, max_length)
    }
    return(name)
  }
  
  # Apply the function to each column name
  col_names <- sapply(col_names, truncate_name)
  
  # Set the new column names in the dataframe
  colnames(df) <- col_names
  
  # Return the dataframe with truncated column names
  return(df)
}


#benutzung
# Verwenden Sie die Funktion mit einer maximalen Länge von 30 Zeichen
df_truncated <- truncate_column_names(df, max_length = 30)

# Rename the variable using base R
names(df_with_timeS)[names(df_with_timeS) == "death_on_the_list_w/o_ltx"] <- "death_on_the_list_w_o_ltx"

#unregister parallelcomuting
unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
# Doppelte Einträge finden
duplicates <- duplicated(df_truncated)

# Anzahl der doppelten Einträge
sum(duplicates)

# Doppelte Einträge entfernen
unique_abbreviations <- unique(abbreviations)

"lap_meld_death" %in% names(df_selected_vars_train_df)



get_best_models <- function(all_combination_of_vars, aic_values, x, df_train, df_test) {
  # Sortieren der AIC-Werte und Abrufen der Indizes der sortierten Werte
  sorted_aic_indices <- order(aic_values)
  
  # Auswahl der ersten x Indizes
  best_aic_indices <- sorted_aic_indices[1:x]
  
  # Auswahl der besten Modellkombinationen
  best_models <- lapply(all_combination_of_vars[best_aic_indices], paste, collapse = ", ")
  
  # Auswahl der besten AIC-Werte
  best_aic_values <- aic_values[best_aic_indices]
  
  # Berechnung der AIC Differenzen
  aic_differences <- best_aic_values - min(aic_values)
  
  # Zählen der Anzahl der verwendeten Variablen in jedem Modell
  num_vars <- stringr::str_count(unlist(best_models), ",") + 1
  
  # Erstellung der besten Modelle
  best_models_trained <- lapply(best_models, function(vars) {
    formula <- as.formula(paste("death ~", gsub(", ", " + ", vars)))
    model <- glm(formula, family = binomial, data = df_train)
    return(model)
  })
  
  # Vorhersagen mit den trainierten Modellen
  predictions <- lapply(best_models_trained, function(model) {
    probs <- predict(model, newdata = df_test, type = "response")
    preds <- ifelse(probs > 0.5, 1, 0)
    return(preds)
  })
  
  # Berechnen der Leistungsmetriken
  performance_metrics <- lapply(predictions, function(preds) {
    confusion <- table(df_test$death, preds)
    accuracy <- sum(diag(confusion)) / sum(confusion)
    sensitivity <- confusion[2,2] / sum(confusion[2,])
    specificity <- confusion[1,1] / sum(confusion[1,])
    ppv <- confusion[2,2] / sum(confusion[,2])
    npv <- confusion[1,1] / sum(confusion[,1])
    return(c(accuracy, sensitivity, specificity, ppv, npv))
  })
  
  # Erstellung einer Dataframe mit den Modellkombinationen, AIC-Werten, AIC Differenzen, Anzahl der Variablen und Leistungsmetriken
  results <- data.frame(models = unlist(best_models), aic_values = best_aic_values, aic_differences = aic_differences, num_vars = num_vars, do.call(rbind, performance_metrics))
  colnames(results)[5:9] <- c("accuracy", "sensitivity", "specificity", "ppv", "npv")
  
  return(results)
}

corr_res <- map(df_selected_vars_borut %>% select(-c(vat,death)), cor.test, y = df_selected_vars_borut$vat)

corr_res %>% 
  # Convert each to a tidy format; `map_dfr()` stacks the data frames 
  map_dfr(tidy, .id = "predictor") %>% 
  ggplot(aes(x = fct_reorder(predictor, estimate))) + 
  geom_point(aes(y = estimate)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .1) +
  labs(x = NULL, y = "Correlation with death")

log_transform <- function(df, skewness_threshold, suffix) {
  # Load library
  library(moments)
  
  # Create a copy of the original dataframe
  df_transformed <- df
  
  # For each variable
  for (variable in names(df_transformed)) {
    # Check if the variable is categorical and if it is numeric
    if (!grepl("_cat", variable) & is.numeric(df_transformed[[variable]])) {
      # Calculate skewness
      skewness <- skewness(df_transformed[[variable]], na.rm = TRUE)
      
      # If the data is right-skewed
      if (!is.na(skewness) && abs(skewness) > skewness_threshold) {
        # Check if minimum value is less than or equal to zero
        min_val <- min(df_transformed[[variable]], na.rm = TRUE)
        if (min_val <= 0) {
          # Add a constant to make all values positive before log transformation
          df_transformed[[variable]] <- df_transformed[[variable]] - min_val + 1
        }
        # Perform log transformation
        df_transformed[[paste(variable, suffix, sep = "_")]] <- log1p(df_transformed[[variable]])
      }
    }
  }
  return(df_transformed)
}







create_histograms <- function(df_original, df_transformed) {
  # Load libraries
  library(ggplot2)
  library(gridExtra)
  library(moments)
  
  # Find variables in df_transformed that have the suffix "_log"
  log_vars <- grep("_log$", names(df_transformed), value = TRUE)
  
  # Remove the suffix "_log" to get the corresponding variables in df_original
  original_vars <- sub("_log$", "", log_vars)
  
  # For each pair of variables
  for (i in seq_along(original_vars)) {
    original_var <- original_vars[i]
    log_var <- log_vars[i]
    
    # Check if the variable is numeric
    if (is.numeric(df_transformed[[log_var]])) {
      # Calculate skewness
      skewness_before <- skewness(df_original[[original_var]], na.rm = TRUE)
      skewness_after <- skewness(df_transformed[[log_var]], na.rm = TRUE)
      
      # Create histograms
      p1 <- ggplot(df_original, aes(!!sym(original_var))) + 
        geom_histogram(aes(y = after_stat(density)), bins = 50, col= "white") +
        geom_density(alpha = .2, fill="#FF6666") +
        labs(x = original_var, title = paste("Histogram of", original_var, "(before transformation)")) +
        annotate("text", x = Inf, y = Inf, label = paste("Skewness:", round(skewness_before, 2)), hjust = 1, vjust = 1)
      
      p2 <- ggplot(df_transformed, aes(!!sym(log_var))) + 
        geom_histogram(aes(y = after_stat(density)), bins = 50, col= "white") +
        geom_density(alpha = .2, fill="#FF6666") +
        labs(x = log_var, title = paste("Histogram of", log_var, "(after transformation)")) +
        annotate("text", x = Inf, y = Inf, label = paste("Skewness:", round(skewness_after, 2)), hjust = 1, vjust = 1)
      
      # Display histograms side by side
      grid.arrange(p1, p2, ncol = 2)
    }
  }
}


calculate_statistics <- function(df_input) {
  # Bibliothek laden
  library(moments)
  
  # DataFrame erstellen
  df <- data.frame(Variable = character(), Median = numeric(), IQR = numeric(), Skewness = numeric())
  
  # Für jede Variable
  for (variable in names(df_input)) {
    # Überprüfen, ob die Variable numerisch ist
    if (is.numeric(df_input[[variable]])) {
      # Verteilung und Schiefe berechnen
      median <- median(df_input[[variable]], na.rm = TRUE)
      iqr <- IQR(df_input[[variable]], na.rm = TRUE)
      skewness <- skewness(df_input[[variable]], na.rm = TRUE)
      
      # Ergebnisse zum DataFrame hinzufügen
      df <- rbind(df, data.frame(Variable = variable, Median = median, IQR = iqr, Skewness = skewness))
    }
  }
  
  # DataFrame zurückgeben
  return(df)
}


recode_factor_to_numeric <- function(df) {
  for (colname in colnames(df)) {
    if (grepl("_category$", colname)) {
      df[[colname]] <- as.numeric(df[[colname]] == "Über_Median")
      names(df)[names(df) == colname] <- sub("_category$", "_num", colname)
    } else if (grepl("_cat$", colname)) {
      df[[colname]] <- as.numeric(df[[colname]] == "Über_Cutpoint")
      names(df)[names(df) == colname] <- sub("_cat$", "_num", colname)
    }
  }
  return(df)
}

recode_numeric_to_factor <- function(df) {
  for (colname in colnames(df)) {
    if (grepl("_num$", colname)) {
      if (grepl("_category_num$", colname)) {
        df[[colname]] <- factor(df[[colname]], levels = c(0, 1), labels = c("Unter_Median", "Über_Median"))
        names(df)[names(df) == colname] <- sub("_category_num$", "_category", colname)
      } else if (grepl("_cat_num$", colname)) {
        df[[colname]] <- factor(df[[colname]], levels = c(0, 1), labels = c("Unter_Cutpoint", "Über_Cutpoint"))
        names(df)[names(df) == colname] <- sub("_cat_num$", "_cat", colname)
      }
    }
  }
  return(df)
}

# Use the function on your dataframe

# Apply the function to your dataframe
df_num <- recode_factor_to_numeric(df)




df_log <- log_transform(df,1,"log")
create_histograms(df, df_log)
df_statistics <- calculate_statistics(df_log)
print(df_statistics)


# Create dummy variables for informative missingness: For variables like
# 'survival_days_until_nt', you could create a new variable (let's call it
# 'status_change'), where 1 indicates the patient's status was changed from
# transplantable to non-transplantable, and 0 indicates the patient's status was
# never changed. Then, you can replace the NA values in the
# 'survival_days_until_nt' variable with 0 (indicating that no days passed until
# a status change, because there was no status change).

# Function to create dummy variables for informative missingness
create_dummy_vars <- function(df, column_names) {
  for(column_name in column_names){
    # Creating new dummy variable
    new_column_name <- paste(column_name, "_status_changed", sep = "")
    df[, new_column_name] <- ifelse(is.na(df[, column_name]), 0, 1)
    
    # Replacing NA in original column with 0
    df[, column_name][is.na(df[, column_name])] <- 0
  }
  return(df)
}


# Function to impute missing values and plot old vs new variable
impute_and_plot <- function(df, column_names) {
  
  # Impute missing values using mice
  temp_data <- mice(df[, column_names], m = 5, maxit = 50, method = 'pmm', seed = 500)
  df_imputed <- complete(temp_data, 1)  # Use the first imputed dataset
  
  # Loop over each column to create the plots
  for(column_name in column_names){
    
    # Create a new data frame for plotting
    plot_df <- data.frame(Old = df[, column_name],
                          Imputed = df_imputed[, column_name])
    
    # Generate the plot
    p <- ggplot(plot_df) +
      geom_density(aes(x = Old), fill = "red", alpha = 0.5) +
      geom_density(aes(x = Imputed), fill = "blue", alpha = 0.5) +
      labs(x = column_name, y = "Density",
           title = paste("Density plot of old and imputed values for", column_name),
           fill = "Variable") +
      theme_minimal() +
      scale_fill_manual(values = c("red", "blue"), labels = c("Old", "Imputed")) +
      guides(fill = guide_legend(reverse = TRUE))
    
    print(p)  # This prints the plot in each loop iteration
  }
  
  return(df_imputed)
}


# Load required library
library(gridExtra)
# Function to impute missing values and plot old vs new variable
impute_and_plot <- function(df, column_names) {
  
  # Determine the imputation method for each column
  imputation_methods <- sapply(df[, column_names], function(col) {
    if (is.numeric(col)) {
      return('pmm')  # Predictive Mean Matching for numeric variables
    } else {
      return('logreg')  # Logistic Regression for categorical variables
    }
  })
  
  # Impute missing values using mice
  temp_data <- mice(df[, column_names], m = 5, maxit = 50, method = imputation_methods, seed = 500)
  
  # Replace missing values in original data with imputed values (from the first imputed dataset)
  df_imputed_final <- complete(temp_data, 1)
  
  # Loop over each column to create the plots
  for(column_name in column_names){
    
    if (is.numeric(df[, column_name])) {
      # Create a new data frame for plotting
      plot_df <- data.frame(Old = df[, column_name],
                            Imputed = df_imputed_final[, column_name])
      
      # Transform data to long format for ggplot
      plot_df_long <- reshape2::melt(plot_df)
      
      # Generate the plot
      p <- ggplot(plot_df_long, aes(x = value, colour = variable, linetype = variable)) +
        geom_density(size = 1.5) +
        labs(x = column_name, y = "Density",
             title = paste("Density plot of old and imputed values for", column_name),
             fill = "Variable") +
        theme_minimal() +
        scale_colour_manual(values = c("red", "blue"), labels = c("Old", "Imputed")) +
        guides(colour = guide_legend(reverse = TRUE), linetype = guide_legend(reverse = TRUE))
    } else {
      # Count the frequency of each category
      old_freq <- table(df[, column_name])
      imputed_freq <- table(df_imputed_final[, column_name])
      
      # Create a new data frame for plotting
      plot_df <- data.frame(Category = c(names(old_freq), names(imputed_freq)),
                            Frequency = c(as.vector(old_freq), as.vector(imputed_freq)),
                            Variable = rep(c("Old", "Imputed"), each = length(old_freq)))
      
      # Generate the plot
      p <- ggplot(plot_df, aes(x = Category, y = Frequency, fill = Variable)) +
        geom_bar(stat = "identity", position = 'dodge') +
        labs(x = column_name, y = "Frequency",
             title = paste("Frequency plot of old and imputed values for", column_name),
             fill = "Variable") +
        theme_minimal() +
        scale_fill_manual(values = c("red", "blue"), labels = c("Old", "Imputed")) +
        guides(fill = guide_legend(reverse = TRUE))
    }
    
    # Print the plot
    print(p)
  }
  
  df[, column_names] <- df_imputed_final[, column_names]
  
  return(df)  # This returns the original data with missing values replaced by imputed values
}

# Now you can call this function for columns with missing values
df <- impute_and_plot(df, c('amount_of_catecholamine', 'bone', 'muscle', 'hcc'))



# Now you can call this function for columns with missing values
df <- impute_and_plot(df, c('amount_of_catecholamine', 'bone', 'muscle',"lab_meld_listing","crea_listing","sat","vat","imat","eat","pat","tat","hcc"))

recode_to_factor <- function(df, cols = NULL) {
  # If no specific columns are provided, use all character columns
  if (is.null(cols)) {
    cols <- names(df)[sapply(df, is.character)]
  }
  
  for (col in cols) {
    if(is.character(df[[col]])) {
      df[[col]] <- as.factor(df[[col]])
    }
  }
  
  return(df)
}

# Now you can call recode_to_factor(df) to convert all character columns in df to factors. If you want to convert specific columns, you can still provide them as a second argument like so: recode_to_factor(df, c('column1', 'column2')).
# 

# Helper function for ggplot2 customization
.ggpar <- function(p, ggtheme = theme_classic(), ...){
  argmt <- list(...)
  p <- ggpubr::ggpar(p, ggtheme = ggtheme, ...)
  if(is.null(argmt$font.x)) p <- p + theme(axis.text.x = element_text(face = "plain"))
  if(is.null(argmt$font.y)) p <- p + theme(axis.text.y = element_text(face = "plain"))
  p
}

# Function to dichotomize variables
.dichotomize <- function(x, cutpoint, labels = c("low", "high")){
  grps <- x
  grps[x <= cutpoint] = labels[1]
  grps[x > cutpoint] = labels[2]
  grps
}

# Function to get non-numeric variables from a data.frame
.get_not_numeric_vars <- function(data_frame){
  is_numeric <- sapply(data_frame, is.numeric)
  if(sum(!is_numeric) == 0) res = NULL
  else res <- colnames(data_frame[, !is_numeric, drop = FALSE])
  res
}

# Function to categorize survival data
surv_categorize <- function(x, variables = NULL, labels = c("low", "high")) {
  if (!inherits(x, "surv_cutpoint")) 
    stop("x must be an object of class surv_cutpoint.")
  
  data <- x$data
  surv_data <- x$data[, 1:2]
  data <- x$data[, -1 * c(1:2), drop = FALSE]
  not_numeric <- x$not_numeric
  
  if (is.null(variables)) 
    variables <- colnames(data)
  
  data <- data[, variables, drop = FALSE]
  cutpoints <- x$cutpoint[variables, "cutpoint"]
  
  nvar <- length(variables)
  
  if (nvar >= 2) {
    res <- apply(t(data), 2, .dichotomize, cutpoints, labels)
    res <- as.data.frame(t(res))
    colnames(res) <- variables
  } else {
    res <- data
    res[, 1] <- .dichotomize(res[, 1], cutpoints, labels)
  }
  
  res <- cbind.data.frame(surv_data, res)
  print("Available variables in x$cutpoint:")
  print(names(x$cutpoint))
  print("Variables to be processed:")
  print(variables)
  print("Inspecting x$cutpoint:")
  print(str(x$cutpoint))
  # Adjusted to match your debug output
  cutpoints <- setNames(x$cutpoint$cutpoint, rownames(x$cutpoint))
  # Add extra variables
  for (var in variables) {
    print(paste("Looking for cutpoint for variable:", var))
    actual_cutpoint <- if(!is.null(cutpoints[var])) cutpoints[var] else NA
    print(paste("Actual cutpoint found:", actual_cutpoint))
    print(paste("Cutpoint for", var, ":", cutpoints[var]))
    print(head(res[, var], 10))
    
    new_var_cat <- ifelse(res[, var] > cutpoints[var], "high", "low")
    
    print(head(new_var_cat, 10))
    
    res[paste(var, "_cutpoint_cat", sep = "")] <- ifelse(res[, var] > actual_cutpoint, "high", "low")
    res[paste(var, "_cutpoint", sep = "")] <- actual_cutpoint
  }
  
  if (!is.null(not_numeric)) 
    res <- cbind.data.frame(res, not_numeric)
  
  attr(res, "labels") <- labels
  structure(res, class = c("data.frame", "surv_categorize"))
}

surv_categorize_new <- function(x, variables = NULL, labels = c("low", "high")) {
  if (!inherits(x, "surv_cutpoint")) {
    stop("x must be an object of class surv_cutpoint.")
  }
  
  # Extract survival data and other columns
  surv_data <- x$data[, 1:2]
  data <- x$data[, -1 * c(1:2), drop = FALSE]
  not_numeric <- x$not_numeric
  
  # If variables are not specified, use all columns in the data
  if (is.null(variables)) {
    variables <- colnames(data)
  }
  
  # Check if variables exist in the data
  if (!all(variables %in% colnames(data))) {
    stop("Some variables do not exist in the data.")
  }
  
  # Extract cutpoints based on the actual structure of x$cutpoint
  cutpoints <- setNames(x$cutpoint$cutpoint, rownames(x$cutpoint))
  
  # Subset data and initialize result
  res <- data[, variables, drop = FALSE]
  
  for (var in variables) {
    actual_cutpoint <- if (!is.null(cutpoints[var])) cutpoints[var] else NA
    
    if (is.na(actual_cutpoint)) {
      warning(paste("Actual cutpoint for variable", var, "is NA. Skipping this variable."))
      next
    }
    
    # Create new columns using vectorized operations
    res[paste(var, "_cutpoint_cat", sep = "")] <- ifelse(res[, var] > actual_cutpoint, labels[2], labels[1])
    res[paste(var, "_cutpoint", sep = "")] <- actual_cutpoint
  }
  
  # Combine all columns to create the final result
  res <- cbind.data.frame(surv_data, res)
  if (!is.null(not_numeric)) {
    res <- cbind.data.frame(res, not_numeric)
  }
  
  attr(res, "labels") <- labels
  structure(res, class = c("data.frame", "surv_categorize"))
}


# Relocate_Columns --------------------------------------------------------


# Move the columns "time", "death", and "year" to the front
final_df <- final_df %>% relocate(c("time", "death", "year"))

# Move all factor columns after the "year" column
final_df <- final_df %>% relocate(where(is.factor), .after = "year")

# Move all numeric columns after the last factor column
final_df <- final_df %>% relocate(where(is.numeric), .after = where(is.factor))

# Move all POSIXct columns after the last numeric column
final_df <- final_df %>% relocate(where(is.POSIXct), .after = where(is.numeric))


# High Cardinality Treatment ----------------------------------------------


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





# Get the names of variables in df1 that are not in df2 -------------------
#
diff_vars <- setdiff(names(final_df), names(data))
print(diff_vars)

# # Get the names of variables in df1 that are in dfd2 --------------------

common_vars <- intersect(names(final_df), names(data))
print(common_vars)

# Specify the variable you want to check
var <- "primary_diagnosis_short"

# specific variable from dataset 1 is in dataset 2 ------------------------
# Check if the variable is in df2
var_in_df2 <- var %in% names(data)

print(var_in_df2)


# Variable_Impotance_funModeling ------------------------------------------

variable_importance <- var_rank_info(data, "death")

# Printing results
variable_importance

# Plotting 
ggplot(variable_importance, 
       aes(x = reorder(var, gr), 
           y = gr, fill = var)
) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  theme_bw() + 
  xlab("") + 
  ylab("Variable Importance 
       (based on Information Gain)"
  ) + 
  guides(fill = FALSE)



# function in R to add a specific variable from dataset 1 into dat --------

# Assume df1 and df2 are your two datasets
df1 <- data.frame(a = 1:5, b = 6:10, c = 11:15)
df2 <- data.frame(d = 16:20, e = 21:25, f = 26:30)

# Specify the variable you want to add
var <- "date_of_birth"

# Check if the variable is in df1
if (var %in% names(final_df)) {
  # Add the variable to df2
  data <- cbind(data, final_df[,var])
}

print(data)

# Specify the variable you want to add
var <- "date_of_birth"

# Check if the variable is in final_df
if (var %in% names(final_df)) {
  # Merge data and final_df by id and var
  data <- merge(data, final_df[, c("bone", var)], by = "bone", all.x = TRUE)
}

print(data)
