library(openxlsx)
library(writexl)
library(survminer)
library(survival)
library(gt)
library(ggthemes)
library(funModeling)
library(tidyverse)

# Save your workspace to a .RData file
save.image(file = "03_09_2023.RData")

write_xlsx(final_df, "final_df_1_09_2023.xlsx")


write.xlsx(final_df, "df_to_surv_cut_cut_21_08_2023.xlsx")
write.xlsx(final_df, "df_to_surv_cut_cut_21_08_2023.xlsx", sheetName = "ShortName")


saveRDS(final_df, "final_df_1_09_2023.rds")

final_df <- recode_to_factor(final_df)
final_df$death <- as.numeric(as.character(final_df$death))

# Funktion zum Formatieren von Lancet-Stil-Tabellen
format_lancet_table <- function(data) {
  gt(data) %>%
    tab_header(
      title = "Table 1",
      subtitle = "Summary of Numeric and Categorical Variables"
    ) %>%
    tab_options(
      table.width = px(900),
      table.font.size = "12pt",
      table.font.names = "Arial"
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(columns = everything())
    ) %>%
    tab_style(
      style = cell_fill(color = "#D3D3D3"),
      locations = cells_column_labels(columns = everything())
    )
}


# Formatieren und Anzeigen der numerischen Zusammenfassungstabelle
numeric_summary_table <- format_lancet_table(final_df)
numeric_summary_table

# Formatieren und Anzeigen der kategorischen Tabelle
cat_table_lancet <- format_lancet_table(cat_table)
cat_table_lancet



df_status(final_df)
df_to_surv_cut <- final_df[,c(2:3,16:26,34:48)]
df_status(df_to_surv_cut)

vars_to_cut <- names(df_to_surv_cut[,-c(1,2)])
final_df <- surv_cutpoint(
  df_to_surv_cut,
  time = "time",
  event = "death",
  variables = vars_to_cut
)
summary(final_df)
plot(final_df, vars_to_cut, palette = "npg")

final_df <- survminer::surv_categorize(df_to_surv_cut.cut) 
final_df <- surv_categorize(df_to_surv_cut.cut)
# old_result <- surv_categorize_old(final_df)
# new_result <- surv_categorize_new(final_df)  
# identical(old_result, new_result)

# Initialize an empty list to store the results
fit_list <- list()

# Loop over the variables in vars_to_cut
for (var in vars_to_cut) {
  # Create the formula string
  formula_str <- paste("Surv(time, death) ~", var)
  
  # Convert the string to a formula object
  formula_obj <- as.formula(formula_str)
  
  # Fit the survival model and store the result in the list
  fit_list[[var]] <- survfit(formula_obj, data = final_df)
}

# Now fit_list is a list of survfit objects, one for each variable in vars_to_cut.


# Set up a multi-panel plot
par(mfrow = c(2, 2)) # This will create a 2x2 grid for plots

# Loop over the fit_list
for (i in 1:length(fit_list)) {
  # Create a new plot for each model
  plot(fit_list[[i]], main = names(fit_list)[i], xlab = "Time", ylab = "Survival Probability")
}


# Loop over the variables in vars_to_cut
for (var in vars_to_cut) {
  
  # Create the formula string
  formula_str <- paste("Surv(time, death) ~", var)
  
  # Debug: Print formula string
  cat("Formula string:", formula_str, "\n")
  
  # Fit the survival model using the formula string directly
  fit <- tryCatch(
    survfit(as.formula(formula_str), data = final_df),
    error = function(e) {
      cat("Error in fitting model:", e$message, "\n")
      return(NULL)
    }
  )
  
  if (is.null(fit)) next  # Skip the rest of this iteration if an error occurred
  
  # Create a new plot for each model using ggsurvplot()
  p <- ggsurvplot(
    fit,
    risk.table = TRUE,
    pval = TRUE,
    conf.int = FALSE,
    xlim = c(0, 3000),
    break.time.by = 365,
    surv.median.line = "hv",
    ggtheme = theme_clean(),
    risk.table.y.text.col = T,
    risk.table.y.text = FALSE
  )
  
  print(p)  # Print the plot to the graphics device
}
class(final_df) <- "data.frame"
final_df <- as.data.frame(final_df)
str(final_df)


df_status(final_df)
diagnose(final_df) %>% flextable()
diagnose_category(final_df) %>% flextable()
diagnose_numeric(final_df) %>% flextable()
diagnose_outlier(final_df) %>% flextable()
col_outlier <- find_outliers(final_df, index = F)

final_df%>% 
  select(col_outlier) %>% 
  plot_outlier()

backup <- final_df
final_df <- backup 

# Create a new data frame with the same structure but no data
new_df <- data.frame(matrix(ncol = ncol(final_df), nrow = nrow(final_df)))
colnames(new_df) <- colnames(final_df)

# Copy data from the old data frame to the new one
for (col in colnames(final_df)) {
  new_df[[col]] <- final_df[[col]]
}

# Check if the new data frame is a standard data frame
str(new_df)

# Identify the character columns
char_cols <- sapply(new_df, is.character)

# Convert the character columns to factors
new_df[char_cols] <- lapply(new_df[char_cols], factor)
str(new_df)

# Get the current column names
current_colnames <- colnames(new_df)

# Add the suffix "_cut" to the names, except for the first two ("time" and "death")
new_colnames <- c("time", "death", paste0(current_colnames[-c(1,2)], "_cut"))

# Assign the new column names to the data frame
colnames(new_df) <- new_colnames




# List of columns you want to replace in new_df
columns_to_replace <-
  c(
    "age_by_listung",
    "bmi",
    "bone_muscle_index",
    "exceptional_meld",
    "height",
    "inr_listing",
    "known_mortality",
    "mortality_increase",
    "patient_number",
    "platelets_listing",
    "sodium_listing",
    "visceral_to_subcutaneous_fat_index_extended",
    "weight",
    "vat",
    "bone",
    "muscle",
    "sat",
    "imat",
    "eat",
    "tat",
    "pat",
    "bone_to_visceral_fat_index_normalized",
    "combined_fat_muscle_index_normalized",
    "imat_normalized",
    "muscle_normalized",
    "muscle_to_paracardial_fat_index_normalized"
  )

# Copy the specified columns from df_to_surv_cut.cut to new_df
new_df[columns_to_replace] <- final_df[columns_to_replace]

backup <- final_df
final_df <- backup
# Identify columns in new_df with suffix "_cutpoint_cat" or "_cutpoint"
suffixes <- c("_cut")
selected_columns <- grep(paste(suffixes, collapse="|"), names(new_df), value=TRUE)

# Add "time", "death", "patient_number" to the list of selected columns
selected_columns <- c(selected_columns, "time", "death")
# Copy selected columns from new_df
copied_columns <- new_df[, selected_columns]

# Merge copied_columns with df based on "time", "death", and 
# # "patient_number"
# 
final_df <- merge(final_df, copied_columns, by=c("time", "death"))

# Erhalte alle Faktorvariablennamen
factor_vars <- names(final_df)[sapply(final_df, is.factor)]

# Verschiebe alle Faktorvariablen nach 'death'
final_df <- final_df %>% relocate(all_of(factor_vars), .after = "year")



# Check the structure of the new data frame
str(new_df)



df_status(final_df)

