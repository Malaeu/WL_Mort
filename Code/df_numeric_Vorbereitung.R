install.packages("bestglm")
library(bestglm)

# Load your dataframe
# my_data <- read.csv("your_data_file.csv")

# Create a survival object
surv_obj <- with(my_data, Surv(FollowUP, death == "yes", type = "right"))

# Run a brute-force search using bestglm package
best_glm <- bestglm(my_data, family = coxph(surv_obj), IC = "AIC")

# Get the best model
best_model <- best_glm$BestModel

# Print the summary of the best model
summary(best_model)


Install and load the necessary packages:
  install.packages(c("survival", "glmulti", "dplyr"))
library(survival)
library(glmulti)
library(dplyr)



# Ersetzen Sie alle 0-Werte in survival_days_until_ltx durch entsprechende Werte aus survival_days_until_nt
df_s$Time <- ifelse(df_s$survival_days_until_ltx == 0, df_s$survival_days_until_nt, df_s$survival_days_until_ltx)
# Überprüfen Sie, ob der Wert in "Time" kleiner ist als der entsprechende Wert in "survival_days_from_wl_until_death"
# Wenn ja, nehmen Sie den Wert aus "survival_days_from_wl_until_death", ansonsten behalten Sie den Wert aus "Time"
df_s$Time <- ifelse(df_s$Time < df_s$survival_days_from_wl_until_death, df_s$survival_days_from_wl_until_death, df_s$Time)
df_s$Time <- ifelse(df_s$Time < df_s$survival_days_from_nt_until_last_contact, df_s$survival_days_from_nt_until_last_contact, df_s$Time)

# Erzeugen Sie einen neuen Dataframe, der nur die Zeilen enthält, in denen alle Werte ungleich Null sind
df_s_nonzero <- df_s[df_s$survival_days_until_nt != 0 & 
                       df_s$survival_days_until_ltx != 0 &
                       df_s$survival_days_from_nt_until_last_contact != 0 &
                       df_s$survival_days_from_wl_until_death != 0, ]

df_s_nonzero <- df_s[df_s$survival_days_until_nt != 0 | 
                       df_s$survival_days_until_ltx != 0 |
                       df_s$survival_days_from_nt_until_last_contact != 0 |
                       df_s$survival_days_from_wl_until_death != 0, ]


print(nrow(df_s_nonzero))  # Anzahl der Beobachtungen in df_s
print(length(df_s_nonzero$Time))  # Anzahl der Beobachtungen in der Zielvariable
print(length(df_s_nonzero$death)) 


df_s_nonzero_prep <- df_s_nonzero[,-c(1,3:5,7:9,42)]
status_df_s_nonzero<- df_status(df_s_nonzero_prep)
Prediktoren <- status_df_s_nonzero$variable

# Löschen von Velues in Vektor Predictors
# Originaler Vektor
predictors <- c("hu_listung", "death", "sex", "age_by_listung", "height",
                "weight", "bmi", "blood_type", "primary_diagnosis", "hcc",
                "child_pugh_score", "lab_meld_listing", "inr_listing", "bili_listing",
                "crea_listing", "sodium_listing", "platelets_listing", "lab_meld_nt",
                "lap_meld_ltx", "lap_meld_death", "inr_removal", "bili_removal",
                "crea_removal", "natrium_removal", "platelets_removal", "dialysis_cat",
                "days_of_dialysis", "icu_cat", "days_of_icu", "ventilation_cat",
                "days_of_ventilation", "catecholamine_cat", "amount_of_catecholamine",
                "portal_vein_thrombosis", "bone", "muscle", "sat", "vat", "imat",
                "eat", "pat", "tat", "Time")

# Elemente, die entfernt werden sollen
remove_elements <- c("death", "Time")

# Entfernen der Elemente
predictors <- setdiff(predictors, remove_elements)


# Minimal-optimal feature selection using mRMR

# Install the package
# install.packages("mRMRe")

# Load the package
library(mRMRe)


# Auswahl nur numerischer Spalten

# Konvertieren von 'death' zu numerischen Werten
df_s_nonzero_prep$death <- as.numeric(df_s_nonzero_prep$death)
df_s_nonzero_prep$death <- as.numeric(df_s_nonzero_prep$death) - 1  # Subtrahieren Sie 1, da R von 1 anfängt zu zählen


numerics <- sapply(df_s_nonzero_prep, is.numeric)
df_numeric_only <- df_s_nonzero_prep[, numerics]

# Auswahl der relevanten Prädiktoren aus den numerischen Spalten
predictors_numeric <- predictors[predictors %in% names(df_numeric_only)]


# Identifizieren Sie die kategorischen Spalten
categoricals <- sapply(df_s_nonzero_prep, is.factor)

# Wählen Sie nur die kategorischen Spalten aus
df_categorical_only <- df_s_nonzero_prep[, categoricals]

# Auswahl der relevanten Prädiktoren aus den kategorischen Spalten
predictors_categorical <- predictors[predictors %in% names(df_categorical_only)]


# Anwendung von mRMR.data
data.mrmr <- mRMR.data(data = data.frame(df_numeric_only[, predictors_numeric]))



# Compute the mRMR ensemble
ensemble.mrmr <- mRMR.ensemble(data.mrmr, target_indices = 1, solution_count = 5)

# Print the solution(s)
print(solutions(ensemble.mrmr))

# All-relevant feature selection using Boruta

# Install the package
# install.packages("Boruta")

# Load the package
library(Boruta)

# Convert the outcome to factor if it's not
df_s$death <- as.factor(df_s$death)

# Perform Boruta feature selection
boruta_output <- Boruta(death ~ ., data = df_s_nonzero_prep[, c("death", predictors)], doTrace = 0)

# Print the results
print(boruta_output)
# Assuming boruta_output is your original Boruta result object
plot(boruta_output, xlab = "", xaxt = "n", main = "Variable Importance")
# Get the variable names
var_names <- getSelectedAttributes(boruta_output, withTentative = TRUE)

# Create an index for the x-axis
x_index <- 1:length(var_names)

# Add the variable names to the plot
axis(side = 1, at = x_index, labels = var_names, las = 2, cex.axis = 0.7)



# Get the final decision
final_decision <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(final_decision)

plot(final_decision, xlab = "", xaxt = "n", main = "Variable Importance")






# Überprüfen Sie die ersten Zeilen der aktualisierten Daten
head(df_s)

# Überprüfe die ersten Zeilen der aktualisierten Daten
head(df_s)


# Überprüfe die ersten Zeilen der aktualisierten Daten
head(df_s)

# Überprüfe die ersten Zeilen der aktualisierten Daten
head(df_s)


# my_data <- read.csv("your_data_file.csv")
#Create a survival object:

surv_obj <- with(df_s_nonzero, Surv(Time, death == "yes", type = "right"))

#Define a custom fit function that filters models based on both p-value and confidence intervals:

custom_coxph <- function(formula, data) {
  model <- coxph(formula, data)
  p_value_threshold <- 0.05
  conf_int_valid <- all(model$conf.int[, 1] > 0) && all(model$conf.int[, 2] < Inf)
  p_values_valid <- all(summary(model)$coef[, 5] < p_value_threshold)
  
  if (conf_int_valid && p_values_valid) {
    return(model)
  } else {
    model$null_log_lik <- model$loglik[1]
    model$loglik[2] <- model$null_log_lik + 1e6  # Set a very high AIC value
    return(model)
  }
}

# Define the formula
formula <- as.formula(Surv(Time, event) ~ .)





#Run a brute-force search using the glmulti package and the custom fit function:
glmulti_cox <- glmulti(surv_obj, data = df_s_nonzero, level = 1, crit = "AIC", fitfunc = custom_coxph, method = "h", confsetsize = 1)

best_model <- getmodels(glmulti_cox, best = TRUE)[[1]]
summary(best_model)




library(Boruta)
library(mlbench)
library(caret)
library(randomForest)


# Feature Selection
set.seed(123)
boruta <- Boruta(death ~ ., data = df_s_nonzero_prep[, c("death", predictors)], doTrace = 2, maxRuns = 100) # number of interations
# print(boruta)
plot(boruta, las = 2, cex.axis = 0.7) # cex.axis is used to reduce the font size
plotImpHistory(boruta)
bor <- TentativeRoughFix(boruta)
print(bor)
# Get the formula of important attributes
important_formula <- getConfirmedFormula(bor)

# Print the formula
print(important_formula)
