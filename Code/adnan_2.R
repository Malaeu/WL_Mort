# Laden Sie die benötigte Bibliothek
library(caret)
library(tidyverse)
library(h2o)
library(VIM)
library(FactoMineR)
library(missMDA)
library(naniar)
library(funModeling)
library(dplyr)
library(mice)
library(haven)# Export in .sav SPSS
library(smotefamily)
library(DMwR)
library(ROSE)

pacman::p_load(
  rio,          # File import
  here,         # File locator
  skimr,        # get overview of data
  tidyverse,    # data management + ggplot2 graphics 
  gtsummary,    # summary statistics and tests
  rstatix,      # summary statistics and statistical tests
  janitor,      # adding totals and percents to tables
  scales,       # easily convert proportions to percents  
  flextable,     # converting tables to pretty images
  funModeling,
  devtools,
  finalfit,
  kableExtra,
  h2o,
  VIM,
  FactoMineR,
  missMDA,
  naniar,
  mice,
  DMwR,
  gt
)



# Function for detecting NA observations: 
na_rate <- function(x) {x %>% is.na() %>% sum() / length(x)}

sapply(df_s, na_rate) %>% round(0)

# Function replaces NA by mean: 
replace_by_mean <- function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  return(x)
}

# A function imputes NA observations for categorical variables: 

replace_na_categorical <- function(x) {
  x %>% 
    table() %>% 
    as.data.frame() %>% 
    arrange(-Freq) ->> my_df
  
  n_obs <- sum(my_df$Freq)
  pop <- my_df$. %>% as.character()
  set.seed(29)
  x[is.na(x)] <- sample(pop, sum(is.na(x)), replace = TRUE, prob = my_df$Freq)
  return(x)
}

# Use the two functions: 
df_t <- df_s %>% 
  mutate_if(is.numeric, replace_by_mean) %>% 
  mutate_if(is.factor, replace_na_categorical)

df_status(df_s)


my_data_status=df_status(df_s, print_results = F)
my_data_status


df_s_no_na_muscle <- df %>% filter(!is.na(muscle))
df_ohne_kinder <- df_s_no_na_muscle %>% filter(age_by_listung > 18)


df_s <- df_ohne_kinder
gg_miss_var(df_s)
miss_var_summary(df_s) %>% print(n = Inf)

# Ordering data by percentage of NA's
arrange(my_data_status, -p_na) %>% select(variable, q_na, p_na)
# Removing variables with more when 50% of NA's values
vars_to_remove=filter(my_data_status, p_na > 50)  %>% .$variable
vars_to_remove
dataset_2=select(dataset, -one_of(vars_to_remove))
df_status(dataset_2)
gg_miss_var(dataset_2)

gg_miss_var(df_s) 
df_status(df_s)

# Finden Sie die Spalten, die NA Werte enthalten
na_columns <- colSums(is.na(df_s)) > 0

# Erhalten Sie die Namen dieser Spalten
na_column_names <- names(df_s)[na_columns]

# Drucken Sie die Namen der Spalten
print(na_column_names)





# Ordering data by percentage of NA's
my_data_status=df_status(dataset_2, print_results = F)
arrange(my_data_status, -p_na) %>% select(variable, q_na, p_na)


res<-summary(aggr(dataset_2, sortVar=TRUE))$combinations

variables_to_impute <- c("death", "lab_meld_listing", "crea_listing", "height", "weight", "bmi", 
                         "hcc", "child_pugh_score", "inr_listing", "bili_listing", "sodium_listing", 
                         "platelets_listing","ltx")

# Führen Sie die Imputation nur für ausgewählte Variablen durch
tempData_2 <- mice(df_s[, variables_to_impute], m = 5, maxit = 50, method = 'pmm', seed = 500)

# Fertigstellen Sie das Imputationsverfahren und erstellen Sie das endgültige Datensatz
df_t <- complete(tempData_2, 1)
# Angenommen, df_s und df_t haben die gleiche Anzahl von Zeilen
# Angenommen, die Variablen, die Sie ersetzen möchten, sind "var1", "var2", "var3"

variable_names <- names(df_t)

df_s <- df_s %>%
  mutate(across(all_of(variable_names), ~df_t[[cur_column()]], .names = "{.col}"))



nrow(df_s) 
nrow(df_t)

my_data_status_2 <- df_status(df_t)

# Rank best features using information theory

variable_importance <- var_rank_info(df_s, "death")

# Printing results
variable_importance




df_ohne_kinder <- df %>% filter(age_by_listung > 18)

df_status(df_s)
vars <- c( 
          "sex", "age_by_listung", "height", 
          "weight", "bmi", "blood_type",  "hcc", 
          "child_pugh_score", "lab_meld_listing", "inr_listing", "bili_listing", 
          "crea_listing", "sodium_listing", "platelets_listing", "lab_meld_nt", 
          "lap_meld_ltx", "lap_meld_death", "inr_removal", "bili_removal", 
          "crea_removal", "natrium_removal", "platelets_removal", "dialysis_cat", 
          "icu_cat", "days_of_icu", 
          "days_of_ventilation", "amount_of_catecholamine", 
          "exceptional_meld", 
          "bone", "muscle", "sat", "vat", "imat", "eat", 
          "pat", "tat","death")


# Note: please specify the categorical variables in your dataset
catvars <- c("hu_listung", 
             "sex", "blood_type", "hcc", 
             "child_pugh_score", "dialysis_cat", "icu_cat", "ventilation_cat", 
             "catecholamine_cat", "portal_vein_thrombosis" 
)

# Kombinieren Sie die beiden Vektoren
all_vars <- unique(c(vars, catvars))




df_s <- df_ohne_kinder %>% select(all_of(all_vars))



# Angenommen, Ihr DataFrame heißt df und die Zielspalte heißt "Death_on_waiting_list"
# Zuerst definieren wir den Index für die Trainingsdaten
set.seed(123)  # Für reproduzierbare Ergebnisse
trainIndex <- createDataPartition(df_s$death, p = .8, 
                                  list = FALSE, 
                                  times = 1)

# Erstellen Sie den Trainingsdatensatz
df_s <- df_s[ trainIndex, ]

# Erstellen Sie den Testdatensatz
test.data  <- df_s[-trainIndex, ]

# Nun erstellen Sie train.labels und test.labels
train.labels <- df_s$death
test.labels <- test.data$death

# Entfernen Sie die Zielspalte aus den Trainings- und Testdaten
df_s$death <- NULL
test.data$death <- NULL
df_s <- df_s[,-c(2:5,7,8,12,17,19,53,62:64)]
df_s$lab_meld_nt[is.na(df_s$lab_meld_nt)] <- 0
df_s$lap_meld_ltx[is.na(df_s$lap_meld_ltx)] <- 0
df_s$lap_meld_death[is.na(df_s$lap_meld_death)] <- 0
df_s$inr_removal[is.na(df_s$inr_removal)] <- 0
df_s$bili_removal[is.na(df_s$bili_removal)] <- 0
df_s$crea_removal[is.na(df_s$crea_removal)] <- 0
df_s$natrium_removal[is.na(df_s$natrium_removal)] <- 0
df_s$platelets_removal[is.na(df_s$platelets_removal)] <- 0
df_s$days_of_icu[is.na(df_s$days_of_icu)] <- 0
df_s$days_of_ventilation[is.na(df_s$days_of_ventilation)] <- 0
df_s$amount_of_catecholamine[is.na(df_s$amount_of_catecholamine)] <- 0
df_s$days_of_icu[is.na(df_s$days_of_icu)] <- 0
df_s$days_of_dialysis[is.na(df_s$days_of_dialysis)] <- 0
df_s$death_on_the_list_without_ltx[is.na(df_s$death_on_the_list_without_ltx)] <- 0
df_s$survival_days_from_wl_until_death[is.na(df_s$survival_days_from_wl_until_death)] <- 0
df_s$survival_days_until_nt[is.na(df_s$survival_days_until_nt)] <- 0
df_s$survival_days_until_ltx[is.na(df_s$survival_days_until_ltx)] <- 0
df_s$survival_days_from_nt_until_last_contact[is.na(df_s$survival_days_from_nt_until_last_contact)] <- 0


df_s$survival_days_from_wl_until_death[is.na(df_s$survival_days_from_wl_until_death)] <- 0


df_s$death_on_the_list_without_ltx <- as.character(df_s$death_on_the_list_without_ltx)
df_s$death_on_the_list_without_ltx[is.na(df_s$death_on_the_list_without_ltx)] <- "0"
df_s$death_on_the_list_without_ltx <- factor(df_s$death_on_the_list_without_ltx)



df_s$ventilation_cat <- as.character(df_s$ventilation_cat)
df_s$ventilation_cat[is.na(df_s$ventilation_cat)] <- "0"
df_s$ventilation_cat <- factor(df_s$ventilation_cat)

df_s$cause_of_death <- as.character(df_s$cause_of_death)
df_s$cause_of_death[is.na(df_s$cause_of_death)] <- "0"
df_s$cause_of_death <- factor(df_s$cause_of_death)

df_s$catecholamine_cat <- as.character(df_s$catecholamine_cat)
df_s$catecholamine_cat[is.na(df_s$catecholamine_cat)] <- "0"
df_s$catecholamine_cat <- factor(df_s$catecholamine_cat)

df_s$icu_cat <- as.character(df_s$icu_cat)
df_s$icu_cat[is.na(df_s$icu_cat)] <- "0"
df_s$icu_cat <- factor(df_s$icu_cat)

df_s$dialysis_cat <- as.character(df_s$dialysis_cat)
df_s$dialysis_cat[is.na(df_s$dialysis_cat)] <- "0"
df_s$dialysis_cat <- factor(df_s$dialysis_cat)



df_s$platelets_removal[is.na(df_s$platelets_removal)] <- 0
df_s$days_of_icu[is.na(df_s$days_of_icu)] <- 0
df_s$days_of_ventilation[is.na(df_s$days_of_ventilation)] <- 0
df_s$ventilation_cat[is.na(df_s$ventilation_cat)] <- 0
df_s$cause_of_death[is.na(df_s$cause_of_death)] <- 0


df_s$death_on_the_list_without_ltx <- as.character(df_s$death_on_the_list_without_ltx)
df_s$death_on_the_list_without_ltx <- replace(df_s$death_on_the_list_without_ltx, df_s$death_on_the_list_without_ltx == "0", "no")
df_s$death_on_the_list_without_ltx <- as.factor(df_s$death_on_the_list_without_ltx)









#df_s$survival_days_from_wl_until_death <- replace(df_s$survival_days_from_wl_until_death, df_s$survival_days_from_wl_until_death == 0, NA)
df_s$lap_meld_death[is.na(df_s$lap_meld_death)] <- 0
df_s$lap_meld_ltx[is.na(df_s$lap_meld_ltx)] <- 0


df_s$cause_of_death <- as.character(df_s$cause_of_death)
df_s$cause_of_death[is.na(df_s$cause_of_death)] <- "Noch am Leben"
df_s$cause_of_death <- factor(df_s$cause_of_death)


df_s$death_on_the_list_without_ltx <- ifelse(df_s$death_on_the_list_without_ltx == "yes", 2, 1)
df_s$death_on_the_list_without_ltx <- ifelse(df_s$death_on_the_list_without_ltx == 1, 0, 1)



# Geben Sie die Kontrollparameter an
ctrl <- rfeControl(functions=rfFuncs, method="cv", number=10)

# Führen Sie RFE durch
results <- rfe(df_s, train.labels, sizes=c(1:4), rfeControl=ctrl)

# Zeigen Sie die Ergebnisse an
print(results)
print(results$optVariables)
# Plot der Ergebnisse
plot(results, type=c("g", "o"))


# Wählen Sie nur numerische Spalten aus
numeric_vars <- sapply(df_s, is.numeric)

# Berechnen Sie die Korrelationsmatrix nur für die numerischen Spalten
correlation_matrix <- cor(df_s[, numeric_vars])


# Berechnung der Korrelation zwischen den Merkmalen
correlation_matrix <- cor(df_s[,vars])

# Anzeige der Korrelationsmatrix
print(correlation_matrix)

# Erstellt eine Kopie der Korrelationsmatrix
correlation_matrix_filtered <- correlation_matrix

# Setzt alle Werte unter 0.5 auf NA
correlation_matrix_filtered[abs(correlation_matrix_filtered) < 0.5] <- NA

# Druckt die gefilterte Korrelationsmatrix
print(correlation_matrix_filtered)

# Installation des Pakets, falls noch nicht installiert
if (!require(corrplot)) {
  install.packages("corrplot")
}

library(corrplot)

# Darstellung der Korrelationsmatrix als Heatmap
corrplot(correlation_matrix, method = "color")



# Erstellen Sie das Modell
model <- glm(death_on_the_list_without_ltx ~  sat + tat, data = df_s, family = binomial())

# Berechnen Sie die Vorhersagewahrscheinlichkeiten
probabilities <- predict(model, type = "response")

# Berechnen Sie die ROC-Kurve und die AUC
roc_obj <- roc(df_s$death, probabilities)
print(auc(roc_obj))

# Zeichnen Sie die ROC-Kurve
plot(roc_obj, main="ROC Curve for MELD Score")

# Berechnen und hinzufügen AUC auf dem Diagramm
auc(roc_obj)
text(0.7, 0.3, paste("AUC =", round(auc(roc_obj), 2)))


print(length(df_s$death_on_the_list_without_ltx))
print(length(probabilities))

print(nrow(df_s))  # Anzahl der Beobachtungen in df_s
print(length(df_s$death_on_the_list_without_ltx))  # Anzahl der Beobachtungen in der Zielvariable





df_status(df)

df_s$death_on_the_list_without_ltx <- df$death_on_the_list_without_ltx
df_s$survival_days_from_wl_until_death <- df$survival_days_from_wl_until_death





# Stellen Sie sicher, dass das survival Paket installiert und geladen ist
if (!require(survival)) {
  install.packages("survival")
  library(survival)
}

# Fügen Sie eine ID-Spalte zu Ihren Daten hinzu
df_s$id <- 1:nrow(df_s)

# Zuerst erstellen Sie eine künstliche Patienten-ID
df_s$patient_id <- 1:nrow(df_s)

# Dann fügen Sie die Patienten-ID direkt in die Surv()-Funktion ein
surv_obj <- Surv(time = df_s$survival_days_from_wl_until_death, 
                 event = df_s$death_on_the_list_without_ltx, 
                 id = df_s$patient_id)

# Erstellen Sie das Cox-Modell
cox_model <- coxph(surv_obj ~ lap_meld_death + cause_of_death + lap_meld_ltx + sat + tat, data = df_s)





# Erstellen Sie dann Ihr Cox-Modell unter Verwendung der neuen ID-Spalte
cox_model <- coxph(Surv(survival_days_from_wl_until_death, death_on_the_list_without_ltx) ~ lap_meld_death + cause_of_death + lap_meld_ltx + sat + tat + id(id), data = df_s)





# Erstellen Sie ein Surv-Objekt
# Hier gehe ich davon aus, dass Sie eine Spalte namens "time" und eine Spalte namens "status" in Ihrem Datensatz haben,
# die die Zeit bis zum Ereignis und den Ereignisstatus darstellen.
surv_obj <- Surv(df_s$survival_days_from_wl_until_death, df_s$death_on_the_list_without_ltx)

# Erstellen Sie das Cox Proportional Hazards Model
cox_model <- coxph(surv_obj ~ sat + tat, data = df_s)

cox_model <- coxph(Surv(survival_days_from_wl_until_death, death_on_the_list_without_ltx) ~ lap_meld_death + cause_of_death + lap_meld_ltx + sat + tat, data = df_s)


# Drucken Sie eine Zusammenfassung des Modells
summary(cox_model)
vars <- c("sex", "age_by_listung", "height", "weight", "bmi", "blood_type", "hcc", "child_pugh_score", "lab_meld_listing", "inr_listing", "bili_listing", "crea_listing", "sodium_listing", "platelets_listing", "lab_meld_nt", "lap_meld_ltx", "lap_meld_death", "inr_removal", "bili_removal", "crea_removal", "natrium_removal", "platelets_removal", "dialysis_cat", "icu_cat", "days_of_icu", "days_of_ventilation", "amount_of_catecholamine", "exceptional_meld", "bone", "muscle", "sat", "vat", "imat", "eat", "pat", "tat")

# Rest des Codes...


library(pROC)

# Definieren der gewünschten AUC-Schwelle
desired_auc <- 0.8

# Initialisieren der Variablen für die beste Kombination und die dazugehörige AUC
best_combination <- NULL
best_auc <- 0

# Definieren der Anzahl der Versuche
num_attempts <- 2000

# Wiederholen des Vorgangs für die angegebene Anzahl von Versuchen
for (i in 1:num_attempts) {
  # Zufällige Auswahl einer Kombination von bis zu 8 Variablen
  num_vars <- length(vars)
  
  # Überprüfen der Stichprobengröße
  if (num_vars < 8) {
    sample_size <- num_vars
  } else {
    sample_size <- 8
  }
  
  
  
  selected_vars <- sample(vars, num_vars)
  
  # Erstellen des Modells mit den ausgewählten Variablen
  model <- glm(death_on_the_list_without_ltx ~ ., data = df_s[, c(selected_vars, "death_on_the_list_without_ltx")], family = binomial())
  
  # Berechnen der Vorhersagewahrscheinlichkeiten
  probabilities <- predict(model, type = "response")
  
  # Berechnen der ROC-Kurve und der AUC
  roc_obj <- roc(df_s$death_on_the_list_without_ltx, probabilities)
  auc <- auc(roc_obj)
  
  # Überprüfen, ob die AUC die gewünschte Schwelle erreicht oder überschreitet
  if (auc >= desired_auc) {
    # Überprüfen, ob die aktuelle AUC besser ist als die bisher beste AUC
    if (auc > best_auc) {
      best_combination <- vars
      best_auc <- auc
    }
  }
}

# Ausgabe der besten Kombination und der dazugehörigen AUC
print(best_combination)
print(names(best_combination))
print(best_auc)

