library(survival)
library(survminer)

df <- df_s_nonzero
df_s_nonzero <- df
df_s_nonzero <- df_s_nonzero_indx_voll
df_s_nonzero$death <- ifelse(df_s_nonzero$death == 'yes', 1, 0)


survival_model <- survfit(Surv(Time, death) ~ child_pugh_score, data = df_s_nonzero)
ggsurvplot(survival_model, data = df_s_nonzero, risk.table = TRUE)
summary(survival_model)


# Assume 'time' is the survival time, 'status' is the censoring indicator (1 = event, 0 = censored), 'sex' is the gender
censored_females <- sum(df_s_nonzero$death[df_s_nonzero$sex == "female"] == 0)
total_females <- sum(df_s_nonzero$sex == "female")
prop_censored_females <- censored_females / total_females

survival_model <- survfit(Surv(Time, death) ~ muscle, data = df_s_nonzero)
ggsurvplot(survival_model, data = df_s_nonzero, risk.table = TRUE)
summary(survival_model)
df_status(df_s_nonzero)





# Erstellen einer Liste der Variablen
variables <- c('bone', 'muscle', 'sat', 'vat', 'imat', 'eat', 'pat', 'tat',"weight","height")

# Normalisieren der Variablen
for (variable in variables) {
  df_s_nonzero[paste(variable, 'normalized', sep = '_')] <- ifelse(df_s_nonzero$sex == 'male',
                                                                   df_s_nonzero[[variable]] / mean(df_s_nonzero[df_s_nonzero$sex == 'male', variable], na.rm = TRUE),
                                                                   df_s_nonzero[[variable]] / mean(df_s_nonzero[df_s_nonzero$sex == 'female', variable], na.rm = TRUE)
  )
}

# Überprüfen der normalisierten Variablen
head(df_s_nonzero[grep('_normalized', names(df_s_nonzero))])


# Die Berechnungen für jeden Index
index_calculations <- list(
  'bone_fat_index' = "bone / tat",
  'paracardial_fat_index' = "pat / bone",
  'epicardial_fat_index' = "eat / bone",
  
  'muscle_fat_index' = "muscle / tat",
  'muscle_to_subcutaneous_fat_index' = "muscle / sat",
  'bone_to_subcutaneous_fat_index' = "bone / sat",
  'bone_to_visceral_fat_index' = "bone / vat",
  'muscle_to_paracardial_fat_index' = "muscle / pat",
  
  'myosteatotic_fat_index_extended' = "imat / (tat + bone) * 100",
  'abdominal_fat_index_extended' = "vat / (sat + bone) * 100",
  'sarcopenia_index_extended' = "muscle / (bone + tat) * 100",
  'paracardial_to_visceral_fat_index' = "pat / vat",
  'epicardial_to_visceral_fat_index' = "eat / vat",
  'bone_muscle_index' = "bone / muscle",
  'visceral_to_subcutaneous_fat_index_extended' = "vat / (sat + muscle) * 100",
  'cardiac_fat_index' = "(pat + eat) / bone",
  'cardiac_fat_to_muscle_index' = "(pat + eat) / muscle",
  'subcutaneous_to_visceral_fat_index' = "sat / vat"
)

# Die Berechnungen für jeden Index mit normalisierten Variablen
index_calculations <- list(
  'bone_fat_index' = "bone_normalized / tat_normalized",
  'paracardial_fat_index' = "pat_normalized / bone_normalized",
  'epicardial_fat_index' = "eat_normalized / bone_normalized",
  
  'muscle_fat_index' = "muscle_normalized / tat_normalized",
  'muscle_to_subcutaneous_fat_index' = "muscle_normalized / sat_normalized",
  'bone_to_subcutaneous_fat_index' = "bone_normalized / sat_normalized",
  'bone_to_visceral_fat_index' = "bone_normalized / vat_normalized",
  'muscle_to_paracardial_fat_index' = "muscle_normalized / pat_normalized",
  
  'myosteatotic_fat_index_extended' = "imat_normalized / (tat_normalized + bone_normalized) * 100",
  'abdominal_fat_index_extended' = "vat_normalized / (sat_normalized + bone_normalized) * 100",
  'sarcopenia_index_extended' = "muscle_normalized / (bone_normalized + tat_normalized) * 100",
  
  'paracardial_to_visceral_fat_index' = "pat_normalized / vat_normalized",
  'epicardial_to_visceral_fat_index' = "eat_normalized / vat_normalized",
  
  'bone_muscle_index' = "bone_normalized / muscle_normalized",
  
  'visceral_to_subcutaneous_fat_index_extended' = "vat_normalized / (sat_normalized + muscle_normalized) * 100",
  
  'cardiac_fat_index' = "(pat_normalized + eat_normalized) / bone_normalized",
  'cardiac_fat_to_muscle_index' = "(pat_normalized + eat_normalized) / muscle_normalized",
  
  'subcutaneous_to_visceral_fat_index' = "sat_normalized / vat_normalized",
  
  'sarcopenia_index' = "muscle_normalized / bone_normalized",
  'myosteatotic_fat_index' = "(imat_normalized / tat_normalized) * 100",
  'abdominal_fat_index' = "vat_normalized / sat_normalized",
  
  'sarcopenia_index_extended' = "muscle_normalized / (bone_normalized + tat_normalized) * 100",
  'myosteatotic_fat_index_extended' = "imat_normalized / (tat_normalized + bone_normalized) * 100",
  'abdominal_fat_index_extended' = "vat_normalized / (sat_normalized + bone_normalized) * 100"
)



# Erstellen Sie die Indexvariablen und füllen Sie sie mit den berechneten Werten
for (index in names(index_calculations)) {
  df_s_nonzero[[index]] <- with(df_s_nonzero, eval(parse(text = index_calculations[[index]])))
}

# Normalisieren Sie die Indexvariablen
for (index in names(index_calculations)) {
  df_s_nonzero[[paste0(index, "_normalized")]] <- df_s_nonzero[[index]] / max(df_s_nonzero[[index]], na.rm = TRUE)
}





# Sie müssen "weight", "height", "bmi", "bilirubin" und "creatinin" ersetzen, wenn Ihre Daten diese Variablen unter anderen Namen verwenden.
additional_index_calculations_normalized <- list(
  'log_sarcopenia_index' = "log(muscle_normalized / bone_normalized)",
  'exp_myosteatotic_fat_index' = "exp(imat_normalized / tat_normalized * 100)",
  'combined_fat_muscle_index' = "log(muscle_normalized) * (sat_normalized / vat_normalized)",
  'hepatic_function_index' = "(crea_listing * log(bili_listing)) / weight_normalized",
  'visceral_fat_adjusted_creatinin_index' = "crea_listing / log(vat_normalized)",
  'combined_muscle_bone_index' = "exp(muscle_normalized) / bone_normalized^2",
  'adjusted_abdominal_fat_index' = "log(vat_normalized / sat_normalized)",
  'exp_height_weight_index' = "exp(weight_normalized)",
  'adjusted_hepatic_function_index' = "log(crea_listing * bili_listing) / tat_normalized",
  'subcutaneous_to_visceral_fat_ratio' = "exp(sat_normalized) / log(vat_normalized)",
  'log_epicardial_paracardial_fat_index' = "log(eat_normalized / pat_normalized)",
  'weight_adjusted_bilirubin_index' = "log(bili_listing) / weight_normalized",
  'exp_sarcopenia_index' = "exp(muscle_normalized / bone_normalized)",
  'creatinin_bilirubin_ratio' = "exp(crea_listing) / log(bili_listing)",
  'adjusted_myosteatotic_fat_index' = "log(imat_normalized / tat_normalized * 100)",
  'muscle_adjusted_hepatic_function_index' = "log(crea_listing * bili_listing) / muscle_normalized",
  'weight_adjusted_epicardial_paracardial_fat_index' = "exp(eat_normalized / pat_normalized) / weight_normalized",
  'total_fat_adjusted_abdominal_fat_index' = "(vat_normalized / sat_normalized) / log(tat_normalized)",
  'height_adjusted_sarcopenia_index' = "exp(muscle_normalized / bone_normalized)"
)

# Erstellen Sie die Indexvariablen und füllen Sie sie mit den berechneten Werten
for (index in names(additional_index_calculations_normalized)) {
  df_s_nonzero[[index]] <- with(df_s_nonzero, eval(parse(text = additional_index_calculations_normalized[[index]])))
}

# Normalisieren Sie die Indexvariablen
for (index in names(additional_index_calculations_normalized)) {
  df_s_nonzero[[paste0(index, "_normalized")]] <- df_s_nonzero[[index]] / max(df_s_nonzero[[index]], na.rm = TRUE)
}


# Namen der Variablen von der 55. bis zur 144. Spalte
var_names <- colnames(df_s_nonzero)[55:144]

# Schleife durch die Variablen
for (var in var_names) {
  df_s_nonzero[[paste0(var, "_category")]] <- ifelse(df_s_nonzero[[var]] > median(df_s_nonzero[[var]], na.rm = TRUE), "Über_Median", "Unter_Median")
}






# Definieren Sie ein Überlebensobjekt
surv_object <- Surv(df_s_nonzero$Time, df_s_nonzero$death)

# Bestimmen Sie den optimalen Cutpoint für die Unterteilung in zwei Gruppen
cutpoint1 <- surv_cutpoint(df_s_nonzero, time = "Time", event = "death", variables = c("lab_meld_listing"))
summary(cutpoint1)
# 2. Plot cutpoint for DEPDC1
# palette = "npg" (nature publishing group), see ?ggpubr::ggpar
plot(cutpoint1, "lab_meld_listing", palette = "npg")
# 3. Categorize variables
res.cat <- surv_categorize(cutpoint1)
head(res.cat)
# 4. Fit survival curves and visualize

fit <- survfit(Surv(Time, death) ~lab_meld_listing, data = res.cat)
ggsurvplot(fit, risk.table = TRUE, conf.int = TRUE)

# Fügen Sie eine neue Variable hinzu, die auf diesem Cutpoint basiert
df_s_nonzero$meld_group1 <- ifelse(df_s_nonzero$lab_meld_listing <= cutpoint1$cutpoint$cutpoint, "Low", "Intermediär/High")

# Führen Sie das Verfahren erneut für die Patienten in der "Intermediär/High" Gruppe durch
df_s_nonzero_high <- df_s_nonzero[df_s_nonzero$meld_group1 == "Intermediär/High",]
cutpoint2 <- surv_cutpoint(df_s_nonzero_high, time = "Time", event = "death", variables = c("lab_meld_listing"))

# Fügen Sie eine neue Variable hinzu, die auf diesem zweiten Cutpoint basiert
df_s_nonzero$meld_group2 <- ifelse(df_s_nonzero$lab_meld_listing <= cutpoint2$cutpoint$cutpoint, "Intermediär", "High")

# Kombinieren Sie die beiden Variablen, um die endgültige MELD Gruppe zu erstellen
df_s_nonzero$meld_group <- ifelse(df_s_nonzero$meld_group1 == "Low", "Low", df_s_nonzero$meld_group2)

# Löschen Sie die temporären Variablen
df_s_nonzero$meld_group1 <- NULL
df_s_nonzero$meld_group2 <- NULL

variable <- names(df_s_nonzero)
var_list <- variable[grep('_category', names(df_s_nonzero))]


# Suffix "_category" entfernen
var_list <- sub("_category", "", var_list)

# Namen der Variablen, die entfernt werden sollen
remove_vars <- c("creatinin_bilirubin_ratio_normalized")

# Aktualisieren Sie die var_list, indem Sie die zu entfernenden Variablen ausschließen
var_list <- setdiff(var_list, remove_vars)

# Gesamtzahl der Variablen
total_vars <- length(var_list)

# Initialisieren Sie eine neue Variable, um den gerundeten Cutpoint zu speichern
df_s_nonzero$cutpoint_rounded <- NA
# Initialisieren Sie eine neue Variable, um die Differenz zwischen dem optimalen Cutpoint und dem Median zu speichern
df_s_nonzero$cutpoint_median_diff <- NA

# Für jede Variable den optimalen Cutpoint berechnen
for (i in seq_along(var_list)) {
  # Formel erstellen
  formula <- as.formula(paste("Surv(Time, death) ~", var_list[i]))
  
  # Berechnen Sie den optimalen Cutpoint
  mstat <- maxstat.test(formula, 
                        data=df_s_nonzero, 
                        smethod="LogRank", 
                        pmethod="exactGauss", 
                        abseps=0.01)
  
  # Den Cutpoint runden und speichern
  cutpoint_rounded <- round(mstat$estimate, 2)
  df_s_nonzero$cutpoint_rounded[i] <- cutpoint_rounded
  # Berechnen Sie die Differenz zum Median und speichern Sie diese
  median_val <- round(median(df_s_nonzero[[var_list[i]]], na.rm = TRUE),2)
  df_s_nonzero$cutpoint_median_diff[i] <- cutpoint_rounded - median_val
  # Den Cutpoint anzeigen
  print(paste("Variable Nummer", i, "von", total_vars, ":", var_list[i], 
              "Optimaler Cutpoint:", cutpoint_rounded))
  
  # Den Cutpoint anwenden
  df_s_nonzero[[paste0(var_list[i], "_cat")]] <- ifelse(df_s_nonzero[[var_list[i]]] > cutpoint_rounded, "Über_Cutpoint", "Unter_Cutpoint")
}

# Vergleichen Sie den gerundeten Cutpoint mit dem Median
df_s_nonzero$cutpoint_vs_median <- ifelse(df_s_nonzero$cutpoint_rounded > median(df_s_nonzero$cutpoint_rounded, na.rm = TRUE), "Über_Median", "Unter_Median")
# Überprüfen Sie die Differenzen
print(df_s_nonzero$cutpoint_median_diff)


library(survival)
library(survminer)

# Wählen Sie eine Variable aus
var <- "abdominal_fat_index"

# Fitten Sie das Überlebensmodell für die erste Kategorie
fit1 <- survfit(Surv(Time, death) ~ df_s_nonzero[[paste0(var, "_category")]], data = df_s_nonzero)

# Fitten Sie das Überlebensmodell für die zweite Kategorie
fit2 <- survfit(Surv(Time, death) ~ df_s_nonzero[[paste0(var, "_cat")]], data = df_s_nonzero)

# Erstellen Sie das Plot-Fenster
plot(fit1, col = "red", main = "Vergleich der Überlebenskurven", xlab = "Zeit", ylab = "Überlebenswahrscheinlichkeit")

# Fügen Sie die zweite Überlebenskurve hinzu
lines(fit2, col = "blue")

# Fügen Sie eine Legende hinzu
legend("topright", legend = c(paste(var, "_category"), paste(var, "_cat")), col = c("red", "blue"), lty = 1)


# Teilen Sie das Grafikfenster in zwei Bereiche auf
par(mfrow = c(1, 2))

# Schleife durch die Variablen in var_list
for (var in var_list) {
  # Fitten Sie das Überlebensmodell für die erste Kategorie
  fit1 <- survfit(Surv(Time, death) ~ df_s_nonzero[[paste0(var, "_category")]], data = df_s_nonzero)
  
  # Fitten Sie das Überlebensmodell für die zweite Kategorie
  fit2 <- survfit(Surv(Time, death) ~ df_s_nonzero[[paste0(var, "_cat")]], data = df_s_nonzero)
  
  # Plotten Sie die Überlebenskurve für die erste Kategorie
  plot(fit1, col = "red", main = paste(var, "_category"), xlab = "Zeit", ylab = "Überlebenswahrscheinlichkeit")
  
  # Plotten Sie die Überlebenskurve für die zweite Kategorie
  plot(fit2, col = "blue", main = paste(var, "_cat"), xlab = "Zeit", ylab = "Überlebenswahrscheinlichkeit")
  
  # Frage nach der Weiterfahrt
  next_var <- readline(prompt="Möchten Sie zur nächsten Variable gehen? (Y/N) ")
  
  # Brechen Sie die Schleife ab, wenn die Antwort 'N' ist
  if (next_var == "N") {
    break
  }
}


# Überprüfen der normalisierten Variablen
head(df_s_nonzero[grep('_normalized', names(df_s_nonzero))])
head(df_s_nonzero[grep('_category', names(df_s_nonzero))])

saveRDS(df_s_nonzero, "df_s_nonzero_indx_voll.rds")
