# Erhalten Sie die besten Modelle mit get_best_models
best_models <- get_best_models(all_combination_of_vars, aic_values, x)

best_models$models <- lapply(best_models$models, function(x) {
  unlist(strsplit(x, ", "))
})

best_models_trained <- lapply(best_models$models, function(vars) {
  formula <- as.formula(paste("death ~", paste(vars, collapse = " + ")))
  model <- glm(formula, family = binomial, data = df_train)
  return(model)
})

# Machen Sie Vorhersagen mit den trainierten Modellen
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

# Zusammenfügen der AIC-Werte und Leistungsmetriken in einen Dataframe
results <- data.frame(aic_values = aic_values, performance_metrics = do.call(rbind, performance_metrics))

x <- 10

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

best_models <- get_best_models(all_combination_of_vars, aic_values, x, df_train, df_test)
