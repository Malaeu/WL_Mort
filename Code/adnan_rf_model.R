


library(caret)
set.seed(123)
trainIndex <- createDataPartition(df_selected_vars_train_df$death, p = 0.8, list = FALSE)
trainData <- df_selected_vars_train_df[trainIndex, ]
testData <- df_selected_vars_train_df[-trainIndex, ]
svm_fit <- train(death ~ ., data = trainData, method = "svmRadial", tuneLength = 10)
varImportance <- varImp(svm_fit, scale = FALSE, useModel=FALSE)
print(varImportance)
library(pROC)
predictions <- predict(svm_fit, testData)
predictions <- as.numeric(predictions)
roc_auc <- auc(roc(testData$death, predictions))
print(roc_auc)

rf_model <- train(death ~ ., data = trainData, method = "rf")
rf_importance <- varImp(rf_model)
print(rf_importance)


# Erzeugen Sie eine Variable importance Tabelle
imp_table <- data.frame(Variable = row.names(rf_importance$importance), 
                        Importance = rf_importance$importance$Overall) %>%
  # Nur Variablen mit Wichtigkeit über 70 auswählen
  filter(Importance > 70) %>%
  # Farben basierend auf der Wichtigkeit zuweisen
  mutate(Color = case_when(
    Importance > 80 ~ 'blue',
    Importance > 75 ~ 'green',
    TRUE ~ 'yellow')) %>%
  # In absteigender Reihenfolge der Wichtigkeit sortieren
  arrange(desc(Importance))

# Plot erstellen
plot <- ggplot(imp_table, aes(x = reorder(Variable, Importance), y = Importance, fill = Color)) +
  geom_col() +
  scale_fill_identity() +
  coord_flip() +
  theme_minimal() +
  labs(x = "Variable", y = "Importance", fill = "Importance", title = "Variable Importance")

# Plot zeigen
print(plot)
ggsave(plot, file="cl_adnan/p2_adnan_ohne_lap_meld_death_rf.png")
