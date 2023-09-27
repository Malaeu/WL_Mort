cl <- makePSOCKcluster(cores) 
registerDoParallel(cl)
# Definieren Sie die tuneGrid
tuneGrid <- expand.grid(.mtry = seq(2, 150, 1))

set.seed(123)
rf_model_tuned <- train(death ~ ., 
                        data = trainData, 
                        method = "rf",
                        ntree = 5000, # Sie können die gewünschte Anzahl von Bäumen hier festlegen
                        tuneGrid = tuneGrid,
                        trControl = trainControl(method = "cv", number = 10, 
                                                 search = "grid"))
print(rf_model_tuned)

# Stoppe den Cluster
stopCluster(cl)
importance_scores <- varImp(rf_model_tuned)

rf_importance <- varImp(rf_model_tuned)
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
ggsave(plot, file="cl_adnan/p2_adnan_ohne_lap_meld_death_rf_2.png")
