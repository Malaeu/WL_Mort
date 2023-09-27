vb_df <- data_backup
x <- vb_df |> select(all_of(X))
y <- vb_df |> select(death)
y <- data %>% select(death) |> 
  mutate(across(where(is.factor), ~ as.numeric(.) - 1))
y <- y$death
# 10-Fold Kreuzvalidierung
niter <- 30

# 269 Beobachtungen
n <- 269

# 20% der Daten für das Testset, also etwa 54 Beobachtungen
ntest <- round(n * 0.2)

# Aufruf der Funktion
my.split <- generate.split(niter, n, ntest)

spl <- wilcox.selection.split(x,y,my.split,algo="new",pvalue=T) 

get_top_n_vars_with_stats <- function(spl, n = 10, show_row_names = FALSE, sort_by_median = TRUE) {
  # Durchschnittlicher Rang und P-Wert für jede Variable
  avg_rank <- colMeans(spl$ordering.split)
  avg_pvalue <- colMeans(spl$pvalue.split)
  
  # Weitere Statistiken
  min_rank <- apply(spl$ordering.split, 2, min)
  max_rank <- apply(spl$ordering.split, 2, max)
  q25_rank <- apply(spl$ordering.split, 2, function(x) quantile(x, 0.25))
  q75_rank <- apply(spl$ordering.split, 2, function(x) quantile(x, 0.75))
  median_rank <- apply(spl$ordering.split, 2, median)
  
  # Erstellen des Dataframes mit Durchschnittswerten und Statistiken
  result_df <- data.frame(
    Variable = colnames(spl$pvalue.split),
    Position = 1:length(colnames(spl$pvalue.split)),
    Avg_Rank = avg_rank,
    Min_Rank = min_rank,
    Max_Rank = max_rank,
    Q25_Rank = q25_rank,
    Q75_Rank = q75_rank,
    Median_Rank = median_rank,
    Avg_PValue = avg_pvalue,
    row.names = NULL
  )
  
  # Sortieren des Dataframes
  if (sort_by_median) {
    result_df <- result_df[order(result_df$Median_Rank), ]
  } else {
    result_df <- result_df[order(result_df$Avg_Rank), ]
  }
  
  # Auswahl der Top-n-Variablen
  top_n_vars <- head(result_df, n)
  
  # Anzeigen der Tabelle
  if (show_row_names) {
    kable(top_n_vars)
  } else {
    kable(top_n_vars, row.names = FALSE)
  }
}


get_top_n_vars_with_stats(spl)

get_top_vars <- function(spl, n = 10, show_row_names = FALSE, sort_by_median = TRUE, Var_Name = TRUE) {
  # Durchschnittlicher Rang und P-Wert für jede Variable
  avg_rank <- colMeans(spl$ordering.split)
  avg_pvalue <- colMeans(spl$pvalue.split)
  
  # Weitere Statistiken
  min_rank <- apply(spl$ordering.split, 2, min)
  max_rank <- apply(spl$ordering.split, 2, max)
  q25_rank <- apply(spl$ordering.split, 2, function(x) quantile(x, 0.25))
  q75_rank <- apply(spl$ordering.split, 2, function(x) quantile(x, 0.75))
  median_rank <- apply(spl$ordering.split, 2, median)
  
  # Erstellen des Dataframes mit Durchschnittswerten und Statistiken
  result_df <- data.frame(
    Variable = colnames(spl$pvalue.split),
    Position = 1:length(colnames(spl$pvalue.split)),
    Avg_Rank = avg_rank,
    Min_Rank = min_rank,
    Max_Rank = max_rank,
    Q25_Rank = q25_rank,
    Q75_Rank = q75_rank,
    Median_Rank = median_rank,
    Avg_PValue = avg_pvalue,
    row.names = NULL
  )
  
  # Sortieren des Dataframes
  if (sort_by_median) {
    result_df <- result_df[order(result_df$Median_Rank), ]
  } else {
    result_df <- result_df[order(result_df$Avg_Rank), ]
  }
  
  # Auswahl der Top-n-Variablen
  top_n_vars <- head(result_df, n)
  
  # Anzeigen der Tabelle
  if (show_row_names) {
    kable_output <- kable(top_n_vars)
  } else {
    kable_output <- kable(top_n_vars, row.names = FALSE)
  }
  
  # Erstellen des Vektors mit den Namen oder Positionen der Top-Variablen
  if (Var_Name) {
    top_var_vector <- top_n_vars$Variable
  } else {
    top_var_vector <- top_n_vars$Position
  }
  
  return(list("Table" = kable_output, "Top_Variables" = top_var_vector))
}

get_top_vars(spl,n=15
             )

fun_modeling_result <- var_rank_info(vb_df, "death")
fun_modeling_result_filter <-
  fun_modeling_result %>% select(var, gr) %>% filter(gr > 0.074) %>% arrange(desc(gr))
