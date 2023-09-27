saveRDS(df, file = "df_30_07_2023.rds")
write_sav(df_selected_vars_borut, "df_selected_vars_borut.sav")
write.xlsx(df_cleaned, "df_cleaned.xlsx")
save.image(file = "df_selected_vars_borut.RData")
rm(list = ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  agua,                     # Data manipulation and visualization
  Boruta,                  # Feature selection
  DataExplorer,            # EDA (Exploratory Data Analysis)
  devtools,                # Package development tools
  discrim,                 # Discriminant analysis
  dlookr,                  # EDA (Exploratory Data Analysis)
  doParallel,              # Parallel computing
  e1071,                   # Machine learning algorithms
  entropy,                 # Entropy-based methods
  finalfit,                # Model fitting and reporting
  flextable,               # Beautifying tables
  funModeling,             # EDA (Exploratory Data Analysis)
  ggpubr,                  # Publication-ready data visualization in R
  ggstatsplot,             # Publication-ready visualizations with statistical details
  gridExtra,               # Grid graphics
  gt,                      # Table creation
  gtsummary,               # Publication-ready summary tables
  haven,                   # Import/export SPSS, SAS, and Stata files
  Hmisc,                   # Data manipulation and summary statistics
  ISLR,                    # Wage dataset
  janitor,                 # Data cleaning
  kableExtra,              # Table formatting
  kernlab,                 # Kernel-based machine learning
  knitr,                   # Dynamic report generation
  listviewer,              # Interactive viewing of lists
  lubridate,               # Date and time manipulation
  mRMRe,                   # Multivariate feature selection
  minerva,                 # Maximal information-based nonparametric exploration
  moments,                 # Skewness, kurtosis, and related tests
  naivebayes,              # Naive Bayes classifier
  openxlsx,                # Read and write Excel files
  opendatatoronto,         # Access to Toronto's open data
  pandoc,                  # Document conversion
  patchwork,               # Combine multiple ggplot2 plots
  pdftools,                # PDF manipulation
  PerformanceAnalytics,    # Econometrics for performance and risk analysis
  pointblank,              # Data validation and reporting
  psych,                   # Psychological research: descriptive stats, FA, PCA, etc.
  randomForestExplainer,   # Random forest interpretation
  readxl,                  # Read Excel files
  reshape2,                # Data reshaping
  reticulate,              # Interface to Python
  rio,                     # Data import/export
  ROCR,                    # Visualizing classifier performance
  rminer,                  # Data mining
  scales,                  # Visualization scaling
  scorecard,               # Credit scorecard development
  skimr,                   # Summary statistics
  SmartEDA,                # EDA (Exploratory Data Analysis)
  stringi,                 # String processing
  stringr,                 # String manipulation
  stringx,                 # String manipulation
  survminer,               # Survival analysis visualization
  survival,                # Survival analysis
  testthat,                # Unit testing
  themis,                  # Dealing with imbalanced data
  tidymodels,              # Modeling and machine learning
  tidyverse,               # Data manipulation and visualization
  validate,                # Data validation
  vip,                      # Variable importance
  sjPlot
)



df <- df_s_nonzero_indx_voll
df <- df_cleaned
df <- df %>% janitor::clean_names()
write.csv(df, "df_07.csv", row.names = FALSE)

tidymodels_prefer() 

old_max <- getOption("max.print")
options(max.print = 5000)  # replace 'n' with the desired number. In this case, it can be 999999 or similar.

df_status(df)

df <- df %>%
  relocate(c(time, death), .after = ...1)
# Erhalte alle Faktorvariablennamen
factor_vars <- names(df)[sapply(df, is.factor)]

# Verschiebe alle Faktorvariablen nach 'death'
df <- df %>% relocate(all_of(factor_vars), .after = "death")

# Erhalte alle Variablennamen, die "days" enthalten
days_vars <- grep("days", names(df), value = TRUE)

# Verschiebe alle "days"-Variablen nach 'time'
df <- df %>% relocate(all_of(days_vars), .after = "time")



df <- df %>%
  mutate(across(where(is.character), as.factor)) %>% 
  mutate(death = ifelse(death == "yes", 1, 0)) 

# Find variables in the dataframe with only one level (excluding NA values)
to_drop <- sapply(df, function(x) length(unique(na.omit(x))) < 2)
drop_count <- sum(to_drop)
print(drop_count)
# Drop these variables
df <- df[, !to_drop]

# Calculate the number of Patients that are NT
nt_count <- sum(df$survival_days_until_nt != 0)
print(nt_count)

# Calculate the number of Patients who are died on WL
death_on_wl_count <- sum(df$survival_days_from_wl_until_death != 0)
print(death_on_wl_count)

# Calculate the number of patients who died on the list without LTX
# Umwandlung in numerische Variable
# Umwandlung der Faktorstufen in numerische Werte
df$death_on_the_list_without_ltx_numeric <- ifelse(df$death_on_the_list_without_ltx == "no", 0, 
                                                   ifelse(df$death_on_the_list_without_ltx == "yes", 1, NA))

dead_without_ltx_count <- sum(df$death_on_the_list_without_ltx_numeric)
# Print the result
print(dead_without_ltx_count)


df$survival_days_until_ltx

sum(df$age_by_listung > 18)

non_zero_non_na_count <- sum(df$acsn_nr != 0 & !is.na(df$acsn_nr))

# Print the result
print(non_zero_non_na_count)
non_zero_non_na_count <- sum(df$date_of_ct != 0 & !is.na(df$date_of_ct))
print(non_zero_non_na_count)

# Removing specified columns
df <-
  df[,!(
    names(df) %in% c(
      'patient_number',
      'acsn_nr',
      'ct_accession_nr_2_intern',
      'lap_meld_death',
      'inr_removal',
      'bili_removal',
      'crea_removal',
      'natrium_removal',
      'platelets_removal',
      'ct_accession_nr_extern',
      "date_of_ct",
      "date_of_nt",
      "last_contact",
      "date_of_ltx",
      "date_of_birth",
      "date_of_wl",
      "body_parts",
      "date_of_death"
    )
  )]

str(df)

df$time <-
  rowSums(df[, c(
    "survival_days_until_nt",
    "survival_days_from_nt_until_last_contact",
    "survival_days_until_ltx",
    "survival_days_from_wl_until_death"
  )], na.rm = TRUE)

head(df$time)


# Subset von df_cleaned wo 'time' gleich 0 ist
subset_df <- df[df$time == 0,]

# Zeige die 'death' Werte dieses Subsets
subset_df$death

# Erstelle einen Vektor mit den Zeilennummern, die entfernt werden sollen
rows_to_remove <- which(df$time == 0)

# Entferne diese Zeilen aus df_cleaned
df <- df[-rows_to_remove, ]

df %>% select(
  time,
  survival_days_until_nt,
  survival_days_from_nt_until_last_contact,
  survival_days_until_ltx,
  survival_days_from_wl_until_death
) %>% head()

df %>% select(time,
              survival_days_until_ltx,
              survival_days_from_wl_until_death,
              ltx,
              death)

df %>% select(was_transplanted,ltx)

df <- df %>% select(-ltx)
df <- df %>% select(-...1)
df <- df %>% filter(age_by_listung>=18) %>% select(everything())

df$time_till_nt <-
  ifelse(
    df$status_changed_to_nt == 1,
    df$survival_days_until_nt,
    ifelse(df$status_changed_to_nt == 0, df$time, NA)
  )

# Dieser Code gibt TRUE zurück, wenn es mindestens einen NA-Wert in
# df$time_till_nt gibt, und FALSE, wenn es keine NA-Werte gibt.

any(is.na(df$time_till_nt))

# In diesem Code wird ifelse(df$status_changed_to_nt == 1 &
# df$had_contact_after_nt == 1, df$survival_days_from_nt_until_last_contact,
# ifelse(df$status_changed_to_nt == 0, df$time, NA)) verwendet, um die Werte für
# die neue Variable time_after_nt zu bestimmen. Wenn status_changed_to_nt gleich
# 1 ist und had_contact_after_nt auch gleich 1 ist, wird der Wert aus
# survival_days_from_nt_until_last_contact übernommen. Wenn status_changed_to_nt
# gleich 0 ist, wird der Wert aus der Variable time übernommen. Wenn
# status_changed_to_nt einen anderen Wert als 0 oder 1 hat oder
# had_contact_after_nt nicht gleich 1 ist, wird time_after_nt auf NA gesetzt.

intersection_df <- df[df$status_changed_to_nt == 0 & df$death_on_the_list_without_ltx == 1, ]
nrow(intersection_df)

df$time_after_nt <-
  ifelse(
    df$status_changed_to_nt == 1 &
      df$had_contact_after_nt == 1,
    df$survival_days_from_nt_until_last_contact,
  ifelse(
    df$status_changed_to_nt == 0 &
      df$death_on_the_list_without_ltx== "no", df$time, NA))

any(is.na(df$time_after_nt))
intersection_df <- df[df$hu_listung == "yes" & df$was_transplanted == 1, ]
nrow(intersection_df)

non_zero_non_na_count <- sum(df$bone != 0 & !is.na(df$bone))
print(non_zero_non_na_count)

df %>% filter (status_changed_to_nt==1) %>% select(had_contact_after_nt) %>% sum()
df %>% select(survival_days_from_nt_until_last_contact,was_transplanted)


diagnose(df) %>% flextable()
diagnose_category(df) %>% flextable()
diagnose_numeric(df) %>% flextable()
diagnose_outlier(df) %>% flextable()
col_outlier <- find_outliers(df, index = F)

df%>% 
  select(col_outlier) %>% 
  plot_outlier()

plot_na_pareto(df)

describe(adnan_df) %>% flextable()

df %>% 
  target_by(death) %>%      
  relate(hu_listung) %>% 
  plot()



df %>% 
  target_by(bone) %>%      
  relate(death_on_the_list_without_ltx) %>% 
  sjPlot::tab_model()


# “rpart” : Recursive Partitioning and Regression Trees
plot(imputate_na(df, bone, time, method = "rpart"))+
  theme_classic()+
  theme(legend.position = "top")

# “mice” : Multivariate Imputation by Chained Equations

plot(imputate_na(df, bone, time, method = "mice", seed = 999))+
  theme_minimal()+
  theme(legend.position = "top")


