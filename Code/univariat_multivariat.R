library(tidyverse)
library(tidymodels)# for the parsnip package, along with the rest of tidymodels
library(msm)
library(ftExtra)
  

# Helper packages
library(readr)       # for importing data
library(broom.mixed) # for converting bayesian models to tidy tibbles
library(dotwhisker)  # for visualizing regression results

# Laden der benötigten Pakete
library(dplyr)

# Angenommen, Ihre Daten sind im Dataframe 'data' gespeichert
# Ersetzen Sie 'data' durch den Namen Ihres Dataframes

# Deskriptive Statistiken für numerische Variablen
num_vars <- c("age_by_listung", "height", "weight", "bmi", "lab_meld_listing",
                  "inr_listing", "bili_listing", "crea_listing", "sodium_listing", "platelets_listing",
                  "lab_meld_nt", "lap_meld_ltx", "lap_meld_death", "inr_removal", "bili_removal",
                  "crea_removal", "natrium_removal", "platelets_removal", "days_of_dialysis", "days_of_icu",
                  "days_of_ventilation", "amount_of_catecholamine", "bone", "muscle", "sat",
                  "vat", "imat", "eat", "pat", "tat")

numeric_summary <- df_s_nonzero_prep %>%
  select(all_of(num_vars)) %>%
  summarise_all(list(min = min, mean = mean, median = median, max = max, sd = sd))

print(numeric_summary)

# Häufigkeitstabellen für kategorische Variablen
cat_vars <- c("hu_listung", "sex", "hcc",
                       "dialysis_cat", "icu_cat", "ventilation_cat", "catecholamine_cat"
                      )

all_covariates <- c(cat_vars, num_vars)

for (var in cat_vars) {
  cat_table <- df_s_nonzero_prep %>%
    group_by(df_s_nonzero_prep[[var]]) %>%
    summarise(count = n(), percentage = (count / nrow(df_s_nonzero_prep)) * 100)
  
  print(paste("Häufigkeitstabelle für:", var))
  print(cat_table)
}



table2 <-
  tbl_summary(
    df_s_nonzero_prep,
    include = c(num_vars,cat_vars),
    by = death, # split table by group
    missing = "no" # don't list missing data separately
    
  ) %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Death on waiting list**") %>%
  modify_footnote(
    all_stat_cols() ~ "Median (IQR) or Frequency (%)"
  ) %>%
  modify_caption("**Table 1. Patient`s Characteristics**") %>%
  bold_labels()


kbl(table2)
print(table2, printToggle = T) %>% 
  kable_styling(bootstrap_options = "striped", full_width = F)



table2
show_header_names(table2)  

# Installieren und Laden der benötigten Pakete
if (!requireNamespace("gt", quietly = TRUE)) {
  install.packages("gt")
}
library(gt)
# Erstellen Sie Beispieldaten für numerische Zusammenfassung und kategoriale Tabelle
numeric_summary <- data.frame(
  Variable = c("Age", "Weight", "Height"),
  Mean = c(35.2, 70.5, 175.3),
  SD = c(8.1, 10.2, 6.3),
  Min = c(20, 50, 160),
  Max = c(50, 90, 190)
)

cat_table <- data.frame(
  Variable = c("Gender", "Smoker", "Disease"),
  Category = c("Male", "Yes", "Positive"),
  Count = c(100, 50, 30),
  Percentage = c(60, 30, 20)
)

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
numeric_summary_table <- format_lancet_table(res_reorder)
numeric_summary_table

# Formatieren und Anzeigen der kategorischen Tabelle
cat_table_lancet <- format_lancet_table(cat_table)
cat_table_lancet


table2 %>%
  kbl() %>%
  kable_styling()

table2 %>%
  kbl(caption = "Recreating booktabs style table") %>%
  kable_classic(full_width = F, html_font = "Cambria")
kbl(table2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


table2 %>%
  head %>%
  as_flextable() %>%
  span_header()
df_s_nonzero_prep %>% tbl_summary()
df_s_nonzero_prep %>%
  tbl_summary(by = death) %>%
  add_p()


# Funktion zum Entfernen von Level 0
remove_zero_level <- function(x) {
  if (is.factor(x) && "0" %in% levels(x)) {
    x <- droplevels(x, exclude = "0")
  }
  return(x)
}

# Anwenden der Funktion auf alle Variablen im Dataframe
df_s_nonzero_prep <- data.frame(lapply(df_s_nonzero_prep, remove_zero_level))

library(survival)


#Nehmen wir an, Ihre Daten befinden sich in einem Dataframe namens `mydata`. Sie können das Cox Proportional Hazards Model für jede kovariate einzeln anpassen. Hier ist ein Beispiel-Code, der dies für alle numerischen und kategorischen Kovariaten tut:
  

# Erstellen Sie eine Liste mit den Namen der Kovariaten
covariates_numeric <- c("age_by_listung", "height", "weight", "bmi", "lab_meld_listing", ...)
covariates_categorical <- c("hu_listung", "sex", "blood_type", "primary_diagnosis", "hcc", ...)

# Kombinieren Sie die numerischen und kategorischen Kovariaten
all_covariates <- c(cat_vars, num_vars)

# Erstellen Sie eine leere Liste, um die Ergebnisse der univariaten Analysen zu speichern
univariate_results <- list()

# Führen Sie die univariate Analyse für jede Kovariate durch
for (covariate in all_covariates) {
  # Erstellen Sie das Cox Proportional Hazards Model
  cox_model <- coxph(Surv(Time, death) ~ df_s_nonzero_prep[[covariate]], data = df_s_nonzero_prep)
  
  # Speichern Sie die Ergebnisse in der Liste
  univariate_results[[covariate]] <- summary(cox_model)
}

# Zeigen Sie die Ergebnisse

covariates <- c("age", "sex",  "ph.karno", "ph.ecog", "wt.loss")
univ_formulas <- sapply(all_covariates,
                        function(x) as.formula(paste('Surv(Time, death)~', x)))

univ_models <- lapply( univ_formulas, function(x){coxph(x, data = df_s_nonzero_prep)})
# Extract data 
univ_results <- lapply(univ_models,
                       function(x){ 
                         x <- summary(x)
                         p.value<-signif(x$wald["pvalue"], digits=2)
                         wald.test<-signif(x$wald["test"], digits=2)
                         beta<-signif(x$coef[1], digits=2);#coeficient beta
                         HR <-signif(x$coef[2], digits=2);#exp(beta)
                         HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                         HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
                         HR <- paste0(HR, " (", 
                                      HR.confint.lower, "-", HR.confint.upper, ")")
                         res<-c(beta, HR, wald.test, p.value)
                         names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
                                       "p.value")
                         return(res)
                         #return(exp(cbind(coef(x),confint(x))))
                       })


univ_results <- lapply(univ_models,
                       function(x){ 
                         x <- summary(x)
                         p.value <- signif(x$wald["pvalue"], digits=2)
                         wald.test <- signif(x$wald["test"], digits=2)
                         beta <- signif(x$coef[1], digits=2) #coeficient beta
                         HR <- signif(x$coef[2], digits=2) #exp(beta)
                         HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                         HR.confint.upper <- signif(x$conf.int[,"upper .95"], 2)
                         HR <- paste0(HR, " (", 
                                      HR.confint.lower, "-", HR.confint.upper, ")")
                         
                         # Create a data frame with the results
                         res <- data.frame(beta = beta, 
                                           HR = HR, 
                                           wald.test = wald.test, 
                                           p.value = p.value,
                                           stringsAsFactors = FALSE)
                         
                         # Ensure the data frame has 4 rows
                         if (nrow(res) < 4) {
                           res <- rbind(res, matrix(NA, nrow = 4 - nrow(res), ncol = ncol(res)))
                           colnames(res) <- c("beta", "HR (95% CI for HR)", "wald.test", "p.value")
                         }
                         
                         return(res)
                       })

res <- t(as.data.frame(univ_results, check.names = FALSE))
res <- as.data.frame(res)

# Zeilennamen in eine neue Spalte "row_name" umwandeln
res$Variable <- rownames(res)

# Optional: Zeilennamen des Dataframes entfernen
rownames(res) <- NULL

# Verschieben der letzten Spalte an die erste Position
res_reorder <- res[, c(ncol(res), 1:(ncol(res)-1))]
df_s_nonzero_prep
