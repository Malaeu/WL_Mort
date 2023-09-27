#=================================
#  State 1: Data Pre-processing
#=================================


# Data Pre-processing -----------------------------------------------------


# df_spss <- rio::import(Ksju_SPSS.sav)
# df <- Tabelle2_Abfrage
# defrag memory
saveRDS(df_s_nonzero_prep, file = "df_s_nonzero_prep.rds")
save(bca_acsn, file = "bca_acsn.Rds")
save(WL, file = "WL.Rds")
write_sav(df, "df.sav")
write.xlsx(df, "df.xlsx")
save.image(file = "df.RData")
rm(list = ls())
load(file = "df.RData")





# Load some packages ------------------------------------------------------

library(pacman)
# unique_packages <- unique(c(
#   "magrittr", "caret", "expss", "readxl", "caTools", "Metrics",
#   "stargazer", "MuMIn", "metafor", "corrplot", "ggpubr", "moments",
#   "RColorBrewer", "mRMRe", "Boruta", "stringr", "tidyr", "varrank",
#   "mixOmics", "funModeling", "scorecard", "rio", "here", "tidyverse",
#   "purrr", "gtsummary", "broom", "lmtest", "parameters", "see",
#   "skimr", "xlsxjars", "haven", "openxlsx", "anytime", "slider",
#   "tidyquant", "labelled", "dlookr", "easystats", "mlr3verse",
#   "conjurer", "synthpop", "afex", "aplpack", "aplore3", "arm", "babynames", 
#   "bestglm", "boot", "cowplot", "DataExplorer", "devtools", "Epi", "equatiomatic",
#   "exact2x2", "ez", "faraway", "fivethirtyeight", "foreign", "gapminder", "gee", 
#   "geepack", "GGally", "ggforce", "ggrepel", "ggridges", "ggthemes", "glue", 
#   "gmodels", "gridExtra", "gt", "Hmisc", "HSAUR", "infer", "janitor", "kableExtra", 
#   "knitr", "lars", "lattice", "leaps", "lme4", "lmerTest", "markdown", "MASS", 
#   "mdsr", "mice", "modelsummary", "moderndive", "mosaic", "multcomp", "naniar", 
#   "NHANES", "nhanesA", "nnet", "palmerpenguins", "pander", "party", "patchwork", 
#   "posterdown", "pROC", "PropCIs", "pscl", "psych", "pwr", "qcc", "QuantPsyc", 
#   "quantreg", "ResourceSelection", "rmarkdown", "rmdformats", "rms", "robustbase", 
#   "ROCR", "rpart", "rpart.plot", "rstanarm", "sandwich", "sessioninfo", "simputation",
#   "spelling", "styler", "survival", "survminer", "tableone", "tidymodels", "usethis",
#   "vcd", "VGAM", "viridis", "visdat", "xfun", "flextable", "officer"
# ))
# 
sapply(unique_packages, pacman::p_load)
# # Direkte Eingabe des Paketnamens als Zeichenkette:
# 
# 
# 
# pacman::p_load("magrittr", "caret", "expss", "readxl", "caTools", "Metrics",
#                "stargazer", "MuMIn", "metafor", "corrplot", "ggpubr", "moments",
#                "RColorBrewer", "mRMRe", "Boruta", "stringr", "tidyr", "varrank",
#                "mixOmics", "funModeling", "scorecard", "rio", "here", "tidyverse",
#                "purrr", "gtsummary", "broom", "lmtest", "parameters", "see",
#                "skimr", "xlsxjars", "haven", "openxlsx", "anytime", "slider",
#                "tidyquant", "labelled", "dlookr", "easystats", "mlr3verse",
#                "conjurer", "synthpop", "afex", "aplpack", "aplore3", "arm", "babynames", 
#                "bestglm", "boot", "cowplot", "DataExplorer", "devtools", "Epi", "equatiomatic",
#                "exact2x2", "ez", "faraway", "fivethirtyeight", "foreign", "gapminder", "gee", 
#                "geepack", "GGally", "ggforce", "ggrepel", "ggridges", "ggthemes", "glue", 
#                "gmodels", "gridExtra", "gt", "Hmisc", "HSAUR", "infer", "janitor", "kableExtra", 
#                "knitr", "lars", "lattice", "leaps", "lme4", "lmerTest", "markdown", "MASS", 
#                "mdsr", "mice", "modelsummary", "moderndive", "mosaic", "multcomp", "naniar", 
#                "NHANES", "nhanesA", "nnet", "palmerpenguins", "pander", "party", "patchwork", 
#                "posterdown", "pROC", "PropCIs", "pscl", "psych", "pwr", "qcc", "QuantPsyc", 
#                "quantreg", "ResourceSelection", "rmarkdown", "rmdformats", "rms", "robustbase", 
#                "ROCR", "rpart", "rpart.plot", "rstanarm", "sandwich", "sessioninfo", "simputation",
#                "spelling", "styler", "survival", "survminer", "tableone", "tidymodels", "usethis",
#                "vcd", "VGAM", "viridis", "visdat", "xfun", "flextable", "officer")
# 
# pacman::p_load_character(pkg_name)

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
  kableExtra
)
# Cleaning and reorganizing ------------------------------------------------------------------


df_status(bca_acsn)
df_status(WL_BodyComposition_Aktuell)
df_status(WL)
WL <- as_tibble(WL_BodyComposition_Aktuell)

WL %>% rename(
  acsn_nr = CT_Accession_Nr_1_Intern
)

names(WL)[3] <- "acsn_nr"

bca_acsn$acsn_nr <- as.numeric(bca_acsn$acsn_nr)

df <- left_join(WL, bca_acsn, by = "acsn_nr")
funModeling::status(df)
df <- df[,-c(54:59)]

df <- df %>% janitor::clean_names()

di=data_integrity(df)
summary(di)
print(di)
plot_num(df)
# Convert variables ending in "_cat" to factors ---------------------------

# Identify variables ending in "_cat"
cat_columns <- grep("_cat$", names(df), value = TRUE)
library(dplyr)
df <- df %>%
  mutate_at(all_of(cat_columns), factor)


# Convert other Categorical to Factors ------------------------------------

# Define the labels and their corresponding values
labels <- c("Alcoholic cirrhosis", "Hepatocellular carcinoma and cirrhosis",
            "NASH", "Primary sclerosing cholangitis", "Autoimmunhepatitis",
            "Others specify", "Virus related cirrhosis", "Budd Chiari",
            "Wilson disease", "Byler disease", "Extrahepatic biliary atresia",
            "Primary biliary cirrhosis", "Polycystic disease", "Alagille syndrome",
            "Drug-induced cirrhosis", "Not drug-induced cirrhosis",
            "Secondary sclerosing cholangitis", "Secondary biliary cirrhosis",
            "Caroli disease", "Hepatoblastoma", "Congenital biliary fibrosis",
            "Post-operative", "Post-traumatic", "Antitrypsin deficiency",
            "Schistosomiasis", "Primary hyperoxaluria", "Cystic fibrosis",
            "Epithelioid hemangioendothelioma")
values <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 15, 16, 17, 18, 19, 20, 21, 21,
            22, 23, 24, 25, 26)

# Check if "Primary_diagnosis" variable exists in the data frame
if ("primary_diagnosis" %in% names(df)) {
  # Convert "Primary_diagnosis" to factor with labels
  df$primary_diagnosis <- factor(df$primary_diagnosis, levels = values, labels = labels)
} else {
  print("Error: 'primary_diagnosis' variable not found in the data frame.")
}



# Define the labels and their corresponding values
labels <- c("MOF", "Sepsis", "Hemorrhagic Shock", "Pulmonary embolism",
            "Unknown Causes", "Cardiogenic Shock", "Malignant Disease",
            "ARDS", "Transplant rejection", "Cerebral hemorrhage",
            "Surgical Complications", "Tumor progression", "Suicide", "PNF")
values <- c(1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)

# Check if "cause_of_death" variable exists in the data frame
if ("cause_of_death" %in% names(df)) {
  # Convert "cause_of_death" to factor with labels
  df$cause_of_death <- factor(df$cause_of_death, levels = values, labels = labels)
} else {
  print("Error: 'cause_of_death' variable not found in the data frame.")
}


# Define the labels and their corresponding values
labels <- c("yes", "no")
values <- c(1, 0)

# Check if "hu_listung" variable exists in the data frame
if ("hu_listung" %in% names(df)) {
  # Convert "hu_listung" to factor with labels
  df$hu_listung <- factor(df$hu_listung, levels = values, labels = labels)
} else {
  print("Error: 'hu_listung' variable not found in the data frame.")
}

# Convert "death" variable
df$death <- factor(df$death, levels = c(1, 0), labels = c("yes", "no"))

# Convert "death_on_the_list_without_ltx" variable
df$death_on_the_list_without_ltx <- factor(df$death_on_the_list_without_ltx, levels = c(1, 0), labels = c("yes", "no"))

# Convert "ltx" variable
df$ltx <- factor(df$ltx, levels = c(1, 0), labels = c("yes", "no"))

# Convert "blood_type" variable
df$blood_type <- factor(df$blood_type, levels = c(1, 2, 3, 4), labels = c("A", "B", "AB", "O"))

# Convert "hcc" variable
df$hcc <- factor(df$hcc, levels = c(1, 0), labels = c("yes", "no"))

# Convert "child_pugh_score" variable
df$child_pugh_score <- factor(df$child_pugh_score, levels = c(1, 2, 3), labels = c("A", "B", "C"))

# Convert "dialysis_cat" variable
df$dialysis_cat <- factor(df$dialysis_cat, levels = c(1, 0), labels = c("yes", "no"))

# Convert "icu_cat" variable
df$icu_cat <- factor(df$icu_cat, levels = c(1, 0), labels = c("yes", "no"))

# Convert "ventilation_cat" variable
df$ventilation_cat <- factor(df$ventilation_cat, levels = c(1, 0), labels = c("yes", "no"))

# Convert "catecholamine_cat" variable
df$catecholamine_cat <- factor(df$catecholamine_cat, levels = c(1, 0), labels = c("yes", "no"))

# Convert "portal_vein_thrombosis" variable
df$portal_vein_thrombosis <- factor(df$portal_vein_thrombosis, levels = c(1, 0, 2), labels = c("yes", "no", "partiell"))

df_status(df)

# Filter rows where "body_parts" is not NA
filtered_df <- df[!is.na(df$body_parts), ]

# View the filtered data frame
filtered_df



# See the unique values of "body_parts"
unique_values <- unique(df$body_parts)

# Print the unique values
print(unique_values)
# Define the labels
labels <- c("whole_scan+abdominal_cavity+l5+l4_l5+l4+l3_l4+l3+l2_l3+l2+l1_l2+l1",
            NA,
            "whole_scan+abdominal_cavity+thoracic_cavity+mediastinum+pericardium+l5+l4_l5+l4+l3_l4+l3+l2_l3+l2+l1_l2+l1",
            "whole_scan+abdominal_cavity")

# Convert "body_parts" to numeric categorical variable with labels
df$body_parts <- as.integer(factor(df$body_parts, levels = labels))

# Verify the variable type
print(class(df$body_parts))

# Convert "body_parts" to numeric categorical variable with labels
df$body_parts <- ifelse(df$body_parts == "whole_scan+abdominal_cavity+l5+l4_l5+l4+l3_l4+l3+l2_l3+l2+l1_l2+l1", 1,
                        ifelse(df$body_parts == "whole_scan+abdominal_cavity+thoracic_cavity+mediastinum+pericardium+l5+l4_l5+l4+l3_l4+l3+l2_l3+l2+l1_l2+l1", 2,
                               ifelse(df$body_parts == "whole_scan+abdominal_cavity", 3, NA)))

# Verify the variable type
print(class(df$body_parts))


# Define the labels
labels <- c("whole_scan+abdominal_cavity+l5+l4_l5+l4+l3_l4+l3+l2_l3+l2+l1_l2+l1",
            "whole_scan+abdominal_cavity+thoracic_cavity+mediastinum+pericardium+l5+l4_l5+l4+l3_l4+l3+l2_l3+l2+l1_l2+l1",
            "whole_scan+abdominal_cavity")

# Convert "body_parts" to a factor with labels
df$body_parts <- factor(df$body_parts, levels = 1:3, labels = labels)

# Verify the variable type
print(class(df$body_parts))
# Convert "sex" to a factor variable with labels
df$sex <- factor(df$sex, levels = c("W", "M"), labels = c("female", "male"))

# Verify the variable type
print(class(df$sex))
df_status(df)

glimpse(df)
missing_glimpse(df)
ff_glimpse(df)

# Install and load the required packages
if (!require(tableone)) install.packages("tableone")
if (!require(knitr)) install.packages("knitr")
if (!require(kableExtra)) install.packages("kableExtra")
library(tableone)
library(knitr)
library(kableExtra)

# Assuming your dataset is named "daten"
# vars <- c("hu_listung",  
#           "survival_days_until_nt", "survival_days_until_ltx", 
#           "survival_days_from_nt_until_last_contact",  "death", 
#           "death_on_the_list_without_ltx", "survival_days_from_wl_until_death", 
#           "ltx","sex", "age_by_listung", "height", 
#           "weight", "bmi", "blood_type", "primary_diagnosis", "hcc", 
#           "child_pugh_score", "lab_meld_listing", "inr_listing", "bili_listing", 
#           "crea_listing", "sodium_listing", "platelets_listing", "lab_meld_nt", 
#           "lap_meld_ltx", "lap_meld_death", "inr_removal", "bili_removal", 
#           "crea_removal", "natrium_removal", "platelets_removal", "dialysis_cat", 
#           "days_of_dialysis", "icu_cat", "days_of_icu", "ventilation_cat", 
#           "days_of_ventilation", "catecholamine_cat", "amount_of_catecholamine", 
#           "exceptional_meld", "portal_vein_thrombosis", "cause_of_death", 
#           "bone", "muscle", "sat", "vat", "imat", "eat", 
#           "pat", "tat")


vars <- c("hu_listung", 
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
          "pat", "tat")






# Sort the variables alphabetically
vars <- sort(vars)


# # Note: please specify the categorical variables in your dataset
# catvars <- c("hu_listung", "death", "death_on_the_list_without_ltx", 
#              "ltx", "sex", "blood_type", "primary_diagnosis", "hcc", 
#              "child_pugh_score", "dialysis_cat", "icu_cat", "ventilation_cat", 
#              "catecholamine_cat", "portal_vein_thrombosis", "cause_of_death", 
#              "body_parts")


# Note: please specify the categorical variables in your dataset
catvars <- c("hu_listung", 
              "sex", "blood_type", "hcc", 
             "child_pugh_score", "dialysis_cat", "icu_cat", "ventilation_cat", 
             "catecholamine_cat", "portal_vein_thrombosis", "cause_of_death" 
             )





catvars <- sort(catvars)

# Create the table
tab1 <- CreateTableOne(vars = vars, data = df, factorVars = catvars)

# Print the table in a nice format
print(tab1, printToggle = FALSE) %>%
  kable(caption = "Tabelle 1. Diskreptive Statistik") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)

dput(names(df))

tab3 <- CreateTableOne(vars = vars, strata = "death" , data = df, factorVars = catvars)
print(tab3, printToggle = FALSE) %>% 
  kable(caption = "Tabelle 1. Diskreptive Statistik") %>%
  kable_styling(bootstrap_options = "striped", full_width = T)
tab3



table2 <-
  tbl_summary(
    df,
    include = c(vars,catvars),
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



print(table2, printToggle = FALSE) %>% 
  kable_styling(bootstrap_options = "striped", full_width = F)



table2
show_header_names(table2)


# Installieren und laden Sie das pROC-Paket
install.packages("pROC")
library(pROC)

# Erstellen Sie die ROC-Kurve
roc_obj <- roc(df$death, df$lab_meld_listing)

# Zeichnen Sie die ROC-Kurve
plot(roc_obj, main="ROC Curve for MELD Score")

# Berechnen und hinzufügen AUC auf dem Diagramm
auc(roc_obj)
text(0.7, 0.3, paste("AUC =", round(auc(roc_obj), 2)))

