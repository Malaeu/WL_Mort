library(flextable) # for beautifying tables
library(dlookr)    # for the main event of the evening ;)
# Load packages
library(mice)

df <- adnan_df_03_08_23
# Create the vector with column names
column_names <- c('survival_days_until_nt', 'survival_days_until_ltx', 
                  'survival_days_from_nt_until_last_contact', 
                  'survival_days_from_wl_until_death', 'days_of_dialysis', 
                  'days_of_icu', 'days_of_ventilation',"lab_meld_nt","lap_meld_ltx")
df <- create_dummy_vars(df, column_names)


# Number of levels
num_levels <- nlevels(df$cause_of_death)
cat("Number of levels:", num_levels, "\n")

# Actual levels
actual_levels <- levels(df$cause_of_death)
cat("Actual levels:\n")
print(actual_levels)

df$cause_of_death <- addNA(df$cause_of_death)

# Now you can call this function for columns with missing values




plot_na_pareto(df)

plot_na_intersect(df)  
plot_na_hclust(df)
diagnose_paged_report(df)
diagnose_web_report(df)

# file name is ./Diagn_heartfailure.html, "blue" theme and not browse
diagnose_web_report(df, output_dir = ".", author = "Eugen Malamutmann UME",
                    output_file = "Diagnose_Waitinglist_Mortality.html", theme = "blue", browse = TRUE)

diagnose(adnan_df) %>% flextable()
diagnose_category(adnan_df) %>% flextable()
diagnose_numeric(adnan_df) %>% flextable()
diagnose_outlier(adnan_df) %>% flextable()


df %>%
  diagnose() %>%
  select(-unique_count, -unique_rate) %>% 
  filter(missing_count > 0) %>% 
  arrange(desc(missing_count)) %>%
  gt() %>%
  tab_header(
    title = "Diagnose-Ergebnisse",
    subtitle = "Nur Zeilen mit mehr als 5 fehlenden Werten"
  ) 


diagnose_outlier(df) %>%
  filter(outliers_cnt > 5) %>%
  arrange(desc(outliers_cnt)) %>%
  flextable()


# Now you can call this function for columns with missing values
df <- impute_and_plot(df, c('amount_of_catecholamine', 'bone', 'muscle',"lab_meld_listing","crea_listing","sat","vat","imat","eat","pat","tat","hcc"))


adnan_df <- df %>%
  select(
    -survival_days_from_wl_until_death,
    -cause_of_death,
    
    -survival_days_from_nt_until_last_contact,
    -time_till_nt,
    -death_on_the_list_without_ltx,
    -death_on_the_list_without_ltx_numeric,
    -lap_meld_ltx_status_changed,
    -lap_meld_ltx,
    -survival_days_until_ltx,
    -was_transplanted,
    -had_contact_after_nt,
    -survival_days_until_nt,
    -lab_meld_listing,
    -days_of_dialysis,
    -days_of_icu,
    -amount_of_catecholamine,
    -days_of_ventilation,
    -lab_meld_nt,
    -survival_days_until_nt_status_changed,
    -survival_days_until_ltx_status_changed,
    -survival_days_from_nt_until_last_contact_status_changed,
    -survival_days_from_wl_until_death_status_changed,
    -days_of_dialysis_status_changed,
    -days_of_icu_status_changed,
    -days_of_ventilation_status_changed,
    -lab_meld_nt_status_changed,
    -lap_meld_ltx_status_changed,
    -crea_listing,
    -bili_listing
  )

adnan_df <- adnan_df |> select(-cutpoint_rounded, -cutpoint_median_diff, -cutpoint_vs_median)

boruta.bank_train <- Boruta(death~., data = adnan_df, doTrace = 3)
print(boruta.bank_train)
#take a call on tentative features
boruta.bank <- TentativeRoughFix(boruta.bank_train)
print(boruta.bank)
plot(boruta.bank, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.bank$ImpHistory),function(i)
  boruta.bank$ImpHistory[is.finite(boruta.bank$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.bank$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.bank$ImpHistory), cex.axis = 0.7)

variable_boruta <- getSelectedAttributes(boruta.bank, withTentative = F)
variable_boruta





