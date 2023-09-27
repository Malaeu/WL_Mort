# Load the necessary library
library(dlookr)

adnan_df <- df %>%
  select(
    
    
    
    
    
    
    -death_on_the_list_without_ltx_numeric,
    -lap_meld_ltx_status_changed,
    
    -survival_days_until_ltx,
    -was_transplanted,
    -had_contact_after_nt,
    -survival_days_until_nt,
    -lab_meld_listing,
    -days_of_dialysis,
    -days_of_icu,
    -amount_of_catecholamine,
    -days_of_ventilation,
    
    -survival_days_until_nt_status_changed,
    -survival_days_until_ltx_status_changed,
    -survival_days_from_nt_until_last_contact_status_changed,
    -survival_days_from_wl_until_death_status_changed,
    -days_of_dialysis_status_changed,
    -days_of_icu_status_changed,
    -days_of_ventilation_status_changed,
    -lab_meld_nt_status_changed,
    -lap_meld_ltx_status_changed,
    
    
  )




# Create the EDA report
df_selected_vars %>%
  eda_paged_report(
    target = "death",
    output_format = "pdf", # or "html"
    output_file = "adnan_df_eda.pdf", # or "your_output_file_name.html"
    output_dir = "~/Documents/Statistik/Adnan/Daten/DR_Adnan_Statistik/cl_adnan/Results",
    title = "Body Composition Faktoren und Lebertransplantations-Wartelistenmortalität",
    subtitle = "Daten Beschreibung des Datensets",
    abstract_title = "(Report 1 von 3)",
    author = "Eugen Malamutmann MD",
    logo_img = "~/Documents/Statistik/Adnan/Daten/DR_Adnan_Statistik/cl_adnan/Files/Uniklinikum-Essen-Logo.png",
    
    create_date = Sys.time(),
    theme = "orange", # or "blue"
    sample_percent = 100
  )

# Create the diagnose of dataset report
df %>%
  diagnose_paged_report(
    target = "death",
    output_format = "pdf", # or "html"
    output_file = "adnan_df.pdf", # or "your_output_file_name.html"
    output_dir = "~/Documents/Statistik/Adnan/Daten/DR_Adnan_Statistik/cl_adnan/Results",
    title = "Body Composition Faktoren und Lebertransplantations-Wartelistenmortalität",
    subtitle = "Daten Beschreibung des Datensets",
    abstract_title = "(Report 2 von 3)",
    author = "Eugen Malamutmann MD",
    logo_img = "~/Documents/Statistik/Adnan/Daten/DR_Adnan_Statistik/cl_adnan/Files/Uniklinikum-Essen-Logo.png",
    
    create_date = Sys.time(),
    theme = "orange", # or "blue"
    sample_percent = 100
  )
