library(Boruta)
library(report)
set.seed(111)
library(funModeling)

variable_boruta <- names(final_df[,c(4:26,34:48,57:81)])

df_status(final_df)

df_selected_vars_borut <- final_df %>% select(all_of(variable_boruta),time,death)
boruta.bank_train <- Boruta(death~., data = df_selected_vars_borut, doTrace = 2)
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
# [1] "time"                                       
# [2] "dialysis_cat"                               
# [3] "icu_cat"                                    
# [4] "ventilation_cat"                            
# [5] "catecholamine_cat"                          
# [6] "meld_score_category"                        
# [7] "vat"                                        
# [8] "imat"                                       
# [9] "pat"                                        
# [10] "status_changed_to_nt"                       
# [11] "mortality_increase"                         
# [12] "known_mortality"                            
# [13] "muscle_normalized"                          
# [14] "vat_normalized"                             
# [15] "imat_normalized"                            
# [16] "pat_normalized"                             
# [17] "paracardial_fat_index"                      
# [18] "bone_to_visceral_fat_index"                 
# [19] "muscle_to_paracardial_fat_index"            
# [20] "bone_muscle_index"                          
# [21] "visceral_to_subcutaneous_fat_index_extended"
# [22] "sarcopenia_index"                           
# [23] "paracardial_fat_index_normalized"           
# [24] "bone_to_visceral_fat_index_normalized"      
# [25] "muscle_to_paracardial_fat_index_normalized" 
# [26] "bone_muscle_index_normalized"               
# [27] "cardiac_fat_index_normalized"               
# [28] "cardiac_fat_to_muscle_index_normalized"     
# [29] "sarcopenia_index_normalized"                
# [30] "log_sarcopenia_index"                       
# [31] "combined_fat_muscle_index"                  
# [32] "log_sarcopenia_index_normalized"            
# [33] "combined_fat_muscle_index_normalized"       
# [34] "status_changed_to_nt_category"              
# [35] "status_changed_to_nt_cat"                   
# [36] "mortality_increase_cat"

bank_df <- attStats(boruta.bank)
print(bank_df)
# Use the select function with the - sign to deselect matching variables
df_selected_vars_borut <- df_selected_vars %>% select(all_of(variable_boruta),time,death)

df_status(df_selected_vars_borut)


library(mRMRe)
data <- data %>%
  mutate(across(where(is.factor), as.numeric))

# f_data <- mRMR.data(data = data.frame(df_selected_vars_borut))
# 
# results <- mRMR.classic("mRMRe.Filter", data = f_data, target_indices = 11,
#                         feature_count = 15)
# solutions <- solutions(results)
# 
# 
# # Build an mRMR-based network and obtain feature connections (topology)
# network <- new("mRMRe.Network", data = f_data, target_indices = c(1, 2),
#                levels = c(2, 1), layers = 1)
# solutions(network)
# 
# 
# mRMR.classic(data = f_data, target_indices = c(29),feature_count = 30)





#Exkludieren Sie `time`
data <-
  df_selected_vars_borut[,!(
    names(df_selected_vars_borut) %in% c(
      "time",
      "amount_of_catecholamine",
      
      "status_changed_to_nt_category",
      "status_changed_to_nt_cat",
      "mortality_increase_cat"
    )
  )]

data <- df_selected_vars_borut
data <- df
# Ihr Datensatz 'data' und die Zielvariable 'target'
target <- "death"
data <- df_selected_vars_borut[ , !(names(df_selected_vars_borut) %in% "time")]

# Erzeugt ein mRMRe.Data Objekt
data.mrmr <- mRMR.data(data)

# Erzeugt das Feature selection Objekt
res <- mRMR.classic(data.mrmr, feature_count = 15, target_indices = c(2))

# Zeigt die Ergebnisse an
print(solutions(res))
res


# Convert target variable into a factor
data[, "death"] <- as.numeric(df_selected_vars_borut[, "death"]) 

# Create a mRMRe.Data object
data.mrmr <- mRMR.data(data)

# Run the feature selection procedure
res <- mRMR.classic(data.mrmr, feature_count = 7, target_indices = ncol(data))

# Print the selected features
print(solutions(res))
selected_feature_names <- colnames(data)[solutions(res)[[1]]]
selected_features <- data[, selected_feature_names]
#selected_features
selected_feature_names <- unique(selected_feature_names) 
selected_feature_names <- selected_feature_names[selected_feature_names != "death"]
selected_feature_names
df_status(df_selected_vars_borut)
