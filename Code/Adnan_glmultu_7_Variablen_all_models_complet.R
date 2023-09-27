save.image(file = "adfnan_30_08_2023.RData")
library(tidyverse)
library(tidymodels)
library(pins)
library(vetiver)
library(plumber)
library(gt)
library(conflicted)
library(car)        # extracts model results
library(MASS)       # provides "birthwt" dataset
library(ISLR)       # provides "Wage" dataset
library(tictoc)     # checks running time
library(sjPlot)     # visualizes model results
library(glmulti)    # finds the BEST model
library(flextable)  # beautifies tables
 # provides a lot of useful stuff !!! 
library(performance)# checks and compares quality of models
library(funModeling)
tidymodels_prefer()
library(nnet)
library(themis)
library(doParallel)
library(gtsummary) 
library(bonsai) 
library(discrim)
library(finetune)
library(patchwork)
library(vip)
library(DALEXtra) 

theme_set(theme_light(base_size = 12)) # beautifies plots
theme_update(panel.grid.minor = element_blank())

df <- df_selected_vars_borut
df <- df |>  select(all_of(selected_feature_names),death,-status_changed_to_nt,-meld_score_category)
df_status(df)

# selected_feature_names 7 Variablen
# [1] "dialysis_cat"                               
# [2] "mortality_increase"                         
# [3] "imat_normalized"                            
# [4] "ventilation_cat"                            
# [5] "muscle_normalized"                          
# [6] "visceral_to_subcutaneous_fat_index_extended"
# [7] "known_mortality"              
# 
# # selected_feature_names 13 Variablen
# [1] "known_mortality"                            
# [2] "bone_muscle_index"                          
# [3] "bone_to_visceral_fat_index_normalized"      
# [4] "dialysis_cat"                               
# [5] "ventilation_cat"                            
# [6] "combined_fat_muscle_index_normalized"       
# [7] "muscle_to_paracardial_fat_index_normalized" 
# [8] "imat_normalized"                            
# [9] "visceral_to_subcutaneous_fat_index_extended"
# [10] "muscle_normalized"                          
# [11] "pat"                                        
# [12] "vat"                                        
# [13] "mortality_increase"    
# 

death ~ dialysis_cat + mortality_increase + imat_normalized + ventilation_cat + muscle_normalized + visceral_to_subcutaneous_fat_index_extended + known_mortality



tic()

h_model <- glmulti(death ~ dialysis_cat + mortality_increase + imat_normalized + ventilation_cat + muscle_normalized + visceral_to_subcutaneous_fat_index_extended + known_mortality,
                   data   = df, 
                   crit   = aicc,       # AICC corrected AIC for small samples
                   level  = 2,          # 2 with interactions, 1 without  
                   method = "h",        # "d", or "h", or "g"
                   family = gaussian, 
                   fitfunction = glm,   # Type of model (LM, GLM, GLMER etc.)
                   confsetsize = 100)   # Keep 100 best models

toc() # 19 sec elapsed: 1921 models 

optimal_model <- h_model@objects[[1]]
optimal_model


plot(effects::allEffects(h_model@objects[[1]]),
     lines = list(multiline = T),
     confint = list(style = "auto"))



print(h_model)



plot(h_model)



weightable(h_model)[1:6,] %>% 
  regulartable() %>%       # beautifying tables
  autofit()


plot(h_model, type = "s")
best_model <- h_model@objects[[2]]


plot_model(best_model, type = "int") %>% 
  plot_grid()

best_model <- h_model@objects[[2]]
car::Anova(best_model)


plot_model(best_model, type = "int") %>% 
  plot_grid()

