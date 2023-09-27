tic()

h_model <- glmulti(death ~ dialysis_cat + mortality_increase + imat_normalized + ventilation_cat + muscle_normalized + visceral_to_subcutaneous_fat_index_extended + known_mortality,
                   data   = df, 
                   crit   = aicc,       # AICC corrected AIC for small samples
                   level  = 2,          # 2 with interactions, 1 without  
                   method = "d",        # "d", or "h", or "g"
                   family = gaussian, 
                   fitfunction = glm,   # Type of model (LM, GLM, GLMER etc.)
                   confsetsize = 100)   # Keep 100 best models

toc() # 19 sec elapsed: 1921 models 