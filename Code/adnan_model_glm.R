library(tidymodels)
library(brulee)
library(modeldata)
library(tidyverse)
library(skimr)
tidymodels_prefer()
theme_set(theme_minimal())
set.seed(1234)


df <- adnan_df_03_08_23
df$death <- as.factor(df$death)
df %>%
  filter(!is.na(death)) %>%
  ggplot(aes(bone, muscle, color = sex, size = meld_score_category)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~sex)

df$amount_of_catecholamine

adnan_df <- df %>%
  filter(!is.na(bone)) %>%
  select(bone,muscle,sat,vat,tat,imat,eat,pat,sex,dialysis_cat,ventilation_cat,portal_vein_thrombosis,death)

plot_na_pareto(adnan_df)
diagnose(adnan_df) %>% flextable()
diagnose_category(adnan_df) %>% flextable()
diagnose_numeric(adnan_df) %>% flextable()
diagnose_outlier(adnan_df) %>% flextable()

col_outlier <- find_outliers(adnan_df, index = F)

adnan_df%>% 
  select(col_outlier) %>% 
  plot_outlier()



bla <- imputate_outlier(adnan_df, pat, method = "mean")
plot(bla)






# “rpart” : Recursive Partitioning and Regression Trees
plot(imputate_na(adnan_df, amount_of_catecholamine, bone, method = "rpart"))+
  theme_classic()+
  theme(legend.position = "top")

# “mice” : Multivariate Imputation by Chained Equations

plot(imputate_na(adnan_df, amount_of_catecholamine, bone, method = "mice", seed = 999))+
  theme_minimal()+
  theme(legend.position = "top")

adnan_df <- adnan_df %>%
  mutate(amount_of_catecholamine = imputate_na(adnan_df, amount_of_catecholamine, bone, 
                                     method = "knn", no_attrs = TRUE))

adnan_df_death_as_num <- adnan_df
adnan_df$death <- as.factor(adnan_df$death)
# Use the select function with the - sign to deselect matching variables
df_selected_vars <- adnan_df %>% select(all_of(selected_feature_names),death)

df_selected_vars$death <- as.factor(df_selected_vars$death)

set.seed(123)
adnan_split <- initial_split(df_selected_vars, strata = death)
adnan_train <- training(adnan_split)
adnan_test <- testing(adnan_split)
set.seed(123)
adnan_boot <- bootstraps(adnan_train)
adnan_boot

glm_spec <- logistic_reg() %>%
  set_engine("glm")

glm_spec

rf_spec <- rand_forest() %>%
  set_mode("classification") %>%
  set_engine("ranger")

rf_spec

adnan_wf<- workflow() %>%
  add_formula(death ~ .)
adnan_wf

glm_rs <- adnan_wf %>%
  add_model(glm_spec) %>%
  fit_resamples(
    resamples = adnan_boot,
    control = control_resamples(save_pred = TRUE)
  )

glm_rs

rf_rs <- adnan_wf %>%
  add_model(rf_spec) %>%
  fit_resamples(
    resamples = adnan_boot,
    control = control_resamples(save_pred = TRUE)
  )

rf_rs
collect_metrics(rf_rs)
collect_metrics(glm_rs)
glm_rs %>%
  conf_mat_resampled()

adnan_final <- adnan_wf %>%
  add_model(glm_spec) %>%
  last_fit(adnan_split)

adnan_final
collect_metrics(adnan_final)

collect_predictions(adnan_final) %>%
  conf_mat(death, .pred_class)

adnan_final$.workflow[[1]] %>%
  tidy(exponentiate = TRUE)
