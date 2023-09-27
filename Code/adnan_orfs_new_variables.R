working_dir <- getwd()
Sys.setenv(TMPDIR = working_dir)




# Create a new variable representing the known mortality rates
df$known_mortality <- with(df, 
                           ifelse(lab_meld_listing < 9, 0.037, 
                                  ifelse(lab_meld_listing >= 10 & lab_meld_listing <= 19, 0.20, 
                                         ifelse(lab_meld_listing >= 20 & lab_meld_listing <= 29, 0.455, 
                                                ifelse(lab_meld_listing >= 30 & lab_meld_listing <= 39, 0.745, 
                                                       ifelse(lab_meld_listing >= 40, 1, NA))))))

# Create a new variable representing the increase in mortality for each MELD point
df$mortality_increase <- with(df, 
                              ifelse(lab_meld_listing > 20, (lab_meld_listing - 20) * 0.02, 
                                     ifelse(lab_meld_listing < 20, (20 - lab_meld_listing) * 0.01, 0)))


adnan_df <- df

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

# Now you can call this function for columns with missing values
adnan_df <- impute_and_plot(adnan_df, c("known_mortality","mortality_increase"))


adnan_df$death <- as.numeric(adnan_df$death)


# Specify the predictors explicitly in the formula
cox_model <- coxph(Surv(time, death) ~ eat + tat + imat + pat + vat + muscle + known_mortality + mortality_increase, data = adnan_df)
summary(cox_model)

ylim = c(30, 65)
xlim = c(170, 235)


#Use the cbind() function to combine the datasets, adding the variable time from dataset adnan_df to dataset df_selected_vars:
#

df_selected_vars <- cbind(df_selected_vars, time = adnan_df$time)


set.seed(329730)

set.seed(1234)
adnan_split <- initial_split(df_selected_vars, strata = death)
adnan_train <- training(adnan_split)
adnan_test <- testing(adnan_split)

index_train <- sample(nrow(df_selected_vars), 150) 

pbc_orsf_train <- pbc_orsf[index_train, ]
pbc_orsf_test <- pbc_orsf[-index_train, ]

fit <- orsf(data = adnan_train, 
            formula = Surv(time, death) ~ . -known_mortality - meld_score_category - mortality_increase,
            oobag_pred_horizon = 365.25 * 5)
fit

orsf_vi_negate(fit)

orsf_vi_permute(fit)


set.seed(329730)
pred_horizon <- 365.25 * 5
fit <- orsf(adnan_train, 
            Surv(time, death) ~ . -known_mortality - meld_score_category - mortality_increase,
            oobag_pred_horizon = pred_horizon)


pd_vent_cat_tv <- orsf_pd_oob(fit, pred_spec = list(ventilation_cat = c("no", "yes")),
                         pred_horizon = seq(365, 365*5))

ggplot(pd_vent_cat_tv, aes(x = pred_horizon, y = mean, color = ventilation_cat)) + 
  geom_line() +
  labs(x = 'Time since baseline',
       y = 'Expected risk')

pd_smry <- orsf_summarize_uni(fit)

pd_smry 

table_dt <- pd_smry$dt


library(gt)

table_gt <- gt(table_dt) %>%
  tab_header(
    title = "Predicted risk at time t = 1826.25 for top 12 predictors",
    subtitle = "Expected risk"
  ) %>%
  tab_footnote(
    footnote = "risk at time",
    locations = cells_body()
  )
install.packages("gtExtras")

library(gtExtras)

table_gt_nature <- table_gt %>%
  tab_options(
    table.font.size = "small",
    heading.align = "center",
    data_row.padding = px(4),
    column_labels.border.top.width = px(1),
    column_labels.border.bottom.width = px(1),
    row_group.border.bottom.width = px(1)
  ) %>%
  tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels()) %>%
  tab_style(style = list(cell_text(weight = "bold")), locations = cells_stubhead()) %>%
  tab_style(style = list(cell_borders(sides = "top", weight = px(0.5))), locations = cells_body()) %>%
  tab_style(style = list(cell_borders(sides = "bottom", weight = px(0.5))), locations = cells_body())

#install.packages("webshot")
#library(webshot)
library(webshot2)
#install.packages("htmlwidgets")
library(htmlwidgets)
#install.packages("htmltools")
library(htmltools)

temp_file <- tempfile(fileext = ".html")
htmltools::save_html(table_gt_nature, temp_file)



htmltools::save_html(table_gt_nature, "table_nature_style.html")




table_gt_nature <- table_gt %>%
  tab_options(
    table.font.size = "small",
    heading.align = "center",
    data_row.padding = px(4),
    column_labels.border.top.width = px(1),
    column_labels.border.bottom.width = px(1),
    row_group.border.bottom.width = px(1)
  ) %>%
  tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels()) %>%
  tab_style(style = list(cell_text(weight = "bold")), locations = cells_stubhead()) %>%
  tab_style(style = list(cell_borders(sides = "top", weight = px(0.5))), locations = cells_body()) %>%
  tab_style(style = list(cell_borders(sides = "bottom", weight = px(0.5))), locations = cells_body()) %>%
  tab_spanner(
    label = "|------------------------------------ risk ------------------------------------|",
    columns = vars(Value, Mean, Median, `25th %`, `75th %`)
  )


pred_spec <- list(pat = seq(1, 10, length.out = 25))

ice_oob <- orsf_ice_oob(fit, pred_spec, boundary_checks = FALSE)

ice_oob

ice_oob[, pred_subtract := rep(pred[id_variable==1], times=25)]
ice_oob[, pred := pred - pred_subtract]

library(ggplot2)

ggplot(ice_oob, aes(x = pat, 
                    y = pred, 
                    group = id_row)) + 
  geom_line(alpha = 0.15) + 
  labs(y = 'Change in predicted risk') +
  geom_smooth(se = FALSE, aes(group = 1))



 fit <-  orsf(data = adnan_train, 
       formula = Surv(time, death) ~ . - meld_score_category  , 
       control = orsf_control_cph(),
       n_tree = 50,
       oobag_pred_horizon = 3500,
       oobag_eval_every = 1)
 fit
 
 orsf_vi_permute(fit)
 
 hist(fit$pred_oobag, 
      main = 'Ensemble out-of-bag survival predictions at t=3,500')
 # what function is used to evaluate out-of-bag predictions?
 fit$eval_oobag$stat_type
 #> [1] "Harrell's C-statistic"
 
 # what is the output from this function?
 fit$eval_oobag$stat_values
 #>           [,1]
 #> [1,] 0.8381955
 
 plot(
   x = seq(1, 50, by = 1),
   y = fit$eval_oobag$stat_values, 
   main = 'Out-of-bag C-statistic computed after each new tree is grown.',
   xlab = 'Number of trees grown',
   ylab = fit$eval_oobag$stat_type
 )
 
 oobag_fun_tdep_cstat <- function(y_mat, s_vec){
   
   as.numeric(
     SurvMetrics::Cindex(
       object = Surv(time = y_mat[, 1], event = y_mat[, 2]), 
       predicted = s_vec,
       t_star = 3500
     )
   )
   
 }
 
 fit_tdep_cstat <- orsf(data = adnan_train,
                        formula = Surv(time, death) ~ . - meld_score_category,
                        n_tree = 500,
                        oobag_pred_horizon = 3500,
                        oobag_fun = oobag_fun_tdep_cstat,
                        importance = 'negate')
 
 fit_tdep_cstat$importance
 