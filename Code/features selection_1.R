assess <- function(model_frame, data_trn, data_tst) {
  
  oos <- model_frame %>% 
    predict(data_tst) %>% 
    group_by(model, .add = TRUE) %>% 
    yardstick::rsq_trad(truth, prediction) %>% 
    mutate(type = "Out-of-sample") %>% 
    arrange(desc(.estimate))
  
  is <- model_frame %>% 
    predict(data_trn) %>% 
    group_by(model, .add = TRUE) %>% 
    yardstick::rsq_trad(truth, prediction) %>% 
    mutate(type = "In-sample")
  
  return(bind_rows(oos, is))
  
}

plotter <- function(df) {
  df %>% 
    mutate(lab = round(.estimate, 2)) %>% 
    mutate(model = str_wrap(model, 12)) %>% 
    mutate(model = factor(model, levels = unique(.$model))) %>% 
    ggplot(aes(model, .estimate)) +
    geom_point(aes(color = type), size = 2.5, shape = 4) + 
    geom_label(aes(label = lab, color = type), size = 2, nudge_x = 0.35) +
    theme_bw() +
    scale_color_manual(values = c("firebrick", "darkblue")) +
    theme(legend.title = element_blank(), 
          axis.title.x = element_blank()) +
    coord_cartesian(ylim = c(0.65, 0.95))
}

fit %>% 
  assess(data_trn, data_tst) %>% 
  plotter


progressr::handlers(global=TRUE)

model_frame_all <- data_trn %>% 
  regress(target ~ .*., 
          OLS = m("lm"),
          BAYES = m("bayes"),
          BMA = m("bma", iter = 1000),
          SEQREP = m("subset", method = "seqrep", IC = "AIC"),
          `FORWARD SELECTION` = m("subset", method = "forward", IC = "AIC"),
          `BACKWARD SELECTION` = m("subset", method = "backward", IC = "AIC"),
          LASSO = m("lasso"),
          BLASSO = m("blasso"),
          RIDGE = m("ridge"),
          BRIDGE = m("bridge"),
          ELASTICNET = m("enet"),
          ADALASSO = m("adalasso", lambda_ridge = c(0.001, 0.01, 0.1)),
          PCR = m("pcr"),
          PLSR = m("plsr"),
          HFR = m("hfr"),
          `GRADIENT BOOSTING` = m("boost"),
          SVR = m("svm"),
          .cv = "vfold_cv", .cv_args = list(v = 10))

eval_frame <- model_frame_all %>% 
  assess(data_trn, data_tst)

eval_frame %>% 
  plotter +
  theme(legend.position = "top", 
        axis.text.x = element_text(angle = 90))