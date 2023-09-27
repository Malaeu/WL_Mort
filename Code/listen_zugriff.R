for(i in seq(-90, 60)){
  value <- all_models[[paste("model_time_of_ct", i)]]$Model$score
  print(value)
}
  model |> report()
  