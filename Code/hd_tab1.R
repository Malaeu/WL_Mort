




hd_tab1 <- adnan_train_df %>%
  tbl_summary(
    by = death,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2
  ) %>%
  add_p(test = list(
    all_continuous() ~ "kruskal.test", 
    all_categorical() ~ "fisher.test"
  )) %>% 
  add_overall() %>%
  modify_table_body(
    ~.x %>% 
      dplyr::filter(!is.na(p.value) & p.value < 0.05)
  ) %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**death**") %>%
  modify_caption("**Table 1: Descriptive Statistics Training Data**")



hd_tab1
# Save the summary table to an html file
library(gt)
# Save the summary table to an html file
hd_tab1 %>% 
  as_gt() %>% 
  gt::gtsave("summary_table.html")
hd_tab1 %>%
  as_kable_extra()
janitor::chisq.test(df$hu_listung, df$death,simulate.p.value = TRUE)

# Assume df is your data frame
variable_names <- names(df_truncate_column_names)
# Assume df is your data frame
variable_names <- names(df_with_timeS)
# Calculate the length of each variable name
name_lengths <- sapply(variable_names, nchar)
# Names with more than 50 characters
long_names <- names(name_lengths)[name_lengths > 60]

# Print the long names
print(long_names)


# Print the lengths
print(name_lengths)
# Lengths of names with more than 50 characters
long_name_lengths <- name_lengths[name_lengths > 59]

# Print the long names and their lengths
print(long_name_lengths)

df_with_timeS <- truncate_column_names(df_with_timeS)

# Rename the variable using base R
names(df_with_timeS)[names(df_with_timeS) == "death_on_the_list_w/o_ltx"] <- "death_on_the_list_w_o_ltx"
