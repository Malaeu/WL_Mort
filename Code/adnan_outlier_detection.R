
save.image(file = "adnan_30_08_2023.RData")
# Write your dataframe to an Excel file
write.xlsx(merged_df_16_08_ordered, "merged_df_16_08_ordered.xlsx")

# Loading necessary libraries
library(readxl)
library(tidyverse)
library(ggplot2)

df <- merged_df_16_08_ordered
# Filter patients based on time_of_ct <= 90
df_filtered <- df[df$time_of_ct > -90,]

# Body composition variables
body_composition_vars <- c("muscle", "vat", "bone", "sat", "imat", "eat", "tat", "pat")

# Univariate Analysis: Descriptive statistics
summary_stats <- df_filtered %>%
  select(all_of(body_composition_vars)) %>%
  summary()

# Plotting distributions
for (var in body_composition_vars) {
  boxplot(df_filtered[[var]], main=var, ylab="Value", xlab=var)
}

# Identifying outliers using IQR method
outliers_list <- list()

for (var in body_composition_vars) {
  Q1 <- quantile(df_filtered[[var]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df_filtered[[var]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  outliers <- df_filtered[df_filtered[[var]] < lower_bound | df_filtered[[var]] > upper_bound,]
  outliers_list[[var]] <- outliers
}

# Exploring association of outliers with 'sex', 'death', and 'time_of_ct'
outliers_details <- list()

for (var in body_composition_vars) {
  outlier_data <- outliers_list[[var]][, c("sex", "death", "time_of_ct")]
  sex_distribution <- table(outlier_data$sex)
  death_distribution <- table(outlier_data$death)
  mean_time_of_ct <- mean(outlier_data$time_of_ct, na.rm=TRUE)
  median_time_of_ct <- median(outlier_data$time_of_ct, na.rm=TRUE)
  
  outliers_details[[var]] <- list(
    sex_distribution = sex_distribution,
    death_distribution = death_distribution,
    mean_time_of_ct = mean_time_of_ct,
    median_time_of_ct = median_time_of_ct
  )
}

# Checking for overlaps in outliers between variables
overlaps <- list()

for (var1 in body_composition_vars) {
  overlaps[[var1]] <- list()
  for (var2 in body_composition_vars) {
    if (var1 != var2) {
      common_outliers <- intersect(rownames(outliers_list[[var1]]), rownames(outliers_list[[var2]]))
      if (length(common_outliers) > 0) {
        overlaps[[var1]][[var2]] <- common_outliers
      }
    }
  }
}
