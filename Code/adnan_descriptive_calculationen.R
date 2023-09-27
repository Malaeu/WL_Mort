save.image(file = "adfnan_16_08_2023.RData")
# Write your dataframe to an Excel file
write.xlsx(merged_df_16_08_ordered, "merged_df_16_08_ordered.xlsx")

# Install the openxlsx package if not already installed
if (!require(openxlsx)) {
  install.packages("openxlsx")
}

# Load the package
library(openxlsx)
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
library(openxlsx)

df_status(merged_df_16_08_ordered)
# Find the column names that start with 'date' or 'survival'
cols_to_add <- grep("^(date|survival|death|last_|pat|vat)", names(df), value = TRUE)

# Create a new dataframe with these columns
new_df <- df[, cols_to_add]

#Merge dataframes by 'pat' and 'vat'
new_df <- select(new_df, -death)


merged_df_16_08 <- merge(adnan_df_03_08_23,merged_df, by = c("pat", "vat"), all.x = TRUE)
# Extract year from date_of_wl
merged_df$year <- year(merged_df$date_of_wl)
#Count number of observations per year
table(merged_df$year)
sum(complete.cases(merged_df$year))
# Print the table
print(year_counts)



# # Ensure that date_of_wl and last_contact are in Date format
# merged_df$date_of_wl <- as.Date(merged_df$date_of_wl)
# merged_df$last_contact <- as.Date(merged_df$last_contact)
# merged_df$date_of_nt <- as.Date(merged_df$date_of_nt)
# merged_df$date_of_ct <- as.Date(merged_df$date_of_ct)
# merged_df$date_of_death <- as.Date(merged_df$date_of_death)
# merged_df$date_of_birth <- as.Date(merged_df$date_of_birth)
# merged_df$date_of_ltx <- as.Date(merged_df$date_of_ltx)

# Create a new variable Time_in_days which is the difference between last_contact and date_of_wl
merged_df$time1 <- as.numeric(merged_df$last_contact - merged_df$date_of_wl)

# Create a new variable time_of_ct which is the difference between date_of_ct and date_of_wl
merged_df_16_08$time_of_ct <- as.numeric(merged_df_16_08$date_of_ct - merged_df_16_08$date_of_wl)

merged_df_16_08$time_of_ct_nt <- as.numeric(merged_df_16_08$date_of_ct - merged_df_16_08$date_of_nt)

merged_df_16_08$time_of_ct_ltx <- as.numeric(merged_df_16_08$date_of_ct - merged_df_16_08$date_of_ltx)

num_complete_cases <- sum(complete.cases(merged_df$survival_days_from_wl_until_death))
print(num_complete_cases)

# Remove .y columns
merged_df_16_08 <- merged_df_16_08[, !grepl("\\.y$", names(merged_df_16_08))]

# Remove .x from remaining column names
names(merged_df_16_08) <- sub("\\.x$", "", names(merged_df_16_08))

table(merged_df_16_08$year)

merged_df_16_08 <- merged_df_16_08 %>%
  filter(year > 2013) %>%
  count(year)

merged_df_16_08 <- merged_df_16_08 %>%
  filter(year > 2013)

# Convert all POSIXct columns to Date

merged_df_16_08[] <- lapply(merged_df_16_08, function(x) if ("POSIXct" %in% class(x)) as.Date(x) else x)
merged_df_16_08[] <- lapply(merged_df_16_08, function(x) if (any(class(x) == "POSIXct")) as.Date(x) else x)



num_complete_cases_deceased <- sum(complete.cases(merged_df_16_08$survival_days_from_wl_until_death))
print(num_complete_cases_deceased)
num_complete_cases_ltx <- sum(complete.cases(merged_df_16_08$survival_days_until_ltx))
print(num_complete_cases_ltx)
num_complete_cases_nt <- sum(complete.cases(merged_df_16_08$survival_days_until_nt))
print(num_complete_cases_nt)

num_complete_cases_survival_nt <- sum(complete.cases(merged_df_16_08$survival_days_from_nt_until_last_contact))
print(num_complete_cases_survival_nt)




# here are 65 rows where survival_days_from_wl_until_death is not NA. 
# That means 65 patients are deceased. from 283 on waiting list
# There are 46 rows where survival_days_until_nt is not NA. 
# That means 46 patients were not transplantable NT. 
# I want to know how many patients from NT are deceased ?
# num_deceased_NT
# [1] 1
# 

# Count complete cases for both columns
num_deceased_NT <- sum(complete.cases(merged_df_16_08$survival_days_from_wl_until_death,
                                      merged_df_16_08$survival_days_until_nt))
num_deceased_NT

num_ltx_from_nt <- sum(complete.cases(merged_df_16_08$survival_days_until_ltx,
                                      merged_df_16_08$survival_days_until_nt))
num_ltx_from_nt

num_deceased_ltx <- sum(complete.cases(merged_df_16_08$survival_days_until_ltx,
                                      merged_df_16_08$survival_days_from_wl_until_death))
num_deceased_ltx


# Create a vector of your calculated values
values <- c(num_complete_cases_deceased, num_complete_cases_ltx, num_complete_cases_nt, 
            num_deceased_NT, num_ltx_from_nt, num_deceased_ltx)

# Create a vector of the names for your calculated values
names <- c("num_complete_cases_deceased", "num_complete_cases_ltx", "num_complete_cases_nt", 
           "num_deceased_NT", "num_ltx_from_nt", "num_deceased_ltx")

# Combine the vectors into a data frame
table_df <- data.frame(Names = names, Values = values)

# Calculate total number of patients
total_patients <- nrow(merged_df_16_08)

# Calculate percentages and round them to one decimal place
percentages <- round((values / total_patients) * 100, digits = 1)

# Add rounded percentages to data frame as a new column
table_df$Percent_of_Total <- percentages

# Print the updated data frame
print(table_df)


merged_df_16_08 <- merged_df_16_08[ , !duplicated(names(merged_df_16_08))]

# Get column names
colnames <- colnames(merged_df_16_08)

# Separate columns by type
year_death_time_cols <- c("year", "death", "time")
factor_cols <- sort(colnames[sapply(merged_df_16_08, is.factor)])
numeric_cols <- setdiff(sort(colnames[sapply(merged_df_16_08, is.numeric)]), 
                        c(year_death_time_cols, factor_cols))
vat_bone_muscle_sat_imat_eat_tat_pat_cols <- c("vat", "bone", "muscle", "sat","imat","eat","tat","pat")
# Get normalized numeric columns
# Get normalized numeric columns
normalized_numeric_cols <- sort(numeric_cols[grepl("_normalized$", numeric_cols)])

# Combine all column names in desired order
ordered_colnames <- c(year_death_time_cols,
                      factor_cols,
                      setdiff(numeric_cols, c(vat_bone_muscle_sat_imat_eat_tat_pat_cols, normalized_numeric_cols)),
                      vat_bone_muscle_sat_imat_eat_tat_pat_cols,
                      normalized_numeric_cols)

# Rearrange data frame columns
merged_df_16_08_ordered <- merged_df_16_08[, ordered_colnames]

# Get column names that end with '.1'
cols_with_suffix_dot_one <- colnames(merged_df_16_08_ordered)[grepl("\\.1$", colnames(merged_df_16_08_ordered))]

# Remove these columns from the dataframe
merged_df_16_08_ordered <- merged_df_16_08_ordered[ , !(colnames(merged_df_16_08_ordered) %in% cols_with_suffix_dot_one)]
# Get column names that are in merged_df_16_08 but not in merged_df_16_08_ordered
cols_to_add <- setdiff(colnames(merged_df_16_08), colnames(merged_df_16_08_ordered))

# Add these columns to merged_df_16_08_ordered
merged_df_16_08_ordered <- cbind(merged_df_16_08_ordered, merged_df_16_08[, cols_to_add])

# Create a logical vector where TRUE indicates a match and FALSE indicates no match
match_check <- (merged_df_16_08_ordered$death == 0 & merged_df_16_08_ordered$death_on_the_list_without_ltx == "no")

# Print out the number of matches and non-matches
cat("Number of matches: ", sum(match_check), "\n")
cat("Number of non-matches: ", length(match_check) - sum(match_check))

#If death_on_the_list_without_ltx == "yes", it means that the patient passed away before they could receive a liver transplant.

#If death_on_the_list_without_ltx == "no", it indicates that the patient passed away after receiving a liver transplant.
#mean several things:

#The patient received a liver transplant in time.
#The patient is still on the waiting list and has not yet received a transplant.
#The patient was removed from the list for reasons other than death (for example, their health improved or worsened to an extent where they were no longer eligible for a transplant).
sum(merged_df_16_08$time_of_ct> -90)
