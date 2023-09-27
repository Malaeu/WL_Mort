# Step 1: Separate the two classes
data_class_0 <- vb_df[vb_df$death == 0, ]
data_class_1 <- vb_df[vb_df$death == 1, ]

# Step 2: Manually undersample the majority class
data_class_0_sampled <- data_class_0[sample(1:nrow(data_class_0), 200), ]

# Step 3: Manually concatenate
vb_df_under_sampled <- rbind(data_class_0_sampled, data_class_1)

# Step 4: Apply SMOTE to oversample the minority class to 88 instances
perc_under_calc <- (200 / 44) * 100

# Apply SMOTE to oversample the minority class to 88 instances
vb_df_balanced <- SMOTE(death ~ ., vb_df_under_sampled, k = 5, perc.over = 100, perc.under = perc_under_calc)

# Check the class distribution
table(vb_df_balanced$death)

vb_df <- vb_df_balanced
