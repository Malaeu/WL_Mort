---
editor_options: 
  markdown: 
    wrap: 72
---

# Mortality_on_waitinglist_LTX

## **Data set Summary:**

1.  **General Overview**:
    -   The data set captures clinical measurements, body composition
        parameters, and states of patients who are on the waiting list
        for liver transplantation or have undergone transplantation.
2.  **Key Variables**:
    -   **year**: The year of the data entry or observation.
    -   **death**: Binary indicator if the patient is deceased (1 if
        yes, 0 otherwise).
    -   **time**: Duration in days from `date_of_wl` to events: either
        death or, if death is 0, up to `last_contact`.
    -   **blood_type**: Blood type of the patient.
    -   **child_pugh_score**: A score to assess the prognosis of liver
        disease.
    -   **hcc**: Indicates if the patient has hepatocellular carcinoma
        (yes/no).
    -   **date_of_ct**: Date of CT scan.
    -   **date_of_wl**: Date the patient was listed for transplantation.
    -   **date_of_nt**: Date indicating when the patient was not
        transplantable.
    -   **last_contact**: Last known contact or follow-up date with the
        patient.
    -   **death_on_the_list_without_ltx**: Indicates if the patient died
        while on the transplant list without receiving a liver
        transplant (yes/no).
    -   **date_of_ltx**: Date of liver transplantation.
    -   **date_of_birth**: Birthdate of the patient.
    -   **date_of_death**: Date of death for the patient.
    -   **meld_score_category**: Category based on the MELD score,
        indicating the severity of liver disease.
    -   **known_mortality**: Known or established mortality risk for the
        patient.
    -   **mortality_increase**: Increase in mortality risk over time or
        based on certain conditions.
    -   **status_changed_to_nt**: Indicates if the status of the patient
        changed to "not transplantable".
3.  **Highlighted Covariates**:
    -   **visceral_to_subcutaneous_fat_index_extended**: A ratio
        indicating the distribution of visceral to subcutaneous fat.
    -   **weight**: Weight of the patient.
    -   **vat**: Measurement of visceral adipose tissue.
    -   **bone**: Bone measurement or related metric.
    -   **muscle**: Muscle measurement or related metric.
    -   **sat**: Subcutaneous adipose tissue measurement.
    -   **imat**: Intramuscular adipose tissue measurement.
    -   **eat**: Epicardial adipose tissue measurement.
    -   **tat**: Total adipose tissue measurement.
    -   **pat**: Paracardial adipose tissue measurement.
    -   **bone_to_visceral_fat_index_normalized**: Normalized index
        comparing bone to visceral fat.
    -   **combined_fat_muscle_index_normalized**: Normalized index
        combining fat and muscle measurements.
    -   **imat_normalized**: Normalized intramuscular adipose tissue
        measurement.
    -   **muscle_normalized**: Normalized muscle measurement.
    -   **muscle_to_paracardial_fat_index_normalized**: Normalized index
        comparing muscle to paracardial fat.
    -   **bone_muscle_index**: An index or measurement related to bone
        and muscle.
4.  **Clinical Implications**:
    -   The dataset can help in understanding the relationships between
        body composition parameters and the outcomes of patients on the
        waiting list for liver transplantation.
    -   The relationship between body composition and risk factors needs
        to be explored. It is of interest to determine if and how these
        measurements relate to factors such as being categorized as "not
        transplantable", death on the waiting list, meld_score_category,
        known_mortality, or mortality_increase.
    -   The dataset can also be used to determine optimal time points
        for CT scans relative to waitlisting to achieve maximum
        predictive power for adverse outcomes based on the `time_of_ct`
        variable. (Done)
5.  **Statistics**:
    -   **Complete Cases for Deceased**: This represents the number of
        patients who are deceased, with or without undergoing liver
        transplantation.
    -   **Total Cases for LTX**: This captures the total number of
        patients who underwent liver transplantation.
    -   **Complete Cases for NT**: Indicates the number of patients who
        were classified as "not transplantable." This might be redundant
        if the variable `status_changed_to_nt` has the exact count of
        such patients.
    -   **Complete Cases for Survival post NT**: Reflects the number of
        patients who remained alive after their status was changed to
        "not transplantable."

------------------------------------------------------------------------

An Exploratory Data Analysis (EDA) can help us understand the structure,
patterns, anomalies, and relationships within the data.

1\. **Univariate Analysis**:

-   **Descriptive Statistics**: For each variable,measures like mean,
    median, standard deviation, and range.
-   **Distribution Plots**: Plot histograms or kernel density plots for
    continuous variables to understand their distribution.
-   **Count Plots**: For categorical variables, plot bar charts to
    visualize the frequency of each category.

2\. **Bivariate Analysis**:

-   **Correlation Matrix**: Calculate and visualize the correlations
    between continuous variables using a heatmap. This will help
    identify variables that might have a linear relationship.
-   **Scatter Plots**: Plot scatter plots for pairs of continuous
    variables to visualize their relationship.
-   **Box Plots**: For continuous and categorical variable pairs, use
    box plots to understand the distribution of the continuous variable
    across different categories.

3\. **Multivariate Analysis**:

-   **Pair Plots**: Use pair plots or scatter plot matrices to visualize
    the relationships between multiple continuous variables at once.

<!-- -->

-   **Grouped Analysis**: Group data by certain categorical variables
    (like `meld_score_category`) and calculate statistics (like mean or
    median) for continuous variables. This can be visualized using
    grouped bar charts.

4\. **Handling Missing Data**:

-   Visualize missing data using heatmaps or missing data plots. This
    will give us a sense of how much data is missing and if there are
    patterns to the missing data.
-   Decide on strategies for handling missing data: imputation,
    deletion, etc., based on the patterns observed.

5\. **Outlier Detection**:

-   Use box plots, scatter plots, or statistical methods (like Z-scores)
    to detect outliers in the data. Decide on strategies to handle them:
    capping, transformation, or removal.

6\. **Feature Engineering**:

-   New features that might be useful for modeling. For example, ratios
    of different body composition parameters or interaction terms
    between significant variables.

7\. **Visual Patterns and Trends**:

-   Time series plots for variables with date-time data.
-   Survival curves if the dataset allows for it.

#### This code performs a Monte Carlo resampling method to find the best model based on AUC (Area Under the Curve) and IPA (Integrated Prediction Accuracy) using the **orsf** <sup>1</sup> function from the **aorsf package** <sup>2</sup>. The main steps of the code are:

If they exist, initialize or load the checked combinations and best
models from files; otherwise, create empty data structures.

Set the maximum working time for the search process.

Define the main effects and two-way interactions for the model.

Run a loop until the maximum working time is reached, performing the
following steps in each iteration:

-   a\. Randomly select terms (main effects and interactions).

-    b. Ensure main effects are included for any selected interaction
    terms.

-   c\. Skip the iteration if the combination was already checked.

-   d\. Construct the formula for orsf.

-   e\. Fit the model using orsf with net control instead of coxph.

-   f\. Skip the iteration if the model did not converge.

-   g\. Evaluate the model using the Score function.

-   h\. Store the model in the best_models list. i. Limit the number of
    stored models to 50 and AUC \> some_value.

-   j\. Add the selected terms to the checked_combinations dataframe.

-    k. Save the checked_combinations and best_models if 10 minutes have
    passed since the last save. Save the current state at the end of the
    run. Iterate through all models in the best_models list to find the
    one with the highest AUC and IPA values. Display the best model
    based on AUC and IPA.

The code iteratively searches for the best model by randomly selecting
terms and fitting the model using the orsf function. The best models are
stored in a list, and the process continues until the maximum working
time is reached. Finally, the best models based on AUC and IPA are
displayed.

## References

1.  Jaeger BC, Long DL, Long DM, Sims M, Szychowski JM, Min YI, Mcclure
    LA, Howard G, Simon N. Oblique random survival forests. *Annals of
    applied statistics* 2019 Sep; 13(3):1847-83. DOI:
    10.1214/19-AOAS1261

2.  Jaeger BC, Welden S, Lenoir K, Speiser JL, Segar MW, Pandey A,
    Pajewski NM. Accelerated and interpretable oblique random survival
    forests. *arXiv e-prints* 2022 Aug; arXiv-2208. URL:
    <https://arxiv.org/abs/2208.01129>
