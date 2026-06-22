
# =========================================================
# Helper functions and shared labels
# =========================================================

clean_id <- function(x) {
  x <- as.character(x)
  x <- sub("^0_", "", x)
  x <- ifelse(grepl("^[0]+[0-9]+$", x), sub("^0+", "", x), x)
  x
}

trait_labels <- c(
  "YengoBMI2018" = "bmi",
  "Okbay2022educ" = "education",
  "SaundersDrnkwk" = "alcohol_drinking_weekly",
  "SaundersSmkInt" = "smoking_initiation",
  "SuzukiDia2024" = "diabetes",
  "Keaton2024diaBP" = "diastolic_blood_pressure",
  "Keaton2024sysBP" = "systolic_blood_pressure",
  "Keaton2024PP" = "pulse_pressure",
  "Graham2021tc_eur" = "total_cholesterol",
  "Socrates2024" = "social_isolation",
  "Trpchevska2022" = "Hearing_Loss",
  "Choquet2021" = "Vision_Loss",
  "Graham2021TG_eur" = "Triglycerides",
  "Graham2021HDL_eur" = "HDL_Cholesterol",
  "Graham2021LDL_eur" = "LDL_Cholesterol",
  "Klimentidis2018mvpa" = "Physical_Activity",
  "Adams2025mdd" = "Major_Depressive_Disorder"
)

all_prs_vars <- c(
  "bmi_score",
  "education_score",
  "alcohol_drinking_weekly_score",
  "smoking_initiation_score",
  "diabetes_score",
  "diastolic_blood_pressure_score",
  "systolic_blood_pressure_score",
  "pulse_pressure_score",
  "total_cholesterol_score",
  "Major_Depressive_Disorder_score",
  "social_isolation_score",
  "Hearing_Loss_score",
  "Vision_Loss_score",
  "Triglycerides_score",
  "HDL_Cholesterol_score",
  "LDL_Cholesterol_score",
  "Physical_Activity_score"
)

pretty_prs_labels <- c(
  "Full composite" = "Full composite",
  "bmi_score" = "BMI",
  "education_score" = "Education (risk-oriented)",
  "alcohol_drinking_weekly_score" = "Alcohol use",
  "smoking_initiation_score" = "Smoking initiation",
  "diabetes_score" = "Diabetes",
  "diastolic_blood_pressure_score" = "Diastolic BP",
  "systolic_blood_pressure_score" = "Systolic BP",
  "pulse_pressure_score" = "Pulse pressure",
  "total_cholesterol_score" = "Total cholesterol",
  "social_isolation_score" = "Social isolation",
  "Hearing_Loss_score" = "Hearing loss",
  "Vision_Loss_score" = "Vision loss",
  "Triglycerides_score" = "Triglycerides",
  "hdl_risk_score" = "HDL (risk-oriented)",
  "LDL_Cholesterol_score" = "LDL cholesterol",
  "Physical_Activity_score" = "Physical activity",
  "Major_Depressive_Disorder_score" = "Major depression"
)

sex_colors <- c(
  "Total" = "grey50",
  "Female" = "#fb6f92",
  "Male" = "#5aa9e6"
)

pretty_names <- function(df) {
  rename_map <- c(
    sex_strata = "Sex Stratum",
    group = "Group",
    dropped_prs = "Dropped PRS",
    estimate = "Estimate",
    conf.low = "CI Lower",
    conf.high = "CI Upper",
    p.value = "P Value",
    test_auc = "Test AUC",
    p_adj_fdr = "Adjusted P Value (FDR)",
    model = "Model",
    outcome = "Outcome",
    term = "Term",
    ID_2 = "Sample ID",
    IID = "IID",
    status_binary = "Status",
    cognitive_impairment = "Cognitive Impairment",
    aaoaae = "Age at Onset / Exam",
    apoe4any = "APOE4 Positivity",
    APOE4_Positivity = "APOE4 Positivity",
    comp_score_raw = "Composite Score (Raw)",
    comp_score_std = "Composite Score (Standardized)",
    train_score_sd = "Training Score SD",
    lambda_min = "Lambda Min",
    alpha = "Alpha",
    n_prs = "Number of PRSs",
    Race = "Race",
    Sex = "Sex",
    PC1 = "PC1",
    PC2 = "PC2",
    PC3 = "PC3",
    PC4 = "PC4"
  )

  current_names <- names(df)
  matched_old <- intersect(names(rename_map), current_names)
  names(df)[match(matched_old, names(df))] <- rename_map[matched_old]
  df
}

pretty_term_labels <- c(
  "bmi_score" = "BMI",
  "education_score" = "Education (Risk-Oriented)",
  "alcohol_drinking_weekly_score" = "Alcohol Use",
  "smoking_initiation_score" = "Smoking Initiation",
  "diabetes_score" = "Diabetes",
  "diastolic_blood_pressure_score" = "Diastolic Blood Pressure",
  "systolic_blood_pressure_score" = "Systolic Blood Pressure",
  "pulse_pressure_score" = "Pulse Pressure",
  "total_cholesterol_score" = "Total Cholesterol",
  "Major_Depressive_Disorder_score" = "Major Depression",
  "social_isolation_score" = "Social Isolation",
  "Hearing_Loss_score" = "Hearing Loss",
  "Vision_Loss_score" = "Vision Loss",
  "Triglycerides_score" = "Triglycerides",
  "hdl_risk_score" = "HDL (Risk-Oriented)",
  "LDL_Cholesterol_score" = "LDL Cholesterol",
  "Physical_Activity_score" = "Physical Activity",
  "bmi_score_std" = "BMI",
  "education_score_std" = "Education (Risk-Oriented)",
  "alcohol_drinking_weekly_score_std" = "Alcohol Use",
  "smoking_initiation_score_std" = "Smoking Initiation",
  "diabetes_score_std" = "Diabetes",
  "diastolic_blood_pressure_score_std" = "Diastolic Blood Pressure",
  "systolic_blood_pressure_score_std" = "Systolic Blood Pressure",
  "pulse_pressure_score_std" = "Pulse Pressure",
  "total_cholesterol_score_std" = "Total Cholesterol",
  "social_isolation_score_std" = "Social Isolation",
  "Hearing_Loss_score_std" = "Hearing Loss",
  "Vision_Loss_score_std" = "Vision Loss",
  "Triglycerides_score_std" = "Triglycerides",
  "hdl_risk_score_std" = "HDL (Risk-Oriented)",
  "LDL_Cholesterol_score_std" = "LDL Cholesterol",
  "Physical_Activity_score_std" = "Physical Activity",
  "comp_score_std" = "Composite Score (Standardized)",
  "Full composite" = "Full Composite"
)

relabel_term_column <- function(df, col_name = "Term") {
  if (col_name %in% names(df)) {
    df[[col_name]] <- dplyr::recode(
      as.character(df[[col_name]]),
      !!!pretty_term_labels,
      .default = as.character(df[[col_name]])
    )
  }
  df
}

relabel_dropped_prs_column <- function(df, col_name = "Dropped PRS") {
  if (col_name %in% names(df)) {
    df[[col_name]] <- dplyr::recode(
      as.character(df[[col_name]]),
      !!!pretty_term_labels,
      .default = as.character(df[[col_name]])
    )
  }
  df
}

relabel_outcome_column <- function(df, col_name = "Outcome") {
  outcome_labels <- c(
    "nfl" = "NfL",
    "tau" = "Total Tau",
    "ptau181" = "pTau181",
    "ptau217" = "pTau217",
    "ab42_ab40" = "Aβ42/Aβ40"
  )

  if (col_name %in% names(df)) {
    df[[col_name]] <- dplyr::recode(
      as.character(df[[col_name]]),
      !!!outcome_labels,
      .default = as.character(df[[col_name]])
    )
  }
  df
}

sig_stars <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ ""
  )
}

format_p_with_stars <- function(p) {
  stars <- sig_stars(p)
  base <- dplyr::case_when(
    is.na(p) ~ NA_character_,
    p < 0.001 ~ formatC(p, format = "e", digits = 2),
    TRUE ~ formatC(p, format = "f", digits = 3)
  )
  ifelse(is.na(base), NA_character_, paste0(base, stars))
}

append_stars_to_pcol <- function(df, p_col) {
  if (p_col %in% names(df)) {
    numeric_p <- suppressWarnings(as.numeric(df[[p_col]]))
    df[[p_col]] <- format_p_with_stars(numeric_p)
  }
  df
}

drop_helper_numeric_cols <- function(df) {
  df[, !grepl(" Numeric$", names(df)), drop = FALSE]
}
