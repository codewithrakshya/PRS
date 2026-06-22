build_habshd_master_dataset <- function(
  fam_file,
  pop_file,
  clinical_file,
  genomics_file,
  meds_file,
  base_dir,
  trait_folders,
  trait_labels,
  biomarker_file
) {
  clean_iid <- function(x) {
    as.character(as.integer(as.character(x)))
  }

  read_population_data <- function(pop_file) {
    if (grepl("\\.gz$", pop_file)) {
      data.table::fread(
        cmd = paste("zcat", shQuote(pop_file)),
        select = c("IID", "MostSimilarPop", "PC1", "PC2", "PC3", "PC4")
      )
    } else {
      data.table::fread(
        pop_file,
        select = c("IID", "MostSimilarPop", "PC1", "PC2", "PC3", "PC4")
      )
    } %>%
      dplyr::mutate(IID = clean_iid(IID)) %>%
      dplyr::distinct(IID, .keep_all = TRUE)
  }

  read_medication_data <- function(meds_file) {
    readr::read_csv(meds_file, show_col_types = FALSE) %>%
      dplyr::rename(Med_ID = med_id) %>%
      dplyr::transmute(
        IID = clean_iid(Med_ID),
        taking_cholesterol_meds = ifelse(toupper(trimws(taking_cholesterol_meds)) == "YES", 1L, 0L),
        taking_diabetes_meds = ifelse(toupper(trimws(taking_diabetes_meds)) == "YES", 1L, 0L),
        taking_hypertension_meds = ifelse(toupper(trimws(taking_hypertension_meds)) == "YES", 1L, 0L)
      ) %>%
      dplyr::group_by(IID) %>%
      dplyr::summarise(
        taking_cholesterol_meds = as.integer(max(taking_cholesterol_meds, na.rm = TRUE)),
        taking_diabetes_meds = as.integer(max(taking_diabetes_meds, na.rm = TRUE)),
        taking_hypertension_meds = as.integer(max(taking_hypertension_meds, na.rm = TRUE)),
        .groups = "drop"
      )
  }

  extract_z_scores <- function(folder, base_dir) {
    score_file <- file.path(base_dir, folder, "habshd", "score", "habshd_pgs.txt.gz")

    if (!file.exists(score_file)) return(NULL)

    df <- if (grepl("\\.gz$", score_file)) {
      data.table::fread(
        cmd = paste("zcat", shQuote(score_file)),
        select = c("IID", "Z_norm2")
      )
    } else {
      data.table::fread(
        score_file,
        select = c("IID", "Z_norm2")
      )
    }

    df %>%
      dplyr::mutate(IID = clean_iid(IID)) %>%
      dplyr::distinct(IID, .keep_all = TRUE) %>%
      dplyr::rename(!!paste0(folder, "_score") := Z_norm2)
  }

  population <- read_population_data(pop_file)
  medications <- read_medication_data(meds_file)

  clinical <- readr::read_csv(clinical_file, show_col_types = FALSE) %>%
    dplyr::mutate(
      Med_ID = clean_iid(Med_ID),
      CDX_Cog = dplyr::na_if(CDX_Cog, 9),
      cognitive_impairment = dplyr::case_when(
        CDX_Cog == 0 ~ 0,
        CDX_Cog %in% c(1, 2) ~ 1,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::arrange(Med_ID, Visit_ID) %>%
    dplyr::distinct(Med_ID, .keep_all = TRUE)

  genomics <- readxl::read_excel(genomics_file, sheet = "Sheet1") %>%
    dplyr::rename(IID = Med_ID) %>%
    dplyr::mutate(IID = clean_iid(IID)) %>%
    dplyr::distinct(IID, .keep_all = TRUE)

  trait_score_list <- purrr::map(trait_folders, ~extract_z_scores(.x, base_dir))
  trait_score_list <- trait_score_list[!vapply(trait_score_list, is.null, logical(1))]
  trait_scores_merged <- purrr::reduce(trait_score_list, dplyr::full_join, by = "IID")

  names(trait_scores_merged) <- ifelse(
    names(trait_scores_merged) %in% paste0(names(trait_labels), "_score"),
    paste0(trait_labels[stringr::str_remove(names(trait_scores_merged), "_score")], "_score"),
    names(trait_scores_merged)
  )

  fam_df <- data.table::fread(fam_file, header = FALSE) %>%
    dplyr::select(V2) %>%
    dplyr::rename(IID = V2) %>%
    dplyr::mutate(IID = clean_iid(IID)) %>%
    dplyr::distinct(IID, .keep_all = TRUE)

  habs_raw <- clinical %>%
    dplyr::rename(IID = Med_ID)

habs <- habs_raw %>%
  dplyr::mutate(
    Race = dplyr::case_when(
      ID_Hispanic == 1 & ID_Race_Black == 0 & ID_Race_White == 0 ~ "Hispanic",
      ID_Race_Black == 1 ~ "Black",
      ID_Race_White == 1 & ID_Hispanic == 1 & ID_Race_Black == 0 ~ "Hispanic",
      TRUE ~ "NHW"
    ),

    GDS_Category = ifelse(
      GDS_Category %in% c("Normal", "Mild Depressive", "Severe Depressive"),
      GDS_Category,
      NA_character_
    ),

    CDX_Cog = dplyr::na_if(CDX_Cog, 9),

    BW_HbA1c = dplyr::na_if(BW_HBA1c, -9999.0),
    BW_LDLchol = dplyr::na_if(BW_LDLchol, -9999.0),
    BW_HDLChol = dplyr::na_if(BW_HDLChol, -9999.0),
    BW_Triglycerides = dplyr::na_if(BW_Triglycerides, -9999.0),

    OM_BP1_DIA = dplyr::na_if(OM_BP1_DIA, -9999),
    OM_BP2_DIA = dplyr::na_if(OM_BP2_DIA, -9999),
    OM_BP1_SYS = dplyr::na_if(OM_BP1_SYS, -9999),
    OM_BP2_SYS = dplyr::na_if(OM_BP2_SYS, -9999),
    OM_Pulse1  = dplyr::na_if(OM_Pulse1, -9999),
    OM_Pulse2  = dplyr::na_if(OM_Pulse2, -9999),

    OM_BP1_DIA = dplyr::na_if(OM_BP1_DIA, -999),
    OM_BP2_DIA = dplyr::na_if(OM_BP2_DIA, -999),
    OM_BP1_SYS = dplyr::na_if(OM_BP1_SYS, -999),
    OM_BP2_SYS = dplyr::na_if(OM_BP2_SYS, -999),
    OM_Pulse1  = dplyr::na_if(OM_Pulse1, -999),
    OM_Pulse2  = dplyr::na_if(OM_Pulse2, -999),

    AUDIT_Total = dplyr::if_else(AUDIT_Total == -9999, NA_real_, as.numeric(AUDIT_Total)),
    OM_BMI = dplyr::if_else(OM_BMI <= 0 | OM_BMI == -9999, NA_real_, as.numeric(OM_BMI)),
    BW_CholTotal = dplyr::if_else(BW_CholTotal < 0, NA_real_, as.numeric(BW_CholTotal)),
    CDX_Depression = dplyr::if_else(CDX_Depression < 0, NA_real_, as.numeric(CDX_Depression)),
    CDX_Diabetes = dplyr::if_else(CDX_Diabetes < 0, NA_real_, as.numeric(CDX_Diabetes)),
    CDX_Hypertension = dplyr::if_else(CDX_Hypertension < 0, NA_real_, as.numeric(CDX_Hypertension)),
    IMH_TBI = dplyr::if_else(IMH_TBI < 0, NA_real_, as.numeric(IMH_TBI)),
    SocialSupport_Total = dplyr::if_else(SocialSupport_Total < 0, NA_real_, as.numeric(SocialSupport_Total)),
    BW_LDLchol = dplyr::if_else(BW_LDLchol < 0, NA_real_, as.numeric(BW_LDLchol)),
    BW_HDLChol = dplyr::if_else(BW_HDLChol < 0, NA_real_, as.numeric(BW_HDLChol)),
    BW_Triglycerides = dplyr::if_else(BW_Triglycerides < 0, NA_real_, as.numeric(BW_Triglycerides)),
    RAPA_1_Total = dplyr::if_else(RAPA_1_Total == -9999, NA_real_, as.numeric(RAPA_1_Total)),
    RAPA_2_Total = dplyr::if_else(RAPA_2_Total == -9999, NA_real_, as.numeric(RAPA_2_Total)),

    Smoke = dplyr::case_when(
      Smoke_Currently == 1 & Smoke_Ever == 1 ~ "Smoker",
      Smoke_Currently == 0 & Smoke_Ever == 1 ~ "Smoker",
      Smoke_Currently == 0 & Smoke_Ever == 0 ~ "Never Smoked",
      Smoke_Currently == 1 & is.na(Smoke_Ever) ~ "Smoker",
      TRUE ~ NA_character_
    ),

    Smoke_binary = dplyr::case_when(
      Smoke == "Never Smoked" ~ 0,
      Smoke == "Smoker" ~ 1,
      TRUE ~ NA_real_
    ),

    Hearing_Loss = dplyr::case_when(
      Auditory_1 == 1 & Auditory_2 == 1 & (Auditory_3 %in% c(3, 4)) ~ "Hearing Loss",
      Auditory_1 == 0 & Auditory_2 == 0 & (Auditory_3 %in% c(1, 2)) ~ "No Hearing Loss",
      TRUE ~ NA_character_
    ),

    Hearing_Loss_binary = dplyr::case_when(
      Hearing_Loss == "Hearing Loss" ~ 1,
      Hearing_Loss == "No Hearing Loss" ~ 0,
      TRUE ~ NA_real_
    ),

    Vision_Loss = dplyr::case_when(
      Visual_1 == 1 & Visual_3 >= 2 ~ "Vision Loss",
      Visual_1 == 0 & Visual_3 == 1 ~ "No Vision Loss",
      TRUE ~ NA_character_
    ),

    Vision_Loss_binary = dplyr::case_when(
      Vision_Loss == "Vision Loss" ~ 1,
      Vision_Loss == "No Vision Loss" ~ 0,
      TRUE ~ NA_real_
    ),

    GDS_binary = dplyr::case_when(
      GDS_Category == "Normal" ~ 0,
      GDS_Category == "Mild Depressive" ~ 1,
      GDS_Category == "Severe Depressive" ~ 2,
      TRUE ~ NA_real_
    )
  ) %>%
  dplyr::mutate(
    OM_BP_DIA = rowMeans(dplyr::across(c(OM_BP1_DIA, OM_BP2_DIA)), na.rm = TRUE),
    OM_BP_SYS = rowMeans(dplyr::across(c(OM_BP1_SYS, OM_BP2_SYS)), na.rm = TRUE),
    OM_Pulse  = rowMeans(dplyr::across(c(OM_Pulse1, OM_Pulse2)), na.rm = TRUE),

    OM_BP_DIA = ifelse(is.nan(OM_BP_DIA), NA_real_, OM_BP_DIA),
    OM_BP_SYS = ifelse(is.nan(OM_BP_SYS), NA_real_, OM_BP_SYS),
    OM_Pulse  = ifelse(is.nan(OM_Pulse), NA_real_, OM_Pulse),

    Pulse_Pressure = ifelse(
      !is.na(OM_BP_SYS) & !is.na(OM_BP_DIA),
      OM_BP_SYS - OM_BP_DIA,
      NA_real_
    )
  )

  biomarker_raw <- readRDS(biomarker_file)

  visit_candidates <- c("visit_id", "Visit_ID", "visit", "VISIT")
  visit_var <- visit_candidates[visit_candidates %in% names(biomarker_raw)][1]

  biomarker_df <- biomarker_raw %>%
    dplyr::select(IID, dplyr::all_of(visit_var), ptau181, ptau217, ab42_ab40, nfl, tau) %>%
    dplyr::mutate(
      IID = clean_iid(IID),
      visit_value = suppressWarnings(as.numeric(.data[[visit_var]]))
    ) %>%
    dplyr::arrange(IID, visit_value) %>%
    dplyr::distinct(IID, .keep_all = TRUE) %>%
    dplyr::select(-visit_value)

  master_df <- fam_df %>%
    dplyr::left_join(habs, by = "IID") %>%
    dplyr::left_join(population, by = "IID") %>%
    dplyr::left_join(trait_scores_merged, by = "IID") %>%
    dplyr::left_join(medications, by = "IID") %>%
    dplyr::left_join(genomics %>% dplyr::select(IID, APOE4_Positivity), by = "IID") %>%
    dplyr::left_join(biomarker_df, by = "IID") %>%
    dplyr::mutate(
      APOE4_Positivity = as.integer(APOE4_Positivity),
      APOE4_Positivity = ifelse(APOE4_Positivity %in% c(0, 1), APOE4_Positivity, NA_integer_),
      education_risk_score = -education_score,
      hdl_risk_score = -HDL_Cholesterol_score
    ) %>%
    dplyr::distinct(IID, .keep_all = TRUE)

  master_df
}


