# =========================================================
# Data loading and merging
# =========================================================

load_required_packages <- function() {
  suppressPackageStartupMessages({
    library(yaml)
    library(readr)
    library(dplyr)
    library(data.table)
    library(purrr)
    library(readxl)
    library(tidyr)
  })
}

load_project_config <- function(path = "config.yml") {
  yaml::read_yaml(path)
}

extract_prs_adgc <- function(fn, base_dir, trait_labels) {
  prs_file <- file.path(base_dir, fn, "adgc", "score", "adgc_pgs.txt.gz")

  prs_data <- read.table(prs_file, header = TRUE) %>%
    dplyr::mutate(IID = clean_id(IID)) %>%
    dplyr::distinct(IID, .keep_all = TRUE) %>%
    dplyr::select(IID, Z_norm2)

  colnames(prs_data)[2] <- paste0(trait_labels[[fn]], "_score")
  prs_data
}

extract_prs_hab <- function(fn, hab_base_dir, hab_trait_labels) {
  prs_file <- file.path(hab_base_dir, fn, "habshd", "score", "habshd_pgs.txt.gz")

  if (!file.exists(prs_file)) {
    message("Missing file: ", prs_file)
    return(NULL)
  }

  prs_data <- readr::read_tsv(prs_file, show_col_types = FALSE) %>%
    dplyr::select(IID, Z_norm2) %>%
    dplyr::mutate(IID = as.character(IID)) %>%
    dplyr::distinct(IID, .keep_all = TRUE)

  colnames(prs_data)[2] <- paste0(hab_trait_labels[[fn]], "_score")
  prs_data
}

load_adgc_data <- function(fam_file, covar_file, base_dir, folder_names, trait_labels, all_prs_vars) {
  fam_data <- read.table(
    fam_file,
    header = FALSE,
    col.names = c("FID", "IID", "PID", "MID", "Sex", "P")
  ) %>%
    dplyr::mutate(IID = clean_id(IID)) %>%
    dplyr::distinct(IID, .keep_all = TRUE)

  covar_data <- read.table(covar_file, header = TRUE, sep = "\t") %>%
    dplyr::mutate(ID_2 = trimws(as.character(ID_2))) %>%
    dplyr::distinct(ID_2, .keep_all = TRUE)

  pca_file <- file.path(base_dir, folder_names[1], "adgc", "score", "adgc_popsimilarity.txt.gz")
  pca_data <- readr::read_tsv(pca_file, show_col_types = FALSE) %>%
    dplyr::mutate(IID = clean_id(IID)) %>%
    dplyr::distinct(IID, .keep_all = TRUE) %>%
    dplyr::select(IID, MostSimilarPop, PC1, PC2, PC3, PC4)

  prs_list <- purrr::map(folder_names, extract_prs_adgc, base_dir = base_dir, trait_labels = trait_labels)
  prs_wide <- purrr::reduce(prs_list, dplyr::full_join, by = "IID")

  adgc_df <- covar_data %>%
    dplyr::left_join(fam_data, by = c("ID_2" = "IID")) %>%
    dplyr::left_join(pca_data, by = c("ID_2" = "IID")) %>%
    dplyr::left_join(prs_wide, by = c("ID_2" = "IID")) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      dplyr::across(where(is.numeric), ~ dplyr::na_if(., -9)),
      status_binary = dplyr::if_else(status == 2, 1, 0),
      status_binary = as.numeric(status_binary),
      Sex = as.numeric(Sex),
      sex = as.numeric(sex),
      aaoaae = as.numeric(aaoaae),
      apoe4any = as.numeric(apoe4any),
      PC1 = as.numeric(PC1),
      PC2 = as.numeric(PC2),
      PC3 = as.numeric(PC3),
      PC4 = as.numeric(PC4)
    ) %>%
    dplyr::filter(status %in% c(1, 2)) %>%
    dplyr::mutate(
      education_risk_score = -education_score,
      hdl_risk_score = -HDL_Cholesterol_score
    )

  adgc_df_prs <- adgc_df %>%
    dplyr::filter(
      !is.na(status_binary),
      !is.na(aaoaae),
      !is.na(apoe4any),
      !is.na(PC1), !is.na(PC2), !is.na(PC3), !is.na(PC4),
      !is.na(Sex)
    ) %>%
    dplyr::filter(dplyr::if_all(dplyr::all_of(all_prs_vars), ~ !is.na(.)))

  adgc_df_prs
  
}

load_habshd_data <- function(
  hab_fam_file,
  hab_pop_file,
  hab_clinical_file,
  hab_genomics_file,
  hab_base_dir,
  hab_merged_rds,
  hab_folder_names,
  hab_trait_labels,
  all_prs_vars
) {
  hab_fam <- data.table::fread(hab_fam_file, header = FALSE)
  colnames(hab_fam) <- c("FID", "IID", "PID", "MID", "Sex", "PHENO")
  hab_fam <- hab_fam %>%
    dplyr::mutate(IID = as.character(IID)) %>%
    dplyr::distinct(IID, .keep_all = TRUE)

  hab_population <- readr::read_tsv(hab_pop_file, show_col_types = FALSE) %>%
    dplyr::select(IID, MostSimilarPop, PC1, PC2, PC3, PC4) %>%
    dplyr::mutate(IID = as.character(IID)) %>%
    dplyr::distinct(IID, .keep_all = TRUE)

  hab_clinical <- readr::read_csv(hab_clinical_file, show_col_types = FALSE) %>%
    dplyr::transmute(
      IID = as.character(as.integer(Med_ID)),
      CDX_Cog = dplyr::na_if(CDX_Cog, 9),
      Visit_ID = Visit_ID
    ) %>%
    dplyr::arrange(IID, Visit_ID) %>%
    dplyr::distinct(IID, .keep_all = TRUE)

  hab_genomics <- readxl::read_excel(hab_genomics_file, sheet = "Sheet1") %>%
    dplyr::rename(IID = Med_ID) %>%
    dplyr::mutate(IID = as.character(IID)) %>%
    dplyr::select(IID, APOE4_Positivity) %>%
    dplyr::distinct(IID, .keep_all = TRUE)

  hab_x <- readRDS(hab_merged_rds)

  hab_outcomes <- hab_x %>%
    dplyr::select(IID, cdx_ci, ptau181, ptau217, ab42_ab40, nfl, tau, Age = age, sex, Race = race) %>%
    dplyr::mutate(IID = as.character(IID)) %>%
    dplyr::distinct(IID, .keep_all = TRUE)

  hab_prs_list <- purrr::map(
    hab_folder_names,
    extract_prs_hab,
    hab_base_dir = hab_base_dir,
    hab_trait_labels = hab_trait_labels
  )

  hab_prs_list <- hab_prs_list[!purrr::map_lgl(hab_prs_list, is.null)]
  hab_prs_wide <- purrr::reduce(hab_prs_list, dplyr::full_join, by = "IID")

  habshd_df <- hab_fam %>%
    dplyr::left_join(hab_outcomes,   by = "IID") %>%
    dplyr::left_join(hab_clinical,   by = "IID") %>%
    dplyr::left_join(hab_population, by = "IID") %>%
    dplyr::left_join(hab_prs_wide,   by = "IID") %>%
    dplyr::left_join(hab_genomics,   by = "IID") %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      Sex = as.numeric(Sex),
      Age = as.numeric(Age),
      APOE4_Positivity = as.numeric(APOE4_Positivity),
      PC1 = as.numeric(PC1),
      PC2 = as.numeric(PC2),
      PC3 = as.numeric(PC3),
      PC4 = as.numeric(PC4),
      cdx_ci = as.numeric(cdx_ci),
      Race = as.factor(Race),
      cognitive_impairment = dplyr::case_when(
        is.na(cdx_ci) ~ NA_real_,
        cdx_ci == 1 ~ 1,
        TRUE ~ 0
      )
    ) %>%
    dplyr::mutate(
      education_risk_score = -education_score,
      hdl_risk_score = -HDL_Cholesterol_score
    )

  habshd_eval <- habshd_df %>%
    dplyr::filter(
      !is.na(cognitive_impairment),
      !is.na(Age),
      !is.na(APOE4_Positivity),
      !is.na(PC1), !is.na(PC2), !is.na(PC3), !is.na(PC4),
      !is.na(Sex),
      !is.na(Race)
    ) %>%
    dplyr::filter(dplyr::if_all(dplyr::all_of(all_prs_vars), ~ !is.na(.)))

  habshd_eval
}
