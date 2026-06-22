#!/usr/bin/env bash
set -Eeuo pipefail

# =========================
# HABS-HD / PRS master runner
# =========================

# -------- paths --------
BUILD_QMD="/wynton/group/andrews/users/rakshyasharma/PRS/shared_data/build_habshd_master_dataset.qmd"
COMPOSITE_QMD="/wynton/group/andrews/users/rakshyasharma/PRS/compositePRS/compositePRS.qmd"
PHENO_QMD="/wynton/group/andrews/users/rakshyasharma/PRS/IndividualPRS/HABSHD/habshd_prs_phenotype_validation.qmd"
HABS_INDIV_QMD="/wynton/group/andrews/users/rakshyasharma/PRS/IndividualPRS/HABSHD/habshd_individual_prs.qmd"
ADGC_INDIV_QMD="/wynton/group/andrews/users/rakshyasharma/PRS/IndividualPRS/ADGC/adgc_individual_prs.qmd"
MAIN_QMD="/wynton/group/andrews/users/rakshyasharma/PRS/compositePRS/main.qmd"

MASTER_RDS="/wynton/group/andrews/users/rakshyasharma/PRS/shared_data/habshd_master_df.rds"
COMPOSITE_ENDO_RDS="/wynton/group/andrews/users/rakshyasharma/PRS/compositePRS/results/rds/endo_results.rds"
COMPOSITE_PLAIN_RDS="/wynton/group/andrews/users/rakshyasharma/PRS/compositePRS/results/rds/plain_results.rds"
ADGC_INDIV_RDS="/wynton/group/andrews/users/rakshyasharma/PRS/IndividualPRS/ADGC/results/rds/adgc_individual_prs_results.rds"
COMBINED_TABLE1_RMD="/wynton/group/andrews/users/rakshyasharma/PRS/tables/combined_table1.Rmd"
COMBINED_TABLE1_RDS="/wynton/group/andrews/users/rakshyasharma/PRS/tables/results/rds/combined_table1_gtsummary.rds"

LOG_DIR="/wynton/group/andrews/users/rakshyasharma/PRS/logs/render_logs"
mkdir -p "$LOG_DIR"

TIMESTAMP="$(date +%Y%m%d_%H%M%S)"
MASTER_LOG="$LOG_DIR/master_run_${TIMESTAMP}.log"

# -------- helpers --------
msg() {
  echo "[$(date '+%F %T')] $*" | tee -a "$MASTER_LOG"
}

fail() {
  msg "ERROR: $*"
  exit 1
}

check_file_exists() {
  local f="$1"
  [[ -f "$f" ]] || fail "Missing file: $f"
  msg "OK exists: $f"
}

render_qmd() {
  local qmd="$1"
  local name="$2"
  local log_file="$LOG_DIR/${name}_${TIMESTAMP}.log"

  msg "Rendering: $qmd"
  R -q -e "rmarkdown::render('$qmd')" >"$log_file" 2>&1 || {
    msg "FAILED: $qmd"
    msg "See log: $log_file"
    tail -n 50 "$log_file" | tee -a "$MASTER_LOG"
    exit 1
  }
  msg "SUCCESS: $qmd"
  msg "Log: $log_file"
}

check_rds_readable() {
  local rds="$1"
  local label="$2"
  check_file_exists "$rds"
  Rscript -e "x <- readRDS('$rds'); cat('$label rows:', nrow(x), '\n'); cat('$label cols:', ncol(x), '\n')" >>"$MASTER_LOG" 2>&1 \
    || fail "Could not read RDS: $rds"
  msg "OK readable RDS: $rds"
}

# -------- preflight: qmd existence --------
msg "Starting master PRS pipeline run"
msg "Checking QMD files..."

check_file_exists "$BUILD_QMD"
check_file_exists "$COMPOSITE_QMD"
check_file_exists "$PHENO_QMD"
check_file_exists "$HABS_INDIV_QMD"
check_file_exists "$ADGC_INDIV_QMD"
check_file_exists "$MAIN_QMD"
check_file_exists "$COMBINED_TABLE1_RMD"
# -------- step 1 --------
msg "STEP 1: build_habshd_master_dataset"
render_qmd "$BUILD_QMD" "01_build_habshd_master_dataset"
check_rds_readable "$MASTER_RDS" "habshd_master_df"

# -------- step 2 --------
msg "STEP 2: compositePRS"
render_qmd "$COMPOSITE_QMD" "02_compositePRS"
check_rds_readable "$COMPOSITE_ENDO_RDS" "composite_endo_results"
check_rds_readable "$COMPOSITE_PLAIN_RDS" "composite_plain_results"
# -------- step 2b --------
msg "STEP 2b: combined_adgc_habshd_table1"
render_qmd "$COMBINED_TABLE1_RMD" "02b_combined_adgc_habshd_table1"
check_rds_readable "$COMBINED_TABLE1_RDS" "combined_table1_gtsummary"
# -------- step 3 --------
msg "STEP 3: habshd_prs_phenotype_validation"
render_qmd "$PHENO_QMD" "03_habshd_prs_phenotype_validation"

# -------- step 4 --------
msg "STEP 4: habshd_individual_prs"
render_qmd "$HABS_INDIV_QMD" "04_habshd_individual_prs"

# -------- step 5 --------
msg "STEP 5: adgc_individual_prs"
render_qmd "$ADGC_INDIV_QMD" "05_adgc_individual_prs"
check_rds_readable "$ADGC_INDIV_RDS" "adgc_individual_prs_results"

# -------- step 6 --------
msg "STEP 6: compositePRS main figure"
render_qmd "$MAIN_QMD" "06_compositePRS_main"

msg "Pipeline completed successfully"
msg "Master log: $MASTER_LOG"
