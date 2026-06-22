# =========================================================
# Model fitting and scoring
# =========================================================

standardize_prs_vars <- function(train_data, test_data, prs_vars) {
  means <- sapply(train_data[prs_vars], mean, na.rm = TRUE)
  sds   <- sapply(train_data[prs_vars], sd, na.rm = TRUE)

  train_std <- train_data
  test_std  <- test_data

  for (v in prs_vars) {
    new_name <- paste0(v, "_std")
    train_std[[new_name]] <- (train_std[[v]] - means[[v]]) / sds[[v]]
    test_std[[new_name]]  <- (test_std[[v]] - means[[v]]) / sds[[v]]
  }

  list(train = train_std, test = test_std)
}

fit_plain_cprs <- function(train_data, test_data, prs_vars,
                           sex_group = "Total", include_sex = TRUE) {

  train_data <- as.data.frame(train_data)
  test_data  <- as.data.frame(test_data)

  std_obj   <- standardize_prs_vars(train_data, test_data, prs_vars)
  train_std <- as.data.frame(std_obj$train)
  test_std  <- as.data.frame(std_obj$test)

  prs_std_vars <- paste0(prs_vars, "_std")

  covars <- c("aaoaae", "apoe4any", "PC1", "PC2", "PC3", "PC4")
  if (include_sex) covars <- c(covars, "Sex")

  train_formula <- stats::as.formula(
    paste("status_binary ~", paste(c(prs_std_vars, covars), collapse = " + "))
  )

  fit <- stats::glm(train_formula, data = train_std, family = stats::binomial())

  prs_betas <- stats::coef(fit)[prs_std_vars]
  prs_betas <- prs_betas[!is.na(prs_betas)]

  prs_std_vars_keep <- names(prs_betas)

  train_x <- train_std %>% dplyr::select(dplyr::all_of(prs_std_vars_keep)) %>% as.matrix()
  test_x  <- test_std  %>% dplyr::select(dplyr::all_of(prs_std_vars_keep)) %>% as.matrix()

  train_score <- as.numeric(train_x %*% prs_betas)
  test_score  <- as.numeric(test_x %*% prs_betas)

  score_sd <- stats::sd(train_score, na.rm = TRUE)

  train_std$comp_score_raw <- train_score
  test_std$comp_score_raw  <- test_score
  train_std$comp_score_std <- train_score / score_sd
  test_std$comp_score_std  <- test_score / score_sd

  final_formula <- stats::as.formula(
    paste("status_binary ~", paste(c("comp_score_std", covars), collapse = " + "))
  )

  final_fit <- stats::glm(final_formula, data = test_std, family = stats::binomial())

  result <- broom::tidy(final_fit, conf.int = TRUE, exponentiate = TRUE) %>%
    dplyr::filter(term == "comp_score_std") %>%
    dplyr::mutate(
      sex_strata = sex_group,
      model = "Plain logistic",
      train_score_sd = score_sd
    )

  auc_obj <- pROC::roc(test_std$status_binary, test_std$comp_score_raw, quiet = TRUE)
  auc_val <- as.numeric(pROC::auc(auc_obj))

  list(
    training_model = fit,
    test_model = final_fit,
    prs_weights = prs_betas,
    result = result %>% dplyr::mutate(test_auc = auc_val),
    train_data = train_std,
    test_data = test_std,
    prs_vars = prs_vars,
    prs_means = sapply(train_data[, prs_vars, drop = FALSE], mean, na.rm = TRUE),
    prs_sds   = sapply(train_data[, prs_vars, drop = FALSE], sd, na.rm = TRUE),
    train_score_sd = score_sd
  )
}

fit_penalized_cprs <- function(train_data, test_data, prs_vars,
                               alpha_value = 0,
                               sex_group = "Total",
                               include_sex = TRUE,
                               model_label = "Ridge") {

  covars <- c("aaoaae", "apoe4any", "PC1", "PC2", "PC3", "PC4")
  if (include_sex) covars <- c(covars, "Sex")

  model_vars_train <- c("ID_2", "status_binary", prs_vars, covars)
  model_vars_test  <- c("ID_2", "status_binary", prs_vars, covars)

  train_model_df <- train_data %>%
    dplyr::select(dplyr::all_of(model_vars_train)) %>%
    stats::na.omit()

  test_model_df <- test_data %>%
    dplyr::select(dplyr::all_of(model_vars_test)) %>%
    stats::na.omit()

  prs_means <- sapply(train_model_df[prs_vars], mean, na.rm = TRUE)
  prs_sds   <- sapply(train_model_df[prs_vars], sd, na.rm = TRUE)

  for (v in prs_vars) {
    std_name <- paste0(v, "_std")
    train_model_df[[std_name]] <- (train_model_df[[v]] - prs_means[[v]]) / prs_sds[[v]]
    test_model_df[[std_name]]  <- (test_model_df[[v]]  - prs_means[[v]]) / prs_sds[[v]]
  }

  prs_std_vars <- paste0(prs_vars, "_std")

  full_formula <- stats::as.formula(
    paste("status_binary ~", paste(c(prs_std_vars, covars), collapse = " + "))
  )

  x_train <- stats::model.matrix(full_formula, data = train_model_df)[, -1, drop = FALSE]
  y_train <- train_model_df$status_binary

  set.seed(1234)
  cvfit <- glmnet::cv.glmnet(
    x = x_train,
    y = y_train,
    family = "binomial",
    alpha = alpha_value,
    nfolds = 10,
    standardize = FALSE
  )

  coef_mat <- stats::coef(cvfit, s = "lambda.min")
  coef_df <- data.frame(
    term = rownames(as.matrix(coef_mat)),
    estimate = as.numeric(coef_mat),
    stringsAsFactors = FALSE
  )

  prs_coef_df <- coef_df %>% dplyr::filter(term %in% prs_std_vars)

  prs_betas <- prs_coef_df$estimate
  names(prs_betas) <- prs_coef_df$term

  x_train_prs <- as.matrix(train_model_df[, names(prs_betas), drop = FALSE])
  x_test_prs  <- as.matrix(test_model_df[, names(prs_betas), drop = FALSE])

  train_lp <- as.numeric(x_train_prs %*% prs_betas)
  test_lp  <- as.numeric(x_test_prs %*% prs_betas)

  train_lp_sd <- stats::sd(train_lp, na.rm = TRUE)

  train_model_df$comp_score_raw <- train_lp
  test_model_df$comp_score_raw  <- test_lp
  train_model_df$comp_score_std <- train_lp / train_lp_sd
  test_model_df$comp_score_std  <- test_lp / train_lp_sd

  final_formula <- stats::as.formula(
    paste("status_binary ~", paste(c("comp_score_std", covars), collapse = " + "))
  )

  final_fit <- stats::glm(final_formula, data = test_model_df, family = stats::binomial())

  result <- broom::tidy(final_fit, conf.int = TRUE, exponentiate = TRUE) %>%
    dplyr::filter(term == "comp_score_std") %>%
    dplyr::mutate(
      sex_strata = sex_group,
      model = model_label,
      alpha = alpha_value,
      lambda_min = cvfit$lambda.min,
      train_score_sd = train_lp_sd
    )

  auc_obj <- pROC::roc(test_model_df$status_binary, test_model_df$comp_score_raw, quiet = TRUE)
  auc_val <- as.numeric(pROC::auc(auc_obj))

  coef_df <- coef_df %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::mutate(
      sex_strata = sex_group,
      model = model_label
    )

  list(
    cvfit = cvfit,
    final_fit = final_fit,
    result = result %>% dplyr::mutate(test_auc = auc_val),
    coefficients = coef_df,
    prs_only_coefficients = prs_coef_df %>%
      dplyr::mutate(sex_strata = sex_group, model = model_label),
    train_data = train_model_df,
    test_data = test_model_df
  )
}

score_external_habshd <- function(hab_data, model_obj, outcome_var = "cognitive_impairment",
                                  sex_subset = NULL, sex_group = "Total") {

  dat <- as.data.frame(hab_data)

  prs_vars <- model_obj$prs_vars
  prs_means <- model_obj$prs_means
  prs_sds <- model_obj$prs_sds
  train_score_sd <- model_obj$train_score_sd

  weight_tbl <- tibble::tibble(
    term = names(model_obj$prs_weights),
    estimate = as.numeric(model_obj$prs_weights)
  )

  for (v in prs_vars) {
    std_name <- paste0(v, "_std")
    dat[[std_name]] <- (dat[[v]] - prs_means[[v]]) / prs_sds[[v]]
  }

  prs_std_vars <- weight_tbl$term
  weight_vec <- weight_tbl$estimate
  names(weight_vec) <- prs_std_vars

  X <- dat %>% dplyr::select(dplyr::all_of(prs_std_vars)) %>% as.matrix()

  dat$comp_score_raw <- as.numeric(X %*% weight_vec)
  dat$comp_score_std <- dat$comp_score_raw / train_score_sd

  if (!is.null(sex_subset)) {
    dat <- dat %>% dplyr::filter(Sex == sex_subset)
  }

  base_covars <- c("comp_score_std", "Age", "APOE4_Positivity", "PC1", "PC2", "PC3", "PC4")

  if ("Race" %in% names(dat) && dplyr::n_distinct(dat$Race[!is.na(dat$Race)]) > 1) {
    base_covars <- c(base_covars, "Race")
  }

  if (sex_group == "Total") {
    base_covars <- c(base_covars, "Sex")
  }

  fit_formula <- stats::as.formula(
    paste(outcome_var, "~", paste(base_covars, collapse = " + "))
  )

  fit <- stats::glm(fit_formula, data = dat, family = stats::binomial())

  res <- broom::tidy(fit, conf.int = TRUE, exponentiate = TRUE) %>%
    dplyr::filter(term == "comp_score_std") %>%
    dplyr::mutate(group = sex_group)

  auc_val <- as.numeric(
    pROC::auc(
      pROC::roc(dat[[outcome_var]], dat$comp_score_raw, quiet = TRUE)
    )
  )

  list(
    fit = fit,
    data = dat,
    result = res %>% dplyr::mutate(test_auc = auc_val)
  )
}

run_endophenotype_models <- function(hab_total_scored, hab_female_scored, hab_male_scored,
                                     endo_outcomes = c("ptau181", "ptau217", "ab42_ab40", "nfl", "tau")) {

  endo_results_total <- purrr::map_dfr(endo_outcomes, function(outcome) {
    fit <- stats::lm(
      stats::as.formula(paste(outcome, "~ comp_score_std + Age + APOE4_Positivity + PC1 + PC2 + PC3 + PC4 + Sex + Race")),
      data = hab_total_scored
    )

    broom::tidy(fit, conf.int = TRUE) %>%
      dplyr::filter(term == "comp_score_std") %>%
      dplyr::mutate(outcome = outcome, group = "Total")
  })

  endo_results_female <- purrr::map_dfr(endo_outcomes, function(outcome) {
    fit <- stats::lm(
      stats::as.formula(paste(outcome, "~ comp_score_std + Age + APOE4_Positivity + PC1 + PC2 + PC3 + PC4 + Race")),
      data = hab_female_scored
    )

    broom::tidy(fit, conf.int = TRUE) %>%
      dplyr::filter(term == "comp_score_std") %>%
      dplyr::mutate(outcome = outcome, group = "Female")
  })

  endo_results_male <- purrr::map_dfr(endo_outcomes, function(outcome) {
    fit <- stats::lm(
      stats::as.formula(paste(outcome, "~ comp_score_std + Age + APOE4_Positivity + PC1 + PC2 + PC3 + PC4 + Race")),
      data = hab_male_scored
    )

    broom::tidy(fit, conf.int = TRUE) %>%
      dplyr::filter(term == "comp_score_std") %>%
      dplyr::mutate(outcome = outcome, group = "Male")
  })

  dplyr::bind_rows(
    endo_results_total,
    endo_results_female,
    endo_results_male
  ) %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(p_adj_fdr = p.adjust(p.value, method = "fdr")) %>%
    dplyr::ungroup()
}

run_loo_plain_adgc <- function(train_df, test_df, prs_vars, include_sex = TRUE, sex_group = "Total") {
  loo_list <- lapply(prs_vars, function(drop_var) {
    current_prs <- setdiff(prs_vars, drop_var)

    fit_obj <- fit_plain_cprs(
      train_data = train_df,
      test_data = test_df,
      prs_vars = current_prs,
      sex_group = sex_group,
      include_sex = include_sex
    )

    fit_obj$result %>%
      dplyr::mutate(
        dropped_prs = drop_var,
        n_prs = length(current_prs)
      )
  })

  dplyr::bind_rows(loo_list)
}

run_loo_habshd <- function(train_df, test_df, hab_data, prs_vars) {
  loo_list <- lapply(prs_vars, function(drop_var) {
    current_prs <- setdiff(prs_vars, drop_var)

    adgc_fit <- fit_plain_cprs(
      train_data = train_df,
      test_data = test_df,
      prs_vars = current_prs,
      sex_group = "Total",
      include_sex = TRUE
    )

    hab_fit <- score_external_habshd(
      hab_data = hab_data,
      model_obj = adgc_fit,
      outcome_var = "cognitive_impairment",
      sex_subset = NULL,
      sex_group = "Total"
    )

    hab_fit$result %>%
      dplyr::mutate(
        dropped_prs = drop_var,
        n_prs = length(current_prs)
      )
  })

  dplyr::bind_rows(loo_list)
}

run_loo_habshd_sex <- function(train_df, test_df, hab_data, prs_vars, sex_code, sex_group) {
  loo_list <- lapply(prs_vars, function(drop_var) {
    current_prs <- setdiff(prs_vars, drop_var)

    adgc_fit <- fit_plain_cprs(
      train_data = train_df,
      test_data = test_df,
      prs_vars = current_prs,
      sex_group = sex_group,
      include_sex = FALSE
    )

    hab_fit <- score_external_habshd(
      hab_data = hab_data,
      model_obj = adgc_fit,
      outcome_var = "cognitive_impairment",
      sex_subset = sex_code,
      sex_group = sex_group
    )

    hab_fit$result %>%
      dplyr::mutate(
        dropped_prs = drop_var,
        n_prs = length(current_prs)
      )
  })

  dplyr::bind_rows(loo_list)
}
