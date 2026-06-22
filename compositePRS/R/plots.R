# =========================================================
# Plot functions
# =========================================================

plot_adgc_model_comparison <- function(compare_results) {
  ggplot2::ggplot(
    compare_results,
    ggplot2::aes(x = sex_strata, y = estimate, ymin = conf.low, ymax = conf.high, color = model)
  ) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
    ggplot2::geom_pointrange(position = ggplot2::position_dodge(width = 0.45), size = 0.7) +
    ggplot2::labs(
      x = NULL,
      y = "Odds Ratio per SD (95% CI)",
      title = "Composite PRS association with AD: plain logistic vs ridge vs elastic net"
    ) +
    ggplot2::theme_bw(base_size = 12)
}

plot_hab_main_results <- function(hab_results) {
  hab_results_plot <- hab_results %>%
    dplyr::mutate(group = factor(group, levels = c("Total", "Female", "Male")))

  ggplot2::ggplot(
    hab_results_plot,
    ggplot2::aes(x = group, y = estimate, ymin = conf.low, ymax = conf.high)
  ) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
    ggplot2::geom_pointrange(size = 0.7) +
    ggplot2::labs(
      x = NULL,
      y = "Odds Ratio per SD (95% CI)",
      title = "External validation in HABS-HD: composite PRS and cognitive impairment"
    ) +
    ggplot2::theme_bw(base_size = 12)
}

plot_endophenotypes <- function(endo_results) {
  endo_plot_df <- endo_results %>%
    dplyr::mutate(
      outcome = factor(outcome, levels = c("nfl", "tau", "ptau181", "ptau217", "ab42_ab40")),
      group = factor(group, levels = c("Total", "Female", "Male"))
    )

  ggplot2::ggplot(
    endo_plot_df,
    ggplot2::aes(x = outcome, y = estimate, ymin = conf.low, ymax = conf.high, color = group)
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    ggplot2::geom_pointrange(position = ggplot2::position_dodge(width = 0.5), size = 0.6) +
    ggplot2::scale_color_manual(values = sex_colors) +
    ggplot2::labs(
      x = NULL,
      y = "Beta (95% CI)",
      color = NULL,
      title = "Composite PRS association with AD endophenotypes"
    ) +
    ggplot2::theme_bw(base_size = 12)
}

plot_loo_adgc <- function(loo_adgc_plot_df) {
  ggplot2::ggplot(
    loo_adgc_plot_df,
    ggplot2::aes(x = dropped_prs, y = estimate, ymin = conf.low, ymax = conf.high, color = sex_strata)
  ) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
    ggplot2::geom_pointrange(position = ggplot2::position_dodge(width = 0.5), size = 0.6) +
    ggplot2::coord_flip() +
    ggplot2::scale_color_manual(values = sex_colors) +
    ggplot2::scale_x_discrete(labels = pretty_prs_labels) +
    ggplot2::labs(
      x = "PRS dropped from composite",
      y = "Odds Ratio per SD (95% CI)",
      color = NULL,
      title = "Leave-one-out sensitivity analysis in ADGC"
    ) +
    ggplot2::theme_bw(base_size = 12)
}

plot_loo_hab <- function(loo_hab_plot_df) {
  ggplot2::ggplot(
    loo_hab_plot_df,
    ggplot2::aes(x = dropped_prs, y = estimate, ymin = conf.low, ymax = conf.high, color = group)
  ) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
    ggplot2::geom_pointrange(position = ggplot2::position_dodge(width = 0.5), size = 0.6) +
    ggplot2::coord_flip() +
    ggplot2::scale_color_manual(values = sex_colors) +
    ggplot2::scale_x_discrete(labels = pretty_prs_labels) +
    ggplot2::labs(
      x = "PRS dropped from composite",
      y = "Odds Ratio per SD (95% CI)",
      color = NULL,
      title = "Leave-one-out sensitivity analysis in HABS-HD"
    ) +
    ggplot2::theme_bw(base_size = 12)
}

plot_adgc_density <- function(test_data) {
  plot_adgc <- as.data.frame(test_data) %>%
    dplyr::mutate(
      status_label = ifelse(status_binary == 1, "AD case", "Control"),
      sex_label = factor(Sex, levels = c(1, 2), labels = c("Male", "Female"))
    )

  ggplot2::ggplot(plot_adgc, ggplot2::aes(x = comp_score_std, fill = status_label)) +
    ggplot2::geom_density(alpha = 0.35) +
    ggplot2::scale_fill_manual(values = c("AD case" = "#e8b7b2", "Control" = "#86cfd1")) +
    ggplot2::labs(
      x = "Composite PRS (standardized)",
      y = "Density",
      fill = NULL,
      title = "Distribution of composite PRS in ADGC test set"
    ) +
    ggplot2::theme_bw(base_size = 12)
}

plot_hab_density <- function(hab_data) {
  plot_hab <- hab_data %>%
    dplyr::mutate(
      ci_label = ifelse(cognitive_impairment == 1, "Cognitive impairment", "No impairment"),
      sex_label = factor(Sex, levels = c(1, 2), labels = c("Male", "Female"))
    )

  ggplot2::ggplot(plot_hab, ggplot2::aes(x = comp_score_std, fill = ci_label)) +
    ggplot2::geom_density(alpha = 0.35) +
    ggplot2::labs(
      x = "Composite PRS (standardized)",
      y = "Density",
      fill = NULL,
      title = "Distribution of composite PRS in HABS-HD"
    ) +
    ggplot2::theme_bw(base_size = 12)
}
