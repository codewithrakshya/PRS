# =========================================================
# Export helpers
# =========================================================

write_highlight_values_sheet <- function(
  wb,
  sheet_name,
  df,
  numeric_p_col,
  value_col,
  p_cutoff = 0.05,
  display_p_col = NULL,
  also_bold_p = TRUE,
  title_text = NULL,
  subtitle_text = NULL
) {
  openxlsx::addWorksheet(wb, sheet_name)

  title_style <- openxlsx::createStyle(
    textDecoration = "bold",
    fontSize = 14,
    halign = "left"
  )

  subtitle_style <- openxlsx::createStyle(
    textDecoration = "italic",
    fontSize = 11,
    wrapText = TRUE,
    halign = "left"
  )

  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    halign = "center",
    border = "Bottom"
  )

  sig_style <- openxlsx::createStyle(textDecoration = "bold")

  start_row <- 1

  if (!is.null(title_text)) {
    openxlsx::writeData(wb, sheet = sheet_name, x = title_text, startRow = start_row, startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, sheet = sheet_name, style = title_style, rows = start_row, cols = 1, stack = TRUE)
    start_row <- start_row + 1
  }

  if (!is.null(subtitle_text)) {
    openxlsx::writeData(wb, sheet = sheet_name, x = subtitle_text, startRow = start_row, startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, sheet = sheet_name, style = subtitle_style, rows = start_row, cols = 1, stack = TRUE)
    start_row <- start_row + 2
  }

  openxlsx::writeData(wb, sheet = sheet_name, x = df, startRow = start_row, startCol = 1, withFilter = TRUE)

  openxlsx::addStyle(
    wb, sheet = sheet_name, style = header_style,
    rows = start_row, cols = 1:ncol(df), gridExpand = TRUE, stack = TRUE
  )

  if (numeric_p_col %in% names(df) && value_col %in% names(df)) {
    sig_rows <- which(!is.na(df[[numeric_p_col]]) & df[[numeric_p_col]] < p_cutoff)

    if (length(sig_rows) > 0) {
      value_col_idx <- match(value_col, names(df))

      openxlsx::addStyle(
        wb, sheet = sheet_name, style = sig_style,
        rows = sig_rows + start_row, cols = value_col_idx,
        gridExpand = TRUE, stack = TRUE
      )

      if (also_bold_p && !is.null(display_p_col) && display_p_col %in% names(df)) {
        p_col_idx <- match(display_p_col, names(df))
        openxlsx::addStyle(
          wb, sheet = sheet_name, style = sig_style,
          rows = sig_rows + start_row, cols = p_col_idx,
          gridExpand = TRUE, stack = TRUE
        )
      }
    }
  }

  openxlsx::setColWidths(wb, sheet = sheet_name, cols = 1:ncol(df), widths = "auto")
  openxlsx::setRowHeights(wb, sheet = sheet_name, rows = 1:(start_row - 1), heights = "auto")
  openxlsx::freezePane(wb, sheet = sheet_name, firstActiveRow = start_row + 1)
}

write_plain_sheet <- function(
  wb,
  sheet_name,
  df,
  title_text = NULL,
  subtitle_text = NULL
) {
  openxlsx::addWorksheet(wb, sheet_name)

  title_style <- openxlsx::createStyle(
    textDecoration = "bold",
    fontSize = 14,
    halign = "left"
  )

  subtitle_style <- openxlsx::createStyle(
    textDecoration = "italic",
    fontSize = 11,
    wrapText = TRUE,
    halign = "left"
  )

  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    halign = "center",
    border = "Bottom"
  )

  start_row <- 1

  if (!is.null(title_text)) {
    openxlsx::writeData(wb, sheet = sheet_name, x = title_text, startRow = start_row, startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, sheet = sheet_name, style = title_style, rows = start_row, cols = 1, stack = TRUE)
    start_row <- start_row + 1
  }

  if (!is.null(subtitle_text)) {
    openxlsx::writeData(wb, sheet = sheet_name, x = subtitle_text, startRow = start_row, startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, sheet = sheet_name, style = subtitle_style, rows = start_row, cols = 1, stack = TRUE)
    start_row <- start_row + 2
  }

  openxlsx::writeData(wb, sheet = sheet_name, x = df, startRow = start_row, startCol = 1, withFilter = TRUE)

  openxlsx::addStyle(
    wb, sheet = sheet_name, style = header_style,
    rows = start_row, cols = 1:ncol(df), gridExpand = TRUE, stack = TRUE
  )

  openxlsx::setColWidths(wb, sheet = sheet_name, cols = 1:ncol(df), widths = "auto")
  openxlsx::setRowHeights(wb, sheet = sheet_name, rows = 1:(start_row - 1), heights = "auto")
  openxlsx::freezePane(wb, sheet = sheet_name, firstActiveRow = start_row + 1)
}

default_table_meta <- function() {
  list(
    "ADGC Plain Results" = list(
      title = "Table 1. ADGC plain logistic composite PRS results",
      subtitle = "Association of the standardized composite PRS with Alzheimer's disease in the ADGC test set using the plain logistic weighting framework."
    ),
    "ADGC Ridge Results" = list(
      title = "Table 2. ADGC ridge composite PRS results",
      subtitle = "Association of the standardized composite PRS with Alzheimer's disease in the ADGC test set using ridge-penalized weights."
    ),
    "ADGC Elastic Net" = list(
      title = "Table 3. ADGC elastic net composite PRS results",
      subtitle = "Association of the standardized composite PRS with Alzheimer's disease in the ADGC test set using elastic net-penalized weights."
    ),
    "ADGC Model Comparison" = list(
      title = "Table 4. ADGC model comparison",
      subtitle = "Comparison of plain logistic, ridge, and elastic net composite PRS models across total, female, and male strata in ADGC."
    ),
    "HABS-HD Main Results" = list(
      title = "Table 5. HABS-HD external validation results",
      subtitle = "External validation of the ADGC-trained composite PRS for cognitive impairment in HABS-HD across total, female, and male strata."
    )
  )
}

build_main_result_tables <- function(
  plain_results,
  ridge_results,
  enet_results,
  compare_results,
  hab_results
) {
  plain_results_pretty <- plain_results %>%
    pretty_names() %>%
    dplyr::rename(`Odds Ratio` = Estimate)

  ridge_results_pretty <- ridge_results %>%
    pretty_names() %>%
    dplyr::rename(`Odds Ratio` = Estimate)

  enet_results_pretty <- enet_results %>%
    pretty_names() %>%
    dplyr::rename(`Odds Ratio` = Estimate)

  compare_results_pretty <- compare_results %>%
    pretty_names() %>%
    dplyr::rename(`Odds Ratio` = Estimate)

  hab_results_pretty <- hab_results %>%
    pretty_names() %>%
    dplyr::rename(`Odds Ratio` = Estimate)

  plain_results_export <- plain_results_pretty %>%
    dplyr::mutate(`P Value Numeric` = as.numeric(`P Value`)) %>%
    append_stars_to_pcol("P Value")

  ridge_results_export <- ridge_results_pretty %>%
    dplyr::mutate(`P Value Numeric` = as.numeric(`P Value`)) %>%
    append_stars_to_pcol("P Value")

  enet_results_export <- enet_results_pretty %>%
    dplyr::mutate(`P Value Numeric` = as.numeric(`P Value`)) %>%
    append_stars_to_pcol("P Value")

  compare_results_export <- compare_results_pretty %>%
    dplyr::mutate(`P Value Numeric` = as.numeric(`P Value`)) %>%
    append_stars_to_pcol("P Value")

  hab_results_export <- hab_results_pretty %>%
    dplyr::mutate(`P Value Numeric` = as.numeric(`P Value`)) %>%
    append_stars_to_pcol("P Value")

  list(
    plain = plain_results_export,
    ridge = ridge_results_export,
    enet = enet_results_export,
    compare = compare_results_export,
    hab = hab_results_export
  )
}

export_main_results_xlsx <- function(
  plain_results,
  ridge_results,
  enet_results,
  compare_results,
  hab_results,
  output_file
) {
  wb <- openxlsx::createWorkbook()
  meta <- default_table_meta()

  tabs <- build_main_result_tables(
    plain_results = plain_results,
    ridge_results = ridge_results,
    enet_results = enet_results,
    compare_results = compare_results,
    hab_results = hab_results
  )

  write_highlight_values_sheet(
    wb, "ADGC Plain Results", drop_helper_numeric_cols(tabs$plain),
    numeric_p_col = "P Value Numeric",
    value_col = "Odds Ratio",
    display_p_col = "P Value",
    p_cutoff = 0.05,
    title_text = meta[["ADGC Plain Results"]]$title,
    subtitle_text = meta[["ADGC Plain Results"]]$subtitle
  )

  write_highlight_values_sheet(
    wb, "ADGC Ridge Results", drop_helper_numeric_cols(tabs$ridge),
    numeric_p_col = "P Value Numeric",
    value_col = "Odds Ratio",
    display_p_col = "P Value",
    p_cutoff = 0.05,
    title_text = meta[["ADGC Ridge Results"]]$title,
    subtitle_text = meta[["ADGC Ridge Results"]]$subtitle
  )

  write_highlight_values_sheet(
    wb, "ADGC Elastic Net", drop_helper_numeric_cols(tabs$enet),
    numeric_p_col = "P Value Numeric",
    value_col = "Odds Ratio",
    display_p_col = "P Value",
    p_cutoff = 0.05,
    title_text = meta[["ADGC Elastic Net"]]$title,
    subtitle_text = meta[["ADGC Elastic Net"]]$subtitle
  )

  write_highlight_values_sheet(
    wb, "ADGC Model Comparison", drop_helper_numeric_cols(tabs$compare),
    numeric_p_col = "P Value Numeric",
    value_col = "Odds Ratio",
    display_p_col = "P Value",
    p_cutoff = 0.05,
    title_text = meta[["ADGC Model Comparison"]]$title,
    subtitle_text = meta[["ADGC Model Comparison"]]$subtitle
  )

  write_highlight_values_sheet(
    wb, "HABS-HD Main Results", drop_helper_numeric_cols(tabs$hab),
    numeric_p_col = "P Value Numeric",
    value_col = "Odds Ratio",
    display_p_col = "P Value",
    p_cutoff = 0.05,
    title_text = meta[["HABS-HD Main Results"]]$title,
    subtitle_text = meta[["HABS-HD Main Results"]]$subtitle
  )

  outdir <- dirname(output_file)
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  openxlsx::saveWorkbook(wb, file = output_file, overwrite = TRUE)

  invisible(output_file)
}
