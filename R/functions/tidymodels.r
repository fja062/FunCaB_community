### Functions for mixed effects modeling

#' Compare full 3-way interaction model to 2-way interaction model using LRT
#' @param data Data frame
#' @param response Name of response variable (string)
#' @param predictor Name of main predictor (string)
#' @param random_effect Name of random effect grouping variable (string, default 'siteID')
#' @return List with both models and the anova table
compare_full_vs_2way_lmer <- function(data, response, predictor, random_effect = "siteID") {
  # Build formula strings
  # Full model: response ~ predictor * precipitation_scaled * temperature_scaled + (1|random_effect)
  full_formula <- as.formula(
    paste0(response, " ~ ", predictor, " * precipitation_scaled * temperature_scaled + (1|", random_effect, ")")
  )
  # 2-way model: response ~ predictor + precipitation_scaled + temperature_scaled + predictor:precipitation_scaled + predictor:temperature_scaled + precipitation_scaled:temperature_scaled + (1|random_effect)
  twoway_formula <- as.formula(
    paste0(response, " ~ ", predictor, " + precipitation_scaled + temperature_scaled + ",
           predictor, ":precipitation_scaled + ", predictor, ":temperature_scaled + precipitation_scaled:temperature_scaled + (1|", random_effect, ")")
  )
  # Fit models
  model_full <- lmerTest::lmer(full_formula, data = data)
  model_2way <- lmerTest::lmer(twoway_formula, data = data)
  # Likelihood ratio test
  lrt <- anova(model_2way, model_full)
  return(list(model_2way = model_2way, model_full = model_full, lrt = lrt))
}

#' Tidy a model object using broom::tidy
#' @param model A model object (e.g., from lmerTest::lmer or lm)
#' @return A tidy data frame of model results
tidy_model <- function(model) {
  broom::tidy(model)
}

#' Run model diagnostics using performance::check_model
#' @param model A model object (e.g., from lmerTest::lmer or lm)
#' @return The result of performance::check_model(model)
check_model_diagnostics <- function(model) {
  performance::check_model(model)
}

#' Plot model effects using sjPlot::plot_model, add grey zero line and theme_bw
#' @param model A model object (e.g., from lmerTest::lmer or lm)
#' @param ... Additional arguments passed to sjPlot::plot_model
#' @return A ggplot object
plot_model_effects <- function(model, ...) {
  # Use unbuilt=TRUE to get the ggplot object before printing
  p <- sjPlot::plot_model(model,  vline.color = "grey50", ...) +
    ggplot2::theme_bw()
  p
}

#' Clean and relabel model terms for presentation
#' @param tidy_df A tidy model output data frame (from broom::tidy)
#' @return A cleaned data frame with relabeled terms, random effects removed, and significance marked
clean_model_terms <- function(tidy_df) {
  # Remove random effects rows (terms starting with sd__ or containing Observation)
  df <- tidy_df |>
    dplyr::filter(!grepl("^sd__|Observation", term))
  # Relabel terms using str_replace for partial matches
  df <- df |>
    dplyr::mutate(
      term = term |>
        str_replace_all("fg_removed", "") |>
        str_replace_all("precipitation_scaled", "P") |>
        str_replace_all("temperature_scaled", "T"),
      signif = dplyr::if_else(!is.na(p.value) & p.value < 0.05, "*", "")
    ) |>
    dplyr::select(term, estimate, std.error, p.value, signif)
  return(df)
}

#' Create a pretty gt table from cleaned model output, with optional grouping
#' @param df A cleaned tidy model output data frame (from clean_model_terms)
#' @param group_var Optional string: column name to group the gt table by (e.g., 'focal_fg'). Default: NULL
#' @return A gt table with rounded numbers and bold significant p-values, optionally grouped
pretty_model_table <- function(df, group_var = NULL) {
  df <- df |>
    dplyr::select(-signif) |>
    dplyr::mutate(
      estimate = round(estimate, 2),
      std.error = round(std.error, 2),
      p.value = round(p.value, 3)
    )
  if (!is.null(group_var)) {
    df |>
      gt::gt(groupname_col = group_var) |>
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(
          columns = c(estimate, std.error, p.value),
          rows = p.value < 0.05
        )
      )
  } else {
    df |>
      gt::gt() |>
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(
          columns = c(estimate, std.error, p.value),
          rows = p.value < 0.05
        )
      )
  }
}

#' Plot tidy model effects with faceting and CI significance
#' @param data A tidy model output data frame (with columns: term, estimate, std.error, ...)
#' @param facet_var The column name (string) to facet by (e.g., 'focal_fg')
#' @param term_order Optional character vector specifying the order of model terms. If NULL, use the order in the data.
#' @return A ggplot object with effect sizes, faceted by the given variable, and point style by CI crossing zero
plot_tidy_effects_facet <- function(data, facet_var, term_order = NULL) {
  if (!is.null(term_order)) {
    data <- data %>%
      dplyr::mutate(term = factor(term, levels = term_order))
  }
  data <- data %>%
    dplyr::mutate(
      ci_lower = estimate - std.error,
      ci_upper = estimate + std.error,
      sig_point = ifelse(p.value < 0.05, "solid", "open")
    )
  ggplot(data %>% dplyr::filter(term != "(Intercept)"),
         aes(x = term, y = estimate, ymin = ci_lower, ymax = ci_upper)) +
    geom_pointrange(aes(shape = sig_point), fill = "white") +
    scale_shape_manual(values = c(solid = 19, open = 21), guide = "none") +
    facet_wrap(as.formula(paste("~", facet_var)), scales = "free_x") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    theme_bw() +
    labs(
      x = "Model term",
      y = "Estimate (± SE)",
      title = "Effect sizes by group"
    ) +
    coord_flip()
}
