### Functions for tidymodels

# Function: prepare_model_data
#
# This function prepares data for mixed effects modeling by filtering,
# and creating a wide table format with functional group removal biomass as separate columns.
#
# Parameters:
#   data: Data frame containing the analysis data (e.g., analysis_data)
#   fg_present: Character string specifying the functional group to filter for (e.g., "G", "F", "B")
#   fg_name: Character string specifying the functional group name for filtering (e.g., "graminoids", "forbs", "bryophytes")
#   exclude_year: Character or numeric specifying which year to exclude (default: "2015")
#
# Returns:
#   A wide-format data frame with:
#     - Response variable: standing_biomass_calculated
#     - Predictor variables: crb_B, crb_F, crb_G (cumulative removed biomass by functional group)
#     - Environmental variables: precipitation, temperature
#     - Grouping variables: year, siteID, blockID, plotID, etc.
#
# Example usage:
#   G_only <- prepare_model_data(
#     data = analysis_data,
#     fg_present = "G",
#     fg_name = "graminoids"
#   )

prepare_model_data <- function(data, fg_present, fg_name, exclude_year = "2015") {
  # Check for required column
  if (!"functional_group" %in% names(data)) {
    stop("The input data must have a column named 'functional_group'. Columns present: ", paste(names(data), collapse=", "))
  }
  # Determine which functional groups were removed based on what remains
  # When fg_remaining == "G", only "F" and "B" were removed (not "G")
  # When fg_remaining == "F", only "G" and "B" were removed (not "F")
  # When fg_remaining == "B", only "G" and "F" were removed (not "B")
  if (fg_present == "G") {
    expected_removed_fg <- c("F", "B")
  } else if (fg_present == "F") {
    expected_removed_fg <- c("G", "B")
  } else if (fg_present == "B") {
    expected_removed_fg <- c("G", "F")
  } else {
    stop("fg_present must be one of 'G', 'F', or 'B'")
  }
  
  expected_fg_cols <- paste0("crb_", expected_removed_fg)
  
  # Filter the data first to avoid non-standard evaluation issues
  filtered_data <- data[data$fg_remaining == fg_present & 
                       data$functional_group == fg_name & 
                       data$year != exclude_year, ]
  
  result <- filtered_data |>
    pivot_wider(
      names_from = removed_fg,
      values_from = cum_removed_biomass,
      names_prefix = "crb_"
    )
  
  # Ensure all expected crb columns exist, fill with 0 if missing
  # (0 means no biomass was removed for that functional group)
  missing_cols <- setdiff(expected_fg_cols, names(result))
  if (length(missing_cols) > 0) {
    for (col in missing_cols) {
      result[[col]] <- 0
    }
  }
  
  result
}


# Function: fit_scaled_mixed_model
# 
# This function provides a complete workflow for fitting mixed effects models with scaled predictors
# using the tidymodels/recipes framework. It handles preprocessing, model fitting, prediction,
# and backtransformation of predictors to their original scale.
#
# Parameters:
#   data: Data frame containing the response and predictor variables
#   fixed_formula: Formula for fixed effects only (e.g., y ~ x1 + x2) - used for scaling
#   model_formula: Full model formula including interactions (e.g., y ~ x1 + x2 + x1:x2) - used for fitting
#   random_effects: String specifying random effects (default: "(1 | siteID)")
#   grouping_var: Name of the grouping variable for random effects (default: "siteID")
#
# Returns:
#   A list containing:
#     - model: The fitted lmer model object
#     - scaled_data: Data frame with scaled predictors and predictions
#     - original_data: Data frame with backtransformed predictors and predictions
#
# Example usage:
#   results <- fit_scaled_mixed_model(
#     data = my_data,
#     fixed_formula = response ~ predictor1 + predictor2,
#     model_formula = response ~ predictor1 + predictor2 + predictor1:predictor2,
#     random_effects = "(1 | siteID)"
#   )

fit_scaled_mixed_model <- function(data, fixed_formula, model_formula, random_effects = "(1 | siteID)", grouping_var = "siteID") {
  # Handle multiple grouping variables
  if (length(grouping_var) > 1) {
    grouping_vars_str <- paste(grouping_var, collapse = " + ")
  } else {
    grouping_vars_str <- grouping_var
  }
  
  # 1. Create and prep the recipe for scaling predictors (include grouping vars but do not scale them)
  rec <- recipe(
    update(fixed_formula, paste(". ~ . +", grouping_vars_str)),
    data = data
  ) %>%
    step_center(all_predictors(), -all_of(grouping_var)) %>%
    step_scale(all_predictors(), -all_of(grouping_var))
  rec_prep <- prep(rec)
  # 2. Bake the scaled data
  scaled_data <- bake(rec_prep, new_data = NULL)
  # 3. Fit the mixed effects model using the scaled data
  full_model_formula <- as.formula(
    paste(paste(deparse(model_formula), collapse = " "), "+", random_effects)
  )
  fit <- lmerTest::lmer(
    formula = full_model_formula,
    control = lmerControl(optimizer = "bobyqa"),
    data = scaled_data
  )
  # 4. Make predictions
  scaled_data$pred <- predict(fit)
  # 5. Backtransform predictors
  center_vals <- rec_prep$steps[[1]]$means
  scale_vals  <- rec_prep$steps[[2]]$sds
  original_data <- scaled_data %>%
    mutate(across(
      all_of(names(center_vals)),
      ~ .x * scale_vals[cur_column()] + center_vals[cur_column()]
    ))
  list(
    model = fit,
    scaled_data = scaled_data,
    original_data = original_data
  )
}


# Function: fit_lmer_with_drop1
#
# Fits a full lmer model with all main effects and interactions, then runs drop1 for LRTs.
# Arguments:
#   data: Data frame
#   response: Name of response variable (string)
#   predictors: Character vector of predictor variable names (main effects)
#   random_effect: Name of random effect grouping variable (string)
#   add_fixed: Optional string for additional fixed effects (e.g., 'year')
# Returns:
#   A list with the fitted model and drop1 results
fit_lmer_with_drop1 <- function(data, response, predictors, random_effect, add_fixed = NULL) {
  library(lme4)
  # Build formula string
  interaction_part <- paste(predictors, collapse = " * ")
  fixed_part <- interaction_part
  if (!is.null(add_fixed)) {
    fixed_part <- paste(fixed_part, "+", add_fixed)
  }
  formula_str <- paste0(response, " ~ ", fixed_part, " + (1 | ", random_effect, ")")
  formula_obj <- as.formula(formula_str)
  # Fit model
  model <- lmer(formula_obj, data = data, REML = FALSE)
  # Run drop1 for LRTs
  lrt <- drop1(model, test = "Chisq")
  list(model = model, drop1 = lrt)
}

# Function: test_all_effects_lmer
#
# Tests all main effects and interactions for a user-specified model using a two-step approach.
# Arguments:
#   data: Data frame
#   response: Name of response variable (string)
#   predictors: Character vector of predictor variable names (main effects)
#   random_effect: Name of random effect grouping variable (string)
#   add_fixed: Optional string for additional fixed effects (e.g., 'year')
# Prints:
#   - LRT for all interactions (full vs. main effects only)
#   - LRT for each main effect (if interaction not significant)
#   - LRT for each interaction (if interaction significant)
# Returns:
#   - List with main_model and full_model

test_all_effects_lmer <- function(data, response, predictors, random_effect, add_fixed = NULL) {
  library(lme4)
  # Build formula strings
  interaction_part <- paste(predictors, collapse = " * ")
  fixed_part <- interaction_part
  if (!is.null(add_fixed)) {
    fixed_part <- paste(fixed_part, "+", add_fixed)
  }
  # Full model (with all interactions)
  full_formula <- as.formula(
    paste0(response, " ~ ", fixed_part, " + (1 | ", random_effect, ")")
  )
  # Main effects only (no interactions)
  main_formula <- as.formula(
    paste0(response, " ~ ", paste(predictors, collapse = " + "),
           if (!is.null(add_fixed)) paste0(" + ", add_fixed) else "",
           " + (1 | ", random_effect, ")")
  )
  # Fit models
  main_model <- lmer(main_formula, data = data, REML = FALSE)
  full_model <- lmer(full_formula, data = data, REML = FALSE)
  # Test interaction(s)
  cat("=== Test for all interactions ===\n")
  print(anova(main_model, full_model))
  # If interaction is not significant, test main effects
  cat("\n=== Test for main effects (if interaction not significant) ===\n")
  print(drop1(main_model, test = "Chisq"))
  # If interaction is significant, test which interaction(s) are important
  cat("\n=== Test for each interaction (if interaction significant) ===\n")
  print(drop1(full_model, test = "Chisq"))
  invisible(list(main_model = main_model, full_model = full_model))
}

# test_all_effects_lmer(
#   data = G_only,
#   response = "delta_biomass",
#   predictors = c("crb_B", "crb_F", "precipitation"),
#   random_effect = "siteID",
#   add_fixed = "year"
# )

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
#' @return A ggplot object with effect sizes, faceted by the given variable, and point style by CI crossing zero
plot_tidy_effects_facet <- function(data, facet_var) {
  # Reverse order so single effects are on top
  desired_order <- rev(c("fg_richness", "T", "P", "fg_richness:T", "fg_richness:P", "P:T"))
  data <- data %>%
    dplyr::mutate(
      ci_lower = estimate - std.error,
      ci_upper = estimate + std.error,
      sig_point = ifelse(p.value < 0.05, "solid", "open"),
      term = factor(term, levels = desired_order)
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
