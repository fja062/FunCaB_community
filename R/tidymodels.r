# Function: fit_scaled_mixed_model
# 
# This function provides a complete workflow for fitting mixed effects models with scaled predictors
# using the tidymodels/recipes framework. It handles preprocessing, model fitting, prediction,
# and backtransformation of predictors to their original scale.
#
# Parameters:
#   data: Data frame containing the response and predictor variables
#   fixed_formula: Formula for fixed effects (e.g., y ~ x1 + x2)
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
#     random_effects = "(1 | siteID)"
#   )

fit_scaled_mixed_model <- function(data, fixed_formula, random_effects = "(1 | siteID)", grouping_var = "siteID") {
  # 1. Create and prep the recipe for scaling predictors (include grouping var but do not scale it)
  rec <- recipe(
    update(fixed_formula, paste(". ~ . +", grouping_var)),
    data = data
  ) %>%
    step_center(all_predictors(), -all_of(grouping_var)) %>%
    step_scale(all_predictors(), -all_of(grouping_var))
  rec_prep <- prep(rec)
  # 2. Bake the scaled data
  scaled_data <- bake(rec_prep, new_data = NULL)
  # 3. Fit the mixed effects model using the scaled data
  model_formula <- as.formula(
    paste(deparse(fixed_formula), "+", random_effects)
  )
  fit <- lmer(
    formula = model_formula,
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
  data |>
    filter(
      fg_remaining == fg_present,
      functional_group == fg_name,
      year != exclude_year
    ) |>
    pivot_wider(
      names_from = removed_fg,
      values_from = cum_removed_biomass,
      names_prefix = "crb_"
    )
}
