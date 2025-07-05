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
