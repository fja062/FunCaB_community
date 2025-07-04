# Example: Scaling predictors with recipes for mixed effects modeling
# Load required libraries
library(tidymodels)
library(lmerTest)
library(dplyr)
library(sjPlot)

# Function to scale predictors, fit mixed effects model, predict, and backtransform
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

# Example usage:
# results <- fit_scaled_mixed_model(
#   G_only,
#   standing_biomass_calculated ~ crb_B + crb_F + precipitation + year
# )
# results$model         # the fitted model
# results$scaled_data   # the scaled data with predictions
# results$original_data # the backtransformed predictors with predictions