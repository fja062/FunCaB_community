# Example: Scaling predictors with recipes for mixed effects modeling
# Load required libraries
library(tidymodels)
library(lmerTest)
library(dplyr)
library(sjPlot)

# Function to scale predictors, fit mixed effects model, predict, and backtransform
fit_scaled_mixed_model <- function(data) {
  # 1. Create and prep the recipe for scaling predictors (main effects only, include siteID but do not scale it)
  rec <- recipe(
    standing_biomass_calculated ~ crb_B + crb_F + precipitation + year + siteID,
    data = data
  ) %>%
    step_center(all_predictors(), -siteID) %>%
    step_scale(all_predictors(), -siteID)

  rec_prep <- prep(rec)

  # 2. Bake the scaled data (siteID will be present and unscaled)
  scaled_data <- bake(rec_prep, new_data = NULL)

  # 3. Fit the mixed effects model using the scaled data (add interactions and random effects here)
  fit <- lmer(
    standing_biomass_calculated ~ crb_B + crb_F + precipitation + crb_B:precipitation + crb_F:precipitation + year + (1 | siteID),
    data = scaled_data
  )

  # 4. Make predictions (on scaled data)
  scaled_data$pred <- predict(fit)

  # 5. Manually backtransform all predictors using the means and sds from the recipe
  center_vals <- rec_prep$steps[[1]]$means
  scale_vals  <- rec_prep$steps[[2]]$sds

  # Backtransform all predictors in a tidyverse style
  original_data <- scaled_data %>%
    mutate(across(
      all_of(names(center_vals)),
      ~ .x * scale_vals[cur_column()] + center_vals[cur_column()]
    ))

  # Return a list with the model, scaled data, and backtransformed data
  list(
    model = fit,
    scaled_data = scaled_data,
    original_data = original_data
  )
}

# Example usage:
# results <- fit_scaled_mixed_model(G_only)
# results$model         # the fitted model
# results$scaled_data   # the scaled data with predictions
# results$original_data # the backtransformed predictors with predictions