# Example: Scaling predictors with recipes for mixed effects modeling
# Load required libraries
library(tidymodels)
library(lmerTest)
library(dplyr)

# Assume G_only is your data frame
# 1. Create and prep the recipe for scaling predictors (main effects only, include siteID but do not scale it)
rec <- recipe(
  standing_biomass_calculated ~ crb_B + crb_F + precipitation + year + siteID,
  data = G_only
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
summary(fit)

# 4. Make predictions (on scaled data)
scaled_data$pred <- predict(fit)

# 5. Manually backtransform all predictors using the means and sds from the recipe
# Extract means and sds from the prepped recipe
center_vals <- rec_prep$steps[[1]]$means
scale_vals  <- rec_prep$steps[[2]]$sds

# Backtransform all predictors in a tidyverse style
original_data <- scaled_data %>%
  mutate(across(
    all_of(names(center_vals)),
    ~ .x * scale_vals[cur_column()] + center_vals[cur_column()]
  ))

# Now, original_data contains the predictors on their original scale, along with predictions from the model.