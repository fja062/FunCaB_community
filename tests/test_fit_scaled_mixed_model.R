# Test script for fit_scaled_mixed_model function
# This script creates synthetic data and tests the complete workflow

# Load required libraries
library(tidymodels)
library(lmerTest)
library(dplyr)

# Source the function (assuming we're running from project root)
source("R/tidymodels.r")

# Set seed for reproducible results
set.seed(123)

# Create synthetic test data
n_sites <- 5
n_years <- 3
n_plots_per_site <- 4
total_obs <- n_sites * n_years * n_plots_per_site

test_data <- data.frame(
  siteID = rep(paste0("Site", 1:n_sites), each = n_years * n_plots_per_site),
  year = rep(2015:2017, times = n_sites * n_plots_per_site),
  plotID = rep(paste0("Plot", 1:n_plots_per_site), times = n_sites * n_years),
  # Predictors with different scales
  predictor1 = rnorm(total_obs, mean = 100, sd = 20),  # Large scale
  predictor2 = rnorm(total_obs, mean = 0.5, sd = 0.1), # Small scale
  predictor3 = rnorm(total_obs, mean = 10, sd = 2),     # Medium scale
  # Response variable
  response = rnorm(total_obs, mean = 50, sd = 10)
)

# Add some structure to the response based on predictors
test_data$response <- test_data$response + 
  0.3 * test_data$predictor1 + 
  0.2 * test_data$predictor2 + 
  0.1 * test_data$predictor3 +
  0.05 * test_data$predictor1 * test_data$predictor2  # Interaction

# Test 1: Basic functionality
cat("=== Test 1: Basic functionality ===\n")

# Run the function
result <- fit_scaled_mixed_model(
  data = test_data,
  fixed_formula = response ~ predictor1 + predictor2 + predictor3,
  model_formula = response ~ predictor1 + predictor2 + predictor3 + predictor1:predictor2,
  random_effects = "(1 | siteID)"
)

# Check that all components are returned
cat("Checking return structure...\n")
stopifnot(all(c("model", "scaled_data", "original_data") %in% names(result)))
cat("✓ All components returned\n")

# Test 2: Check scaling
cat("\n=== Test 2: Check scaling ===\n")

# Check that predictors were scaled (mean ≈ 0, sd ≈ 1)
scaled_means <- colMeans(result$scaled_data[, c("predictor1", "predictor2", "predictor3")])
scaled_sds <- apply(result$scaled_data[, c("predictor1", "predictor2", "predictor3")], 2, sd)

cat("Scaled means (should be close to 0):\n")
print(round(scaled_means, 3))
cat("Scaled SDs (should be close to 1):\n")
print(round(scaled_sds, 3))

# Verify scaling worked
stopifnot(all(abs(scaled_means) < 0.1))
stopifnot(all(abs(scaled_sds - 1) < 0.1))
cat("✓ Scaling worked correctly\n")

# Test 3: Check backtransformation
cat("\n=== Test 3: Check backtransformation ===\n")

# Check that backtransformed predictors match original
original_predictors <- test_data[, c("predictor1", "predictor2", "predictor3")]
backtransformed_predictors <- result$original_data[, c("predictor1", "predictor2", "predictor3")]

differences <- abs(original_predictors - backtransformed_predictors)
max_diff <- max(differences)

cat("Maximum difference between original and backtransformed predictors:\n")
print(max_diff)

stopifnot(max_diff < 1e-10)  # Should be very close
cat("✓ Backtransformation worked correctly\n")

# Test 4: Check model fitting
cat("\n=== Test 4: Check model fitting ===\n")

# Check that model was fitted
cat("Model class:", class(result$model), "\n")
cat("Model formula:\n")
print(formula(result$model))

# Check that predictions were made
cat("Number of predictions:", length(result$scaled_data$pred), "\n")
cat("Range of predictions:", range(result$scaled_data$pred), "\n")

stopifnot(length(result$scaled_data$pred) == nrow(test_data))
cat("✓ Model fitting and predictions worked\n")

# Test 5: Check that grouping variable was not scaled
cat("\n=== Test 5: Check grouping variable handling ===\n")

# siteID should remain unchanged (not scaled) - check values, not necessarily class
original_siteIDs <- as.character(test_data$siteID)
scaled_siteIDs <- as.character(result$scaled_data$siteID)

cat("Original siteID class:", class(test_data$siteID), "\n")
cat("Scaled siteID class:", class(result$scaled_data$siteID), "\n")
cat("Values are identical:", identical(original_siteIDs, scaled_siteIDs), "\n")

# Check that the values are the same (allowing for class differences)
stopifnot(identical(original_siteIDs, scaled_siteIDs))
cat("✓ Grouping variable (siteID) was not scaled\n")

# More general test: check that any grouping variable would not be scaled
# This tests the function's ability to handle different grouping variables
cat("\n=== Test 5b: Test with different grouping variables ===\n")

# Create test data with multiple grouping variables
test_data2 <- test_data
test_data2$blockID <- paste0("Block", rep(1:2, each = nrow(test_data2)/2))
test_data2$treatment <- rep(c("Control", "Treatment"), times = nrow(test_data2)/2)

# Test with multiple grouping variables
result2 <- fit_scaled_mixed_model(
  data = test_data2,
  fixed_formula = response ~ predictor1 + predictor2 + predictor3,
  model_formula = response ~ predictor1 + predictor2 + predictor3 + predictor1:predictor2,
  random_effects = "(1 | blockID) + (1 | treatment)",
  grouping_var = c("blockID", "treatment")
)

# Check that all grouping variables were not scaled
grouping_vars <- c("blockID", "treatment")
for (var in grouping_vars) {
  original_vals <- as.character(test_data2[[var]])
  scaled_vals <- as.character(result2$scaled_data[[var]])
  
  cat("Checking", var, "- values identical:", identical(original_vals, scaled_vals), "\n")
  stopifnot(identical(original_vals, scaled_vals))
}

cat("✓ All grouping variables (blockID, treatment) were not scaled\n")

# Test with single grouping variable (different from default)
cat("\n=== Test 5c: Test with single non-default grouping variable ===\n")

result3 <- fit_scaled_mixed_model(
  data = test_data2,
  fixed_formula = response ~ predictor1 + predictor2 + predictor3,
  model_formula = response ~ predictor1 + predictor2 + predictor3 + predictor1:predictor2,
  random_effects = "(1 | treatment)",
  grouping_var = "treatment"
)

# Check that the single grouping variable was not scaled
original_treatment <- as.character(test_data2$treatment)
scaled_treatment <- as.character(result3$scaled_data$treatment)

stopifnot(identical(original_treatment, scaled_treatment))
cat("✓ Single grouping variable (treatment) was not scaled\n")

# Test 6: Model summary
cat("\n=== Test 6: Model summary ===\n")
cat("Model summary:\n")
print(summary(result$model))

# Test 7: Check interaction term
cat("\n=== Test 7: Check interaction term ===\n")

# Verify that the interaction term is in the model
model_terms <- attr(terms(result$model), "term.labels")
has_interaction <- any(grepl("predictor1:predictor2", model_terms))

cat("Interaction term in model:", has_interaction, "\n")
stopifnot(has_interaction)
cat("✓ Interaction term included in model\n")

cat("\n=== All tests passed! ===\n")
cat("The fit_scaled_mixed_model function is working correctly.\n") 
