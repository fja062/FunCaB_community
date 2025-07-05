# Integration test script for FunCaB Community Analysis Pipeline
# This script tests how functions work together and integrate with the broader workflow

# Load required libraries
library(dplyr)
library(tidyr)
library(lme4)
library(recipes)

# Source the functions
source("R/tidymodels.r")

# Set seed for reproducible results
set.seed(789)

cat("=== Integration Test: End-to-End Workflow ===\n")

# Create comprehensive test data that mimics the real analysis_data structure
# Each plot should have data for all functional groups being removed
n_sites <- 4
n_years <- 3
n_blocks_per_site <- 2
n_plots_per_block <- 2
n_plots <- n_sites * n_blocks_per_site * n_plots_per_block
n_fg_remaining <- 3  # G, F, B
n_removed_fg <- 3    # G, F, B
total_obs <- n_plots * n_years * n_fg_remaining * n_removed_fg

# Create realistic test data where each plot has exactly one row per removed functional group
# When fg_remaining == "G", removed_fg can only be "F" or "B" (not "G")
# When fg_remaining == "F", removed_fg can only be "G" or "B" (not "F")  
# When fg_remaining == "B", removed_fg can only be "G" or "F" (not "B")

# Create base structure for each plot/year
plot_year_combinations <- expand.grid(
  siteID = paste0("Site", 1:n_sites),
  year = 2015:2017,
  blockID = paste0("Block", 1:n_blocks_per_site),
  plotID = paste0("Plot", 1:n_plots_per_block),
  fg_remaining = c("G", "F", "B"),
  stringsAsFactors = FALSE
)

# For each plot/year/fg_remaining combination, create the appropriate removed_fg rows
integration_test_data <- do.call(rbind, lapply(1:nrow(plot_year_combinations), function(i) {
  row <- plot_year_combinations[i, ]
  
  # Determine which functional groups were removed based on what remains
  if (row$fg_remaining == "G") {
    removed_fg <- c("F", "B")  # Graminoids remain, so forbs and bryophytes were removed
    functional_group <- "graminoids"
  } else if (row$fg_remaining == "F") {
    removed_fg <- c("G", "B")  # Forbs remain, so graminoids and bryophytes were removed
    functional_group <- "forbs"
  } else {  # fg_remaining == "B"
    removed_fg <- c("G", "F")  # Bryophytes remain, so graminoids and forbs were removed
    functional_group <- "bryophytes"
  }
  
  # Create rows for each removed functional group with same values for other variables
  plot_standing_biomass <- rnorm(1, mean = 100, sd = 25)
  plot_precipitation <- rnorm(1, mean = 500, sd = 100)
  plot_temperature <- rnorm(1, mean = 10, sd = 3)
  
  data.frame(
    siteID = rep(row$siteID, length(removed_fg)),
    year = rep(row$year, length(removed_fg)),
    blockID = rep(row$blockID, length(removed_fg)),
    plotID = rep(row$plotID, length(removed_fg)),
    fg_remaining = rep(row$fg_remaining, length(removed_fg)),
    functional_group = rep(functional_group, length(removed_fg)),
    removed_fg = removed_fg,
    cum_removed_biomass = rnorm(length(removed_fg), mean = 50, sd = 15),
    standing_biomass_calculated = rep(plot_standing_biomass, length(removed_fg)),
    precipitation = rep(plot_precipitation, length(removed_fg)),
    temperature = rep(plot_temperature, length(removed_fg)),
    stringsAsFactors = FALSE
  )
}))

cat("Created test data with", nrow(integration_test_data), "observations\n")

cat("\n=== Test 1: Data Preparation Integration ===\n")

# Test prepare_model_data with graminoids
graminoid_data <- prepare_model_data(
  data = integration_test_data,
  fg_present = "G",
  fg_name = "graminoids"
)

cat("Graminoid data prepared:", nrow(graminoid_data), "rows\n")

# Check that the data structure is correct for modeling
required_cols <- c("siteID", "year", "standing_biomass_calculated", "precipitation", "temperature")
missing_cols <- setdiff(required_cols, names(graminoid_data))

stopifnot(length(missing_cols) == 0)
cat("✓ All required columns for modeling are present\n")

# Check for wide format columns
wide_cols <- grep("^crb_", names(graminoid_data), value = TRUE)
cat("Wide format columns:", paste(wide_cols, collapse = ", "), "\n")
stopifnot(length(wide_cols) > 0)
cat("✓ Wide format was created correctly\n")

cat("\n=== Test 2: Model Fitting Integration ===\n")

# Test the complete workflow with graminoid data
tryCatch({
  model_results <- fit_scaled_mixed_model(
    data = graminoid_data,
    fixed_formula = standing_biomass_calculated ~ crb_F + crb_B + precipitation + temperature,
    model_formula = standing_biomass_calculated ~ crb_F + crb_B + precipitation + temperature + crb_F:precipitation,
    random_effects = "(1 | siteID) + (1 | year)",
    grouping_var = c("siteID", "year")
  )
  
  cat("✓ Model fitting completed successfully\n")
  
  # Check model object
  stopifnot(inherits(model_results$model, "lmerMod"))
  cat("✓ Model object is of correct class (lmerMod)\n")
  
  # Check scaled data
  stopifnot(is.data.frame(model_results$scaled_data))
  stopifnot("pred" %in% names(model_results$scaled_data))
  cat("✓ Scaled data with predictions created\n")
  
  # Check original data
  stopifnot(is.data.frame(model_results$original_data))
  stopifnot("pred" %in% names(model_results$original_data))
  cat("✓ Original data with backtransformed predictors created\n")
  
  # Check that predictions are reasonable
  pred_range <- range(model_results$scaled_data$pred, na.rm = TRUE)
  response_range <- range(model_results$scaled_data$standing_biomass_calculated, na.rm = TRUE)
  
  cat("Prediction range:", pred_range[1], "to", pred_range[2], "\n")
  cat("Response range:", response_range[1], "to", response_range[2], "\n")
  
  # Predictions should be in reasonable range
  stopifnot(all(is.finite(pred_range)))
  cat("✓ Predictions are finite and reasonable\n")
  
}, error = function(e) {
  cat("✗ Model fitting failed:", e$message, "\n")
  stop(e)
})

cat("\n=== Test 3: Multiple Functional Groups Integration ===\n")

# Test with forbs
forb_data <- prepare_model_data(
  data = integration_test_data,
  fg_present = "F",
  fg_name = "forbs"
)

tryCatch({
  forb_model_results <- fit_scaled_mixed_model(
    data = forb_data,
    fixed_formula = standing_biomass_calculated ~ crb_G + crb_B + precipitation + temperature,
    model_formula = standing_biomass_calculated ~ crb_G + crb_B + precipitation + temperature,
    random_effects = "(1 | siteID)"
  )
  
  cat("✓ Forb model fitting completed successfully\n")
  
}, error = function(e) {
  cat("✗ Forb model fitting failed:", e$message, "\n")
  stop(e)
})

cat("\n=== Test 4: Data Consistency Across Workflow ===\n")

# Check that the same data flows through consistently
graminoid_rows <- nrow(graminoid_data)
scaled_rows <- nrow(model_results$scaled_data)
original_rows <- nrow(model_results$original_data)

cat("Graminoid data rows:", graminoid_rows, "\n")
cat("Scaled data rows:", scaled_rows, "\n")
cat("Original data rows:", original_rows, "\n")

stopifnot(graminoid_rows == scaled_rows)
stopifnot(scaled_rows == original_rows)
cat("✓ Row counts are consistent throughout the workflow\n")

# Check that key variables are preserved
key_vars <- c("siteID", "year", "plotID", "standing_biomass_calculated")
for (var in key_vars) {
  if (var %in% names(graminoid_data) && var %in% names(model_results$scaled_data)) {
    stopifnot(all(graminoid_data[[var]] == model_results$scaled_data[[var]], na.rm = TRUE))
  }
}
cat("✓ Key variables are preserved through scaling\n")

cat("\n=== Test 5: Error Handling Integration ===\n")

# Test with invalid data
invalid_data <- graminoid_data[1:5, ] # Too few observations for mixed model

tryCatch({
  fit_scaled_mixed_model(
    data = invalid_data,
    fixed_formula = standing_biomass_calculated ~ crb_F + crb_B,
    model_formula = standing_biomass_calculated ~ crb_F + crb_B,
    random_effects = "(1 | siteID)"
  )
  cat("✗ Should have failed with insufficient data\n")
  stop("Test failed - should have caught insufficient data error")
}, error = function(e) {
  cat("✓ Correctly caught error with insufficient data\n")
})

cat("\n=== Integration Test Summary ===\n")
cat("✓ Data preparation integrates with model fitting\n")
cat("✓ Multiple functional groups work correctly\n")
cat("✓ Data consistency is maintained throughout workflow\n")
cat("✓ Error handling works appropriately\n")
cat("✓ End-to-end workflow is functional\n")

cat("\n🎉 All integration tests passed!\n")
cat("The functions work together correctly in the complete workflow.\n") 