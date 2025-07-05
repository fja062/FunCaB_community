# Test script for prepare_model_data function
# This script tests the data preparation function with synthetic data

# Load required libraries
library(dplyr)
library(tidyr)

# Source the function
source("R/tidymodels.r")

# Set seed for reproducible results
set.seed(456)

# Create synthetic test data similar to analysis_data structure
n_sites <- 3
n_years <- 2
n_plots_per_site <- 2
total_obs <- n_sites * n_years * n_plots_per_site

test_data <- data.frame(
  siteID = rep(paste0("Site", 1:n_sites), each = n_years * n_plots_per_site),
  year = rep(2015:2016, times = n_sites * n_plots_per_site),
  plotID = rep(paste0("Plot", 1:n_plots_per_site), times = n_sites * n_years),
  fg_remaining = rep(c("G", "F", "B"), each = total_obs/3),
  functional_group = rep(c("graminoids", "forbs", "bryophytes"), each = total_obs/3),
  removed_fg = rep(c("G", "F", "B"), times = total_obs/3),
  cum_removed_biomass = rnorm(total_obs, mean = 50, sd = 10),
  standing_biomass_calculated = rnorm(total_obs, mean = 100, sd = 20),
  precipitation = rnorm(total_obs, mean = 500, sd = 100),
  temperature = rnorm(total_obs, mean = 10, sd = 2)
)

cat("=== Test 1: Basic functionality ===\n")

# Test the function
result <- prepare_model_data(
  data = test_data,
  fg_present = "G",
  fg_name = "graminoids"
)

# Check that the result is a data frame
stopifnot(is.data.frame(result))
cat("✓ Result is a data frame\n")

# Check that filtering worked
cat("Original data rows:", nrow(test_data), "\n")
cat("Filtered data rows:", nrow(result), "\n")

# Should have fewer rows after filtering
stopifnot(nrow(result) < nrow(test_data))
cat("✓ Filtering reduced the number of rows\n")

cat("\n=== Test 2: Check filtering logic ===\n")

# Check that only the specified functional group is present
unique_fg_remaining <- unique(result$fg_remaining)
unique_functional_group <- unique(result$functional_group)

cat("Unique fg_remaining:", paste(unique_fg_remaining, collapse = ", "), "\n")
cat("Unique functional_group:", paste(unique_functional_group, collapse = ", "), "\n")

stopifnot(all(unique_fg_remaining == "G"))
stopifnot(all(unique_functional_group == "graminoids"))
cat("✓ Only specified functional group (G/graminoids) is present\n")

# Check that 2015 is excluded
unique_years <- unique(result$year)
cat("Years in result:", paste(unique_years, collapse = ", "), "\n")
stopifnot(!("2015" %in% unique_years))
cat("✓ Year 2015 is excluded\n")

cat("\n=== Test 3: Check pivot_wider functionality ===\n")

# Check that the wide format was created
expected_cols <- c("crb_G", "crb_F", "crb_B")
existing_cols <- expected_cols[expected_cols %in% names(result)]

cat("Expected wide columns:", paste(expected_cols, collapse = ", "), "\n")
cat("Existing wide columns:", paste(existing_cols, collapse = ", "), "\n")

stopifnot(length(existing_cols) > 0)
cat("✓ Pivot_wider created wide format columns\n")

cat("\n=== Test 4: Test with different functional group ===\n")

# Test with forbs
result_forbs <- prepare_model_data(
  data = test_data,
  fg_present = "F",
  fg_name = "forbs"
)

unique_fg_remaining_forbs <- unique(result_forbs$fg_remaining)
unique_functional_group_forbs <- unique(result_forbs$functional_group)

stopifnot(all(unique_fg_remaining_forbs == "F"))
stopifnot(all(unique_functional_group_forbs == "forbs"))
cat("✓ Function works with different functional group (F/forbs)\n")

cat("\n=== Test 5: Test with different exclude year ===\n")

# Test excluding 2016 instead of 2015
result_2016 <- prepare_model_data(
  data = test_data,
  fg_present = "G",
  fg_name = "graminoids",
  exclude_year = "2016"
)

unique_years_2016 <- unique(result_2016$year)
cat("Years when excluding 2016:", paste(unique_years_2016, collapse = ", "), "\n")
stopifnot(!("2016" %in% unique_years_2016))
cat("✓ Function correctly excludes specified year\n")

cat("\n=== All tests passed! ===\n")
cat("The prepare_model_data function is working correctly.\n") 