#!/usr/bin/env Rscript
# Test Runner for FunCaB Community Analysis
# 
# This script runs all tests or specific test categories
# Usage: 
#   Rscript run_tests.R                    # Run all tests
#   Rscript run_tests.R --unit             # Run only unit tests
#   Rscript run_tests.R --integration      # Run only integration tests
#   Rscript run_tests.R --function name    # Run tests for specific function

# Load required libraries
library(dplyr)

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

# Test configuration
test_config <- list(
  unit_tests = c(
    "tests/test_compare_full_vs_2way_lmer.R",
    "tests/test_removed_biomass_raw.R",
    "tests/test_removed_biomass_final_raw.R"
  ),
  integration_tests = c(
    # No integration tests currently available
  ),
  all_tests = c(
    "tests/test_compare_full_vs_2way_lmer.R",
    "tests/test_removed_biomass_raw.R",
    "tests/test_removed_biomass_final_raw.R"
  )
)

# Function to run a single test file
run_test_file <- function(test_file) {
  cat("\n" , rep("=", 60), "\n", sep = "")
  cat("Running:", test_file, "\n")
  cat(rep("=", 60), "\n", sep = "")
  
  tryCatch({
    source(test_file)
    cat("✓", test_file, "PASSED\n")
    return(TRUE)
  }, error = function(e) {
    cat("✗", test_file, "FAILED\n")
    cat("Error:", e$message, "\n")
    return(FALSE)
  })
}

# Function to run all tests
run_all_tests <- function() {
  cat("Running all tests...\n")
  results <- sapply(test_config$all_tests, run_test_file)
  
  cat("\n" , rep("=", 60), "\n", sep = "")
  cat("TEST SUMMARY\n")
  cat(rep("=", 60), "\n", sep = "")
  
  passed <- sum(results)
  total <- length(results)
  
  cat("Passed:", passed, "/", total, "\n")
  
  if (passed == total) {
    cat("🎉 All tests passed!\n")
    quit(status = 0)
  } else {
    cat("❌ Some tests failed!\n")
    quit(status = 1)
  }
}

# Function to run unit tests only
run_unit_tests <- function() {
  cat("Running unit tests...\n")
  results <- sapply(test_config$unit_tests, run_test_file)
  
  cat("\nUnit test summary:", sum(results), "/", length(results), "passed\n")
}

# Function to run integration tests only
run_integration_tests <- function() {
  cat("Running integration tests...\n")
  results <- sapply(test_config$integration_tests, run_test_file)
  
  cat("\nIntegration test summary:", sum(results), "/", length(results), "passed\n")
}

# Function to run tests for a specific function
run_function_tests <- function(function_name) {
  cat("Running tests for function:", function_name, "\n")
  
  # Map function names to test files
  function_tests <- list(
    "compare_full_vs_2way_lmer" = "tests/test_compare_full_vs_2way_lmer.R",
    "removed_biomass_raw" = "tests/test_removed_biomass_raw.R",
    "removed_biomass_final_raw" = "tests/test_removed_biomass_final_raw.R"
  )
  
  if (function_name %in% names(function_tests)) {
    run_test_file(function_tests[[function_name]])
  } else {
    cat("No tests found for function:", function_name, "\n")
    cat("Available functions:", paste(names(function_tests), collapse = ", "), "\n")
  }
}

# Main execution
if (length(args) == 0) {
  # No arguments - run all tests
  run_all_tests()
} else if (args[1] == "--unit") {
  run_unit_tests()
} else if (args[1] == "--integration") {
  run_integration_tests()
} else if (args[1] == "--function" && length(args) > 1) {
  run_function_tests(args[2])
} else {
  cat("Usage:\n")
  cat("  Rscript run_tests.R                    # Run all tests\n")
  cat("  Rscript run_tests.R --unit             # Run unit tests only\n")
  cat("  Rscript run_tests.R --integration      # Run integration tests only\n")
  cat("  Rscript run_tests.R --function name    # Run tests for specific function\n")
} 