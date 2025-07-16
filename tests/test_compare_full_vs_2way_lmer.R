library(testthat)
library(lme4)

# Create a small synthetic dataset
df <- data.frame(
  response = rnorm(40),
  predictor = rep(letters[1:2], each = 20),
  precipitation_scaled = rnorm(40),
  temperature_scaled = rnorm(40),
  siteID = rep(1:4, each = 10)
)
df$predictor <- as.factor(df$predictor)

# Note: Convergence warnings are expected with random data and are not a failure of the function itself.
test_that("compare_full_vs_2way_lmer returns expected output structure", {
  result <- compare_full_vs_2way_lmer(
    data = df,
    response = "response",
    predictor = "predictor",
    random_effect = "siteID"
  )
  expect_type(result, "list")
  expect_true(all(c("model_2way", "model_full", "lrt") %in% names(result)))
  expect_true(inherits(result$model_2way, "merMod") || inherits(result$model_2way, "lmerModLmerTest"))
  expect_true(inherits(result$model_full, "merMod") || inherits(result$model_full, "lmerModLmerTest"))
  expect_s3_class(result$lrt, "anova")
}) 