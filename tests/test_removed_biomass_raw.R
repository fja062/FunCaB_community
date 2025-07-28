library(testthat)
library(dplyr)
library(ggplot2)

test_that("removed_biomass_raw only contains data from 2015 to 2021", {
  expect_true(all(removed_biomass_raw$year >= 2015 & removed_biomass_raw$year <= 2021))
})

test_that("removed_biomass_raw has 12 unique siteIDs", {
  expect_equal(length(unique(removed_biomass_raw$siteID)), 12)
})

test_that("Each site has 4 blocks", {
  block_counts <- removed_biomass_raw %>%
    group_by(siteID) %>%
    summarise(n_blocks = n_distinct(blockID))
  expect_true(all(block_counts$n_blocks == 4))
})

test_that("Each site has 8 treatments", {
  treatment_counts <- removed_biomass_raw %>%
    group_by(siteID) %>%
    summarise(n_treatments = n_distinct(treatment))
  expect_true(all(treatment_counts$n_treatments == 8))
})

test_that("Biomass does not have extreme outliers within each removed_fg group (using 1.5*IQR rule)", {
  outlier_summary <- removed_biomass_raw %>%
    group_by(removed_fg) %>%
    summarise(
      outliers = list({
        x <- biomass
        Q1 <- quantile(x, 0.25, na.rm = TRUE)
        Q3 <- quantile(x, 0.75, na.rm = TRUE)
        IQR <- Q3 - Q1
        lower <- Q1 - 1.5 * IQR
        upper <- Q3 + 1.5 * IQR
        x[x < lower | x > upper]
      }),
      n_outliers = sum({
        x <- biomass
        Q1 <- quantile(x, 0.25, na.rm = TRUE)
        Q3 <- quantile(x, 0.75, na.rm = TRUE)
        IQR <- Q3 - Q1
        lower <- Q1 - 1.5 * IQR
        upper <- Q3 + 1.5 * IQR
        x < lower | x > upper
      }, na.rm = TRUE)
    )
  expect_true(all(outlier_summary$n_outliers == 0))
})

# Plot biomass by removed_fg, highlighting outliers
removed_biomass_raw <- removed_biomass_raw %>%
  group_by(removed_fg) %>%
  mutate(
    Q1 = quantile(biomass, 0.25, na.rm = TRUE),
    Q3 = quantile(biomass, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lower = Q1 - 1.5 * IQR,
    upper = Q3 + 1.5 * IQR,
    is_outlier = biomass < lower | biomass > upper
  ) %>%
  ungroup()

biomass_plot <- ggplot(removed_biomass_raw, aes(x = removed_fg, y = biomass, color = is_outlier)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.7) +
  scale_color_manual(values = c("black", "red")) +
  labs(title = "Biomass by removed_fg with outliers highlighted", color = "Outlier") +
  theme_minimal()

ggsave(filename = "tests/biomass_by_removed_fg_outliers_raw.png", plot = biomass_plot, width = 8, height = 5)

test_that("All treatments are valid", {
  allowed_treatments <- c("GF", "FB", "GB", "C", "G", "F", "B", "FGB")
  expect_true(all(removed_biomass_raw$treatment %in% allowed_treatments))
})

test_that("blockID is first 3 letters of siteID plus a digit 1-4", {
  expect_true(all(mapply(function(site, block) grepl(paste0("^", substr(site, 1, 3), "[1-4]$"), block), removed_biomass_raw$siteID, removed_biomass_raw$blockID)))
})

test_that("plotID is a combination of blockID and treatment", {
  block_treat <- paste0(removed_biomass_raw$blockID, removed_biomass_raw$treatment)
  block_treat_sep <- paste0(removed_biomass_raw$blockID, "_", removed_biomass_raw$treatment)
  expect_true(all(removed_biomass_raw$plotID == block_treat | removed_biomass_raw$plotID == block_treat_sep))
}) 