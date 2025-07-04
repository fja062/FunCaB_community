# library(tidymodels)


# G_only

# fit <- lmerTest::lmer(standing_biomass_calculated ~ crb_B + crb_F + precipitation + crb_B:precipitation + crb_F:precipitation + year + (1|siteID), data = G_only)
#         summary(fit)
#         # plot_model(fit)
#         # plot_model(fit, type = "pred", terms = c("precipitation", "crb_F"))

# rec <- recipe(standing_biomass_calculated ~ crb_B + crb_F + precipitation + crb_B*precipitation + crb_F*precipitation + year + (1|siteID), data = G_only) %>%
#   step_center(all_predictors()) %>%
#   step_scale(all_predictors())