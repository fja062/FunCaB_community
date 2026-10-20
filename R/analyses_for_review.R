library(glmmTMB)
library(emmeans)

test <- standing_biomass_22 |>
  pivot_wider(names_from = removed_fg, values_from = biomass, values_fill = 0) |>
  mutate(
   B_treat = as.integer(str_detect(fg_removed, stringr::fixed("B"))),
   G_treat = as.integer(str_detect(fg_removed, stringr::fixed("G"))),
   F_treat = as.integer(str_detect(fg_removed, stringr::fixed("F")))) |>
  group_by(plotID) |>
  mutate(total_biomass = sum(F,G,B)) |>
  ungroup() |>
  tidylog::filter(fg_removed != "FGB")



ctrl_means <- test |>
  filter(fg_removed == "none") |>
  group_by(siteID) |>
  summarise(
    G_control_mean = mean(G, na.rm = TRUE),
    F_control_mean = mean(F, na.rm = TRUE),
    B_control_mean = mean(B, na.rm = TRUE),
    .groups = "drop"
  )

harvest_dat_std <- test |>
  left_join(ctrl_means, by = "siteID") |>
  mutate(
    G_biomass_diff = G - G_control_mean,
    F_biomass_diff = F - F_control_mean,
    B_biomass_diff = B - B_control_mean
  )

F_dat <- harvest_dat_std  |>
  filter(F_treat == 0)

mod_F_comp <- lmer(
  F_biomass_diff ~ G_treat * B_treat + temperature_scaled + precipitation_scaled +
    G_treat:temperature_scaled + G_treat:precipitation_scaled +
    (1 | siteID),
  data = F_dat
)


m_total <- glmmTMB(
  total_biomass ~
    (G_treat + F_treat + B_treat)^2 +
    temperature_scaled * precipitation_scaled +
    (G_treat + F_treat + B_treat):(temperature_scaled + precipitation_scaled) +
    (1 | siteID/blockID),
  family = Gamma(link = "log"),
  data = test
)

tidy(m_total)

emm_total <- emmeans(
  m_total,
  ~ G_treat * F_treat * B_treat,
  type = "response",
  at = list(
    temperature_scaled = 0,
    precipitation_scaled = 0
  )
)

emm_total

contrast(
  emm_total,
  method = "trt.vs.ctrl",
  ref = 1,
  adjust = "holm"
)
