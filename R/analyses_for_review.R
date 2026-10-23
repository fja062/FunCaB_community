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

#Forbs
F_dat <- harvest_dat_std  |>
  filter(F_treat == 0)

mod_F_comp <- lmer(
  F_biomass_diff ~ G_treat * B_treat + temperature_scaled + precipitation_scaled +
    G_treat:temperature_scaled + G_treat:precipitation_scaled + B_treat:temperature_scaled + B_treat:precipitation_scaled +
    (1 | siteID/blockID),
  data = F_dat
)

tidy(mod_F_comp)

#Graminoids
G_dat <- harvest_dat_std  |>
  filter(G_treat == 0)

mod_G_comp <- lmer(
  G_biomass_diff ~ F_treat * B_treat + temperature_scaled + precipitation_scaled +
    F_treat:temperature_scaled + F_treat:precipitation_scaled + B_treat:temperature_scaled + B_treat:precipitation_scaled +
    (1 | siteID/blockID),
  data = G_dat
)

tidy(mod_G_comp)


# Bryophytes
B_dat <- harvest_dat_std  |>
  filter(B_treat == 0)

mod_B_comp <- lmer(
  B ~ G_treat * F_treat + temperature_scaled + precipitation_scaled +
    G_treat:temperature_scaled + G_treat:precipitation_scaled + F_treat:temperature_scaled + F_treat:precipitation_scaled + temperature_scaled * precipitation_scaled +
    (1 | siteID/blockID),
  data = B_dat
)

tidy(mod_B_comp)

emm_B <- emmeans(
  mod_B_comp,
  ~ G_treat * F_treat,
  at = list(
    temperature_scaled = 1.048062,
    precipitation_scaled = 2.221637
  )
)

contrast(emm_B, method = "trt.vs.ctrl", ref = 1)

# full model
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
    temperature_scaled = 1.048062,
    precipitation_scaled = 2.221637
  )
)

emm_total

contrast(
  emm_total,
  method = "trt.vs.ctrl",
  ref = 1,
  adjust = "holm"
)


#  contrast                                                ratio     SE  df null z.ratio p.value
#  G_treat1 F_treat0 B_treat0 / G_treat0 F_treat0 B_treat0 0.810 0.1030 Inf    1  -1.654  0.1961
#  G_treat0 F_treat1 B_treat0 / G_treat0 F_treat0 B_treat0 0.848 0.1080 Inf    1  -1.303  0.1961
#  G_treat1 F_treat1 B_treat0 / G_treat0 F_treat0 B_treat0 0.433 0.0558 Inf    1  -6.497 <0.0001
#  G_treat0 F_treat0 B_treat1 / G_treat0 F_treat0 B_treat0 0.658 0.0838 Inf    1  -3.282  0.0031
#  G_treat1 F_treat0 B_treat1 / G_treat0 F_treat0 B_treat0 0.302 0.0385 Inf    1  -9.386 <0.0001
#  G_treat0 F_treat1 B_treat1 / G_treat0 F_treat0 B_treat0 0.426 0.0540 Inf    1  -6.737 <0.0001
#  G_treat1 F_treat1 B_treat1 / G_treat0 F_treat0 B_treat0 0.123 0.0270 Inf    1  -9.564 <0.0001



# figures

ggplot(test, aes(x = fg_removed, y = total_biomass)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.12, alpha = 0.6) +
  labs(
    x = "Functional groups removed",
    y = "Total biomass"
  ) +
  theme_classic() + facet_grid(~precipitation_level)

ggplot(test, aes(x = fg_removed, y = total_biomass)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.12, alpha = 0.6) +
  labs(
    x = "Functional groups removed",
    y = "Total biomass"
  ) +
  theme_classic() + facet_grid(~temperature_level)

ggplot(test, aes(x = fg_removed, y = total_biomass, colour = as_factor(precipitation_level))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.12, alpha = 0.6) +
  labs(
    x = "Functional groups removed",
    y = "Total biomass"
  ) +
  theme_classic() + facet_grid(~temperature_level)
