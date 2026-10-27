library(glmmTMB)
library(emmeans)
library(DHARMa)
set.seed(123)


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
  F ~ G_treat * B_treat + temperature_scaled + precipitation_scaled +
    G_treat:temperature_scaled + G_treat:precipitation_scaled + B_treat:temperature_scaled + B_treat:precipitation_scaled +
    (1 | siteID/blockID),
  data = F_dat
)

tidy(mod_F_comp)
emm_F <- emmeans(
  mod_F_comp,
  ~ G_treat * B_treat,
  at = list(
    temperature_scaled = 1.048062,
    precipitation_scaled = 2.221637
  )
)

contrast(emm_F, method = "trt.vs.ctrl", ref = 1)



#Graminoids
G_dat <- harvest_dat_std  |>
  filter(G_treat == 0)

mod_G_comp <- lmer(
  G ~ F_treat * B_treat + temperature_scaled + precipitation_scaled +
    F_treat:temperature_scaled + F_treat:precipitation_scaled + B_treat:temperature_scaled + B_treat:precipitation_scaled +
    (1 | siteID/blockID),
  data = G_dat
)

tidy(mod_G_comp)

emm_G <- emmeans(
  mod_G_comp,
  ~ F_treat * B_treat,
  at = list(
    temperature_scaled = 1.048062,
    precipitation_scaled = 2.221637
  )
)

contrast(emm_G, method = "trt.vs.ctrl", ref = 1)


# Bryophytes
mod_B_comp <- glmmTMB(
  B ~ G_treat * F_treat +
    temperature_scaled * precipitation_scaled +
    (G_treat + F_treat):(temperature_scaled + precipitation_scaled) + G_treat:F_treat:temperature_scaled +
  G_treat:F_treat:precipitation_scaled +
    (1 | siteID/blockID),
  family = Gamma(link = "log"),
  data = harvest_dat_std |>
    filter(B_treat == 0, B > 0)
)

tidy(mod_B_comp)

# check model fit
sim_B <- simulateResiduals(
  fittedModel = mod_B_comp,
  n = 1000
)

plot(sim_B)

# consider altering parameters for better fit


# contrasts for mean temperature and precipitation
emm_B <- emmeans(
  mod_B_comp,
  ~ G_treat * F_treat,
  at = list(
    temperature_scaled = 1.05,
    precipitation_scaled = 2.21
  ),
  type = "response"
)

b_contrasts <- contrast(emm_B, method = "trt.vs.ctrl",
         ref = 1,
         adjust = "holm")

summary(b_contrasts, infer = c(TRUE, TRUE))


# contrasts allowing temperature to vary and fixing precipitation to the mean
emm_B_temp <- emmeans(
  mod_B_comp,
  ~ G_treat * F_treat | temperature_scaled,
  at = list(
    temperature_scaled = c(
      1.05 - 0.22,
      1.05,
      1.05 + 0.22
    ),
    precipitation_scaled = 2.21
  ),
  type = "response"
)

b_contrasts_temp <- contrast(
  emm_B_temp,
  method = "trt.vs.ctrl",
  ref = 1,
  adjust = "holm"
)

summary(b_contrasts_temp, infer = c(TRUE, TRUE)) |>
  tibble() |>
  ggplot(aes(
    x = temperature_scaled,
    y = ratio,
    colour = contrast,
    group = contrast
  )
  ) +
  geom_hline(yintercept = 1, linetype = 2, colour = "grey40") +
  geom_ribbon(
    aes(
      ymin = asymp.LCL,
      ymax = asymp.UCL,
      fill = contrast
    ),
    alpha = 0.15,
    colour = NA
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    x = "Temperature",
    y = "B biomass relative to control",
    colour = "Contrast",
    fill = "Contrast"
  ) +
  theme_classic()


# contrasts allowing precipitation to vary and fixing temperature to the mean
emm_B_temp <- emmeans(
  mod_B_comp,
  ~ G_treat * F_treat | precipitation_scaled,
  at = list(
    precipitation_scaled = c(
      0.87,
      2.22,
      4.65
    ),
    temperature_scaled = 1.05
  ),
  type = "response"
)

b_contrasts_precip <- contrast(
  emm_B_temp,
  method = "trt.vs.ctrl",
  ref = 1,
  adjust = "holm"
)

summary(b_contrasts_precip, infer = c(TRUE, TRUE)) |>
  tibble() |>
  ggplot(aes(
      x = precipitation_scaled,
      y = ratio,
      colour = contrast,
      group = contrast
    )
  ) +
  geom_hline(yintercept = 1, linetype = 2, colour = "grey40") +
  geom_ribbon(
    aes(
      ymin = asymp.LCL,
      ymax = asymp.UCL,
      fill = contrast
    ),
    alpha = 0.15,
    colour = NA
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    x = "Temperature",
    y = "B biomass relative to control",
    colour = "Contrast",
    fill = "Contrast"
  ) +
  theme_classic()




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
