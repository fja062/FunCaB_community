ddd <- sb_wide |>
tidylog::left_join(crb_wide, by = c("siteID", "blockID", "plotID", "fg_removed", "fg_remaining", "fg_richness", "temperature_level", "precipitation_level", "temperature_scaled", "precipitation_scaled"))

fit_g <- lmerTest::lmer(sb_G ~ crb_F * crb_B + (1 | siteID), data = ddd |> filter(fg_remaining == "G"))

fit_gf <- lmerTest::lmer(sb_G ~ sb_F * crb_B + (1 | siteID), data = ddd |> filter(fg_remaining == "GF"))

summary(fit_g)
summary(fit_gf)

ddd2 <- standing_biomass_22 |>
        mutate(fg_status = "remaining") |>
        select(-year, -fg_status, -comments, -temperature, -precipitation) |>
        rename(standing_biomass = biomass) |>
        pivot_wider(names_from = removed_fg, values_from = standing_biomass, names_prefix = "sb_", values_fill = 0) |>
        tidylog::left_join(
            cumulative_removed_biomass |>
        mutate(fg_status = "removed") |>
        select(-fg_status) |>
        pivot_wider(names_from = removed_fg, values_from = cumulative_removed_biomass, names_prefix = "crb_", values_fill = 0), by = c("siteID", "blockID", "plotID", "fg_removed", "fg_remaining", "fg_richness", "temperature_level", "precipitation_level", "temperature_scaled", "precipitation_scaled"))

new_ddd <- ddd2 |> filter(str_detect(fg_remaining, "G")) |>
mutate(diff_G = sb_G - crb_G,
diff_F = sb_F - crb_F,
diff_B = sb_B - crb_B)

fit_g <- lmerTest::lmer(sb_G ~ sb_F + sb_B + crb_F + crb_B + sb_F:crb_B + sb_B:crb_F + crb_F:crb_B + temperature_scaled + temperature_scaled:sb_F:crb_B + temperature_scaled:sb_B:crb_F + temperature_scaled:crb_F:crb_B +(1 | siteID), data = new_ddd)
summary(fit_g)
