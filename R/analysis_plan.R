# data analysis plan
analysis_plan <- list(

  # calculate diversity metrics
  tar_target(
    name = diversity,
    command = calc_diversity(cover_data)
  ),


  # make pca
  # tar_target(
  #   name = community_pca,
  #   command = make_sp_pca(cover)
  # )



  # 1. Effect of remaining/removed biomass on biomass of focal PFG
  # Single FG presence:
  #  g ~ b crb * f crb
  #  b ~ g crb * f crb
  #  f ~ b crb * g crb

  # forb cover in 2015 and crb forbs: check relationship
  # stepwise model removal

  tar_target(
    name = single_fg_model,
    command = {
      G_only <- prepare_model_data(
        data = analysis_data,
        fg_present = "G",
        fg_name = "graminoids"
      )

      # fit the model:
      # sb G ~ crb_B + crb_F + precip + crb_B:precip + crb_F:precip + year + (1 | siteID)
      result <- fit_scaled_mixed_model(
        data = G_only,
        fixed_formula = delta_biomass ~ crb_B + crb_F + precipitation + year,
        model_formula = delta_biomass ~ crb_B + crb_F + precipitation + crb_B:precipitation + crb_F:precipitation + year
      )
      # result$model
      # result$scaled_data
      # result$original_data

      # plot_model(result$model)
      # plot_model(result$model, type = "pred", terms = c("precipitation", "crb_F"))

      # F_only <- analysis_data |>
      #   filter(
      #     fg_remaining == "F",
      #     functional_group == "forbs",
      #     year != "2015"
      #   ) |>
      #   select(year, siteID, blockID, plotID, fg_removed, fg_remaining, removed_fg, standing_biomass_calculated, cum_removed_biomass, temperature_level, precipitation_level) |>
      #   pivot_wider(names_from = removed_fg, values_from = cum_removed_biomass, names_prefix = "crb_") %>%
      #   make_fancy_data(., gridded_climate, fix_treatment = FALSE) |>
      #   mutate(
      #     year = year - 2000,
      #     precipitation = precipitation / 1000
      #   )

      # scale(F_only$crb_G)

      # fit <- lmerTest::lmer(standing_biomass_calculated ~ crb_B * crb_G * precipitation + year + (1 | siteID), data = F_only)
      # summary(fit)


      # B_only <- analysis_data |>
      #   filter(
      #     fg_remaining == "B",
      #     functional_group == "bryophytes",
      #     year != "2015"
      #   ) |>
      #   select(year, siteID, blockID, plotID, fg_removed, fg_remaining, removed_fg, standing_biomass_calculated, cum_removed_biomass, temperature_level, precipitation_level) |>
      #   pivot_wider(names_from = removed_fg, values_from = cum_removed_biomass, names_prefix = "crb_") %>%
      #   make_fancy_data(., gridded_climate, fix_treatment = FALSE) |>
      #   mutate(
      #     year = year - 2000,
      #     precipitation = precipitation / 1000
      #   )

      # fit <- lmerTest::lmer(standing_biomass_calculated ~ crb_G + crb_F + precipitation + year + (1 | siteID), data = B_only)
      # summary(fit)
    }
  ),



  # Multiple FG presence:
  # gb ~ b sb * f crb
  # gb ~ g sb * f crb
  # gf ~ f sb * b crb
  # gf ~ g sb * b crb
  # fb ~ b sb * g crb
  # fb ~ f sb * g crb

  # fgb(c) ~ b sb * f sb
  # fgb(c) ~ g sb * f sb
  # fgb(c) ~ b sb * g sb

  tar_target(
    name = multiple_fg_model,
    command = {
      # two fg present
      GB <- analysis_data |>
        filter(
          fg_remaining == "GB",
          functional_group != "forbs",
          year != "2015"
        ) |>
        select(year, siteID, blockID, plotID, fg_removed, fg_remaining, removed_fg, functional_group, standing_biomass_calculated, cum_removed_biomass, temperature_level, precipitation_level) |>
        pivot_wider(names_from = functional_group, values_from = standing_biomass_calculated, names_prefix = "sb_")

      # gb ~ b sb * f crb
      # fit <- lmerTest::lmer(sb_graminoids ~ sb_bryophytes * cum_removed_biomass * precipitation + year + (1 | siteID), data = GB)
      # summary(fit)

      # # gb ~ g sb * f crb
      # fit <- lmerTest::lmer(sb_bryophytes ~ sb_graminoids * cum_removed_biomass + (1 | siteID), data = GB)



      # three fg present
      # FGB <- analysis_data |>
      #   filter(fg_remaining == "FGB") |>
      #   select(siteID, blockID, plotID, fg_removed, fg_remaining, removed_fg, functional_group, standing_biomass_19, cum_removed_biomass, temperature_level, precipitation_level) |>
      #   pivot_wider(names_from = functional_group, values_from = standing_biomass_19, names_prefix = "sb_") %>%
      #   make_fancy_data(., gridded_climate, fix_treatment = FALSE)

      # #fgb(c) ~ b sb * f sb
      # fit <- lmerTest::lmer(sb_graminoids ~ sb_bryophytes * sb_forbs + (1|siteID), data = FGB)
      # summary(fit)
    }
  )



  # 2. Effect of remaining/removed biomass on richness/evenness/diversity/traits of focal PFG
  # Single FG presence (2019):
  #  g ~ b crb * f crb
  # f ~ b crb * g crb
  # OR
  # ∂g ~ b crb * f crb
  # ∂f ~ b crb * g crb

  # Multiple FG presence:
  #  gb ~ b sb * f crb
  # gf ~ f sb * b crb
  # gf ~ g sb * b crb
  # fb ~ b sb * g crb

  # fgb(c) ~ b sb * f sb
  # fgb(c) ~ b sb * g sb
)
