# data analysis plan
analysis_plan <- list(

  ## PART 1: Effect of removed biomass on remaining biomass

  tar_target(
    name = fg_richness_analysis,
    command = {

      # prepare data
      dat <- biomass_22 |>
        # sum biomass by fg_removed and remaining
        group_by(siteID, blockID, plotID, fg_removed, fg_remaining, fg_richness, fg_status, temperature_level, precipitation_level, temperature_scaled, precipitation_scaled) |>
        summarise(standing_biomass = sum(biomass), .groups = "drop") |>
        # bare ground plots need to stay, but biomass is 0
        mutate(fg_status = if_else(fg_removed == "FGB", "remaining", fg_status),
               standing_biomass = if_else(fg_removed == "FGB", 0, standing_biomass)) |> 
        filter(fg_status == "remaining")

      # compare full vs 2-way model
      results <- compare_full_vs_2way_lmer(
        data = dat,
      response = "standing_biomass",
      predictor = "fg_richness"
      )

    }
  ),

    tar_target(
    name = fg_richness_tidy,
    command = clean_model_terms(tidy_model(fg_richness_analysis$model_2way))
  ),

  tar_target(
    name = fg_identity_analysis,
    command = {

      # prepare data
      dat <- biomass_22 |> 
        # bare ground plots need to stay, but biomass is 0
        mutate(fg_status = if_else(fg_removed == "FGB", "remaining", fg_status),
               standing_biomass = if_else(fg_removed == "FGB", 0, biomass)) |> 
        filter(fg_status == "remaining") |>
        mutate(fg_removed = factor(fg_removed, levels = c("none", "G", "F", "B", "GF", "GB", "FB", "FGB")))

      #dat |> distinct(fg_removed, removed_fg, fg_remaining, fg_richness, fg_status)

      # compare full vs 2-way model
      results <- compare_full_vs_2way_lmer(
        data = dat,
      response = "standing_biomass",
      predictor = "fg_removed"
      )

    }
  ),

  tar_target(
    name = fg_identity_tidy,
    command = clean_model_terms(tidy_model(fg_identity_analysis$model_2way))
  )


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

  # stepwise model removal

  # tar_target(
  #   name = single_fg_model,
  #   command = {
  #     G_only <- prepare_model_data(
  #       data = analysis_data,
  #       fg_present = "G",
  #       fg_name = "graminoids"
  #     )

  #     # fit the model:
  #     # sb G ~ crb_B + crb_F + precip + crb_B:precip + crb_F:precip + year + (1 | siteID)
  #     result <- fit_scaled_mixed_model(
  #       data = G_only,
  #       fixed_formula = delta_biomass ~ crb_B + crb_F + precipitation + year,
  #       model_formula = delta_biomass ~ crb_B + crb_F + precipitation + crb_B:precipitation + crb_F:precipitation + year,
  #       grouping_var = c("siteID", "crb_B", "crb_F")
  #     )
  #     # summary(result$model)
  #     # result$scaled_data
  #     # result$original_data

  #     # result <- fit_lmer_with_drop1(
  #     #   data = G_only,
  #     #   response = "delta_biomass",
  #     #   predictors = c("crb_B", "crb_F", "precipitation"),
  #     #   random_effect = "siteID",
  #     #   add_fixed = "year"
  #     #   )

  #     # # plot_model(result$model)
  #     # # plot_model(result$model, type = "pred", terms = c("precipitation", "crb_"))

  #     # F_only <- prepare_model_data(
  #     #   data = analysis_data,
  #     #   fg_present = "F",
  #     #   fg_name = "forbs"
  #     # )

  #     # result <- fit_scaled_mixed_model(
  #     #   data = F_only,
  #     #   fixed_formula = delta_biomass ~ crb_B + crb_G + precipitation + year,
  #     #   model_formula = delta_biomass ~ crb_B + crb_G + precipitation + crb_B:precipitation + crb_G:precipitation + year,
  #     #   grouping_var = c("siteID", "crb_B", "crb_G")
  #     # )

  #     # B_only <- prepare_model_data(
  #     #   data = analysis_data,
  #     #   fg_present = "B",
  #     #   fg_name = "bryophytes"
  #     # )

  #     # result <- fit_scaled_mixed_model(
  #     #   data = B_only,
  #     #   fixed_formula = delta_biomass ~ crb_G + crb_F + precipitation + year,
  #     #   model_formula = delta_biomass ~ crb_G * crb_F * precipitation + crb_G:precipitation + crb_F:precipitation + year,
  #     #   grouping_var = c("siteID", "crb_G", "crb_F")
  #     # )

  #   }
  # ),



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

  # tar_target(
  #   name = multiple_fg_model,
  #   command = {
  #     # two fg present
  #     GB <- analysis_data |>
  #       filter(
  #         fg_remaining == "GB",
  #         functional_group != "forbs",
  #         year != "2015"
  #       ) |>
  #       select(year, siteID, blockID, plotID, fg_removed, fg_remaining, removed_fg, functional_group, standing_biomass_calculated, cum_removed_biomass, temperature_level, precipitation_level) |>
  #       pivot_wider(names_from = functional_group, values_from = standing_biomass_calculated, names_prefix = "sb_")

  #     # gb ~ b sb * f crb
  #     # fit <- lmerTest::lmer(sb_graminoids ~ sb_bryophytes * cum_removed_biomass * precipitation + year + (1 | siteID), data = GB)
  #     # summary(fit)

  #     # # gb ~ g sb * f crb
  #     # fit <- lmerTest::lmer(sb_bryophytes ~ sb_graminoids * cum_removed_biomass + (1 | siteID), data = GB)



  #     # three fg present
  #     # FGB <- analysis_data |>
  #     #   filter(fg_remaining == "FGB") |>
  #     #   select(siteID, blockID, plotID, fg_removed, fg_remaining, removed_fg, functional_group, standing_biomass_19, cum_removed_biomass, temperature_level, precipitation_level) |>
  #     #   pivot_wider(names_from = functional_group, values_from = standing_biomass_19, names_prefix = "sb_") %>%
  #     #   make_fancy_data(., gridded_climate, fix_treatment = FALSE)

  #     # #fgb(c) ~ b sb * f sb
  #     # fit <- lmerTest::lmer(sb_graminoids ~ sb_bryophytes * sb_forbs + (1|siteID), data = FGB)
  #     # summary(fit)
  #   }
  # )



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
