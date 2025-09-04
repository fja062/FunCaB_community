# data analysis plan
analysis_plan <- list(

  ## PART 1: Effect of removed fg on standing biomass

  # functional group richness
  tar_target(
    name = fg_richness_analysis,
    command = {
      # prepare data
      dat <- standing_biomass_22 |>
        # sum biomass by fg_removed and remaining
        group_by(siteID, blockID, plotID, fg_removed, fg_remaining, fg_richness, fg_status, temperature_level, precipitation_level, temperature_scaled, precipitation_scaled) |>
        summarise(standing_biomass = sum(biomass), .groups = "drop")

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

  # functional group identity
  tar_target(
    name = fg_identity_analysis,
    command = {
      # compare full vs 2-way model
      results <- compare_full_vs_2way_lmer(
        data = standing_biomass_22,
        response = "biomass",
        # should this be fg_removed x fg_richness. Or should it be identidy (fg_removed) x removed_fg?
        predictor = "fg_removed"
      )
    }
  ),
  tar_target(
    name = fg_identity_tidy,
    command = clean_model_terms(tidy_model(fg_identity_analysis$model_2way))
  ),

  # make pca
  tar_target(
    name = community_pca,
    command = make_sp_pca(cover_data)
  ),


  # PART 2: Single group effects
  # Graminoids present

    # Create a long dataset for each focal functional group present in fg_remaining
  tar_target(
    name = standing_biomass_by_focal_fg,
    command = {
      fgs <- c("G" = "graminoids", "F" = "forbs", "B" = "bryophytes")
      purrr::map_dfr(names(fgs), function(fg_code) {
        standing_biomass_22 %>%
          filter(str_detect(fg_remaining, fg_code)) %>%
          mutate(focal_fg = fgs[[fg_code]]) %>%
          group_by(siteID, blockID, plotID, fg_removed, fg_remaining, fg_richness, fg_status, temperature_level, precipitation_level, temperature_scaled, precipitation_scaled, focal_fg) %>%
          summarise(standing_biomass = sum(biomass), .groups = "drop")
      })
    }
  ),

  # functional group richness by focal group (graminoids, forbs, bryophytes)
  tar_target(
    name = fg_richness_focal_analysis,
    command = {
      split_data <- split(standing_biomass_by_focal_fg, standing_biomass_by_focal_fg$focal_fg)
      purrr::imap(split_data, function(dat, fg) {
        compare_full_vs_2way_lmer(
          data = dat,
          response = "standing_biomass",
          predictor = "fg_richness"
        )
      })
    }
  ),

  tar_target(
    name = fg_richness_focal_tidy,
    command = {
      purrr::imap_dfr(fg_richness_focal_analysis, function(res, fg) {
        clean_model_terms(tidy_model(res$model_2way)) %>%
          dplyr::mutate(focal_fg = fg)
      })
    }
  ),

  # functional group identity by focal group (graminoids, forbs, bryophytes)
  tar_target(
    name = fg_identity_focal_analysis,
    command = {
      split_data <- split(standing_biomass_by_focal_fg, standing_biomass_by_focal_fg$focal_fg)
      purrr::imap(split_data, function(dat, fg) {
        compare_full_vs_2way_lmer(
          data = dat,
          response = "standing_biomass",
          predictor = "fg_removed"
        )
      })
    }
  ),

  tar_target(
    name = fg_identity_focal_tidy,
    command = {
      purrr::imap_dfr(fg_identity_focal_analysis, function(res, fg) {
        clean_model_terms(tidy_model(res$model_2way)) %>%
          dplyr::mutate(focal_fg = fg)
      })
    }
  )

  # tar_target(
  #   name = fg_biomass_analysis,
  #   command = {
  #     # merge cumulative removed biomass with standing biomass
  #     dat <- sb_long |>
  #       tidylog::left_join(crb_long,
  #         by = c("siteID", "blockID", "plotID", "fg_removed", "fg_remaining", "fg_richness", "temperature_level", "precipitation_level", "temperature_scaled", "precipitation_scaled")) |>
  #         mutate(fg_removed = factor(fg_removed,
  #            levels = c("none", "G", "F", "B", "GF", "GB", "FB", "FGB"))) |>
  #         mutate(standing_biomass_log = log(standing_biomass + 1))
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

)
