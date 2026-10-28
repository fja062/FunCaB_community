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
        tidylog::summarise(standing_biomass = sum(biomass), .groups = "drop")

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
        # should this be fg_removed x fg_richness. Or should it be identity (fg_removed) x removed_fg?
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


  tar_target(
    name = fg_biomass_analysis,
    command = {
      joined_dat <- sb_long |>
      tidylog::left_join(removed_biomass_long, by = c("siteID", "blockID", "plotID", "fg_removed", "fg_remaining", "fg_richness", "temperature_level", "precipitation_level", "temperature_scaled", "precipitation_scaled")) |>
        mutate(combined_fg_trt = paste(fg_removed, removed_fg, sep = "_"))

      fit_mod <- lmerTest::lmer(standing_biomass ~ 0 + prop_removed_biomass_trt + removed_fg + (1|siteID), data = joined_dat)
      summary(fit_mod)

      fit_mod <- lmerTest::lmer(standing_biomass ~ 0 + prop_removed_biomass_trt*removed_fg*fg_removed + (1|siteID), data = joined_dat)
      summary(fit_mod)
            # maybe we want this to be a zero-intercept model. At the moment this compares to bryophytes, which is #not so interesting.

}
),

tar_target(
    name = single_fg_biomass_analysis,
    command = {
      joined_dat <- sb_long |>
      tidylog::left_join(removed_biomass_long, by = c("siteID", "blockID", "plotID", "fg_removed", "fg_remaining", "fg_richness", "temperature_level", "precipitation_level", "temperature_scaled", "precipitation_scaled")) |>
        mutate(combined_fg_trt = paste(fg_removed, removed_fg, sep = "_"))

      # Function to fit models with error handling
      fit_single_fg_model <- function(data, formula) {
        # Check if we have enough levels for factors
        if (n_distinct(data$fg_removed) < 2) {
          return(NULL)  # Skip if not enough levels
        }
        tryCatch({
          lmerTest::lmer(as.formula(formula), data = data)
        }, error = function(e) {
          message(paste("Model failed:", e$message))
          return(NULL)
        })
      }

      # Create model combinations
      model_combinations <- expand_grid(
        fg_remaining = c("F", "G", "B"),
        removed_fg = c("F", "G", "B"),
        env_var = c("temperature_scaled", "precipitation_scaled")
      ) %>%
        # Filter to valid combinations (remaining != removed)
        filter(fg_remaining != removed_fg) %>%
        # Create model formulas
        mutate(
          model_formula = case_when(
            env_var == "temperature_scaled" ~ "standing_biomass ~ prop_removed_biomass_trt * fg_removed * temperature_scaled + (1|siteID)",
            env_var == "precipitation_scaled" ~ "standing_biomass ~ prop_removed_biomass_trt * fg_removed * precipitation_scaled + (1|siteID)"
          )
        )

      # Fit models for each combination
      single_fg_models <- model_combinations %>%
        rowwise() %>%
        mutate(
          # Filter data for this combination
          model_data = list(
            joined_dat %>%
              filter(fg_remaining == .data$fg_remaining, removed_fg == .data$removed_fg)
          )
        ) %>%
        ungroup() %>%
        mutate(
          n_obs = map_int(model_data, nrow),
          # Fit model if we have enough data
          model = map2(model_data, model_formula, ~ {
            if (.y >= 10) {  # Minimum sample size check
              fit_single_fg_model(.x, .y)
            } else {
              NULL
            }
          })
        ) %>%
        # Add model summaries for successful models
        mutate(
          model_summary = map(model, ~ if (!is.null(.x)) broom::tidy(.x) else NULL),
          model_anova = map(model, ~ if (!is.null(.x)) anova(.x) else NULL)
        ) %>%
        # Remove failed models
        filter(!map_lgl(model, is.null))

      # Return the results
      single_fg_models

}
),


tar_target(
  name = fg_biomass_total_compensation_analysis,
  command = {
    m_total <- glmmTMB(
      total_biomass ~
        (G_treat + F_treat + B_treat)^2 +
        temperature_scaled * precipitation_scaled +
        (G_treat + F_treat + B_treat):(temperature_scaled + precipitation_scaled) +
        (1 | siteID/blockID),
      family = Gamma(link = "log"),
      data = standing_biomass_22_wide
    )

    tidy(m_total)

    emm_total <- emmeans(
      m_total,
      ~ G_treat * F_treat * B_treat,
      type = "response",
      at = list(
        temperature_scaled = 1.05,
        precipitation_scaled = 2.21
      )
    )

    emm_total

    contrasts_total <- contrast(
      emm_total,
      method = "trt.vs.ctrl",
      ref = 1,
      adjust = "holm"
    )


    summary(contrasts_total, infer = c(TRUE, TRUE)) |>
      tibble() |>
      gt()


  }

),


# PART 2: Single group effects

  # ------------------------------------------------------------
  # Analysis data: B retained and present
  # ------------------------------------------------------------
  tar_target(
    name = B_comp_dat,
    command = standing_biomass_22_wide |>
      filter(B_treat == 0, B > 0)
  ),

  # ------------------------------------------------------------
  # Full bryophyte compensation model
  # ------------------------------------------------------------
  tar_target(
    name = mod_B_comp,
    command = glmmTMB(
      B ~
        G_treat * F_treat * temperature_scaled +
        G_treat * F_treat * precipitation_scaled +
        temperature_scaled:precipitation_scaled +
        (1 | siteID/blockID),
      family = Gamma(link = "log"),
      data = B_comp_dat
    )
  ),

  # ------------------------------------------------------------
  # Full-model coefficient table
  # ------------------------------------------------------------
  tar_target(
    name = B_comp_model_results,
    command = broom.mixed::tidy(
      mod_B_comp,
      effects = "fixed",
      conf.int = TRUE
    ) |>
      mutate(
        estimate = round(estimate, 3),
        std.error = round(std.error, 3),
        conf.low = round(conf.low, 3),
        conf.high = round(conf.high, 3),
        p.value = case_when(
          p.value < 0.001 ~ "<0.001",
          TRUE ~ format(round(p.value, 3), nsmall = 3)
        )
      )
  ),

  tar_target(
    name = B_comp_model_results_table,
    command = B_comp_model_results |>
      gt() |>
      cols_label(
        term = "Model term",
        estimate = "Estimate",
        std.error = "SE",
        conf.low = "Lower 95% CI",
        conf.high = "Upper 95% CI",
        p.value = md("*p*-value")
      )
  ),

  # ------------------------------------------------------------
  # DHARMa diagnostics
  # ------------------------------------------------------------
  tar_target(
    name = B_comp_dharma,
    command = {
      set.seed(123)
      simulateResiduals(
        fittedModel = mod_B_comp,
        n = 1000
      )
    }
  ),

  # This target returns the diagnostic plot
  tar_target(
    name = B_comp_dharma_plot,
    command = plot(B_comp_dharma)
  ),

  # ------------------------------------------------------------
  # Contrasts at mean temperature and mean precipitation
  # ------------------------------------------------------------
  tar_target(
    name = B_comp_emm_mean_climate,
    command = emmeans(
      mod_B_comp,
      ~ G_treat * F_treat,
      at = list(
        temperature_scaled = 1.05,
        precipitation_scaled = 2.21
      )
    )
  ),

  tar_target(
    name = B_comp_contrasts_mean_climate,
    command = contrast(
      B_comp_emm_mean_climate,
      method = "trt.vs.ctrl",
      ref = 1,
      adjust = "holm"
    ) |>
      summary(
        type = "response",
        infer = c(TRUE, TRUE)
      ) |>
      as.data.frame()
  ),

  tar_target(
    name = B_comp_contrasts_mean_climate_table,
    command = B_comp_contrasts_mean_climate |>
      mutate(
        ratio = round(ratio, 2),
        asymp.LCL = round(asymp.LCL, 2),
        asymp.UCL = round(asymp.UCL, 2),
        `95% CI` = paste0(asymp.LCL, "–", asymp.UCL),
        p.value = case_when(
          p.value < 0.001 ~ "<0.001",
          TRUE ~ format(round(p.value, 3), nsmall = 3)
        )
      ) |>
      select(contrast, ratio, `95% CI`, p.value) |>
      gt() |>
      cols_label(
        contrast = "Contrast",
        ratio = "Biomass ratio",
        p.value = md("Adjusted *p*-value")
      )
  ),

  # ------------------------------------------------------------
  # Temperature-dependent contrasts
  # Precipitation held at its mean
  # ------------------------------------------------------------
  tar_target(
    name = B_comp_emm_temperature,
    command = emmeans(
      mod_B_comp,
      ~ G_treat * F_treat | temperature_scaled,
      at = list(
        temperature_scaled = c(
          1.05 - 0.22,
          1.05,
          1.05 + 0.22
        ),
        precipitation_scaled = 2.21
      )
    )
  ),

  tar_target(
    name = B_comp_contrasts_temperature,
    command = contrast(
      B_comp_emm_temperature,
      method = "trt.vs.ctrl",
      ref = 1,
      adjust = "holm"
    ) |>
      summary(
        type = "response",
        infer = c(TRUE, TRUE)
      ) |>
      as.data.frame()
  ),



  # ------------------------------------------------------------
  # Precipitation-dependent contrasts
  # Temperature held at its mean
  # ------------------------------------------------------------
  tar_target(
    name = B_comp_emm_precipitation,
    command = emmeans(
      mod_B_comp,
      ~ G_treat * F_treat | precipitation_scaled,
      at = list(
        precipitation_scaled = c(0.87, 2.22, 4.65),
        temperature_scaled = 1.05
      )
    )
  ),

  tar_target(
    name = B_comp_contrasts_precipitation,
    command = contrast(
      B_comp_emm_precipitation,
      method = "trt.vs.ctrl",
      ref = 1,
      adjust = "holm"
    ) |>
      summary(
        type = "response",
        infer = c(TRUE, TRUE)
      ) |>
      as.data.frame()
  ),




# ============================================================
# F FUNCTIONAL-GROUP COMPENSATION MODEL
# F retained; effects of G and B removal
# ============================================================

tar_target(
  F_comp_dat,
  standing_biomass_22_wide |>
    filter(F_treat == 0, F > 0)
),

tar_target(
  mod_F_comp,
  glmmTMB(
    F ~
      G_treat * B_treat * temperature_scaled +
      G_treat * B_treat * precipitation_scaled +
      temperature_scaled:precipitation_scaled +
      (1 | siteID/blockID),
    family = Gamma(link = "log"),
    data = F_comp_dat
  )
),

# Fixed-effect coefficient output
tar_target(
  F_comp_model_results,
  broom.mixed::tidy(
    mod_F_comp,
    effects = "fixed",
    conf.int = TRUE
  )
),

tar_target(
  F_comp_model_results_table,
  F_comp_model_results |>
    mutate(
      estimate = round(estimate, 3),
      std.error = round(std.error, 3),
      conf.low = round(conf.low, 3),
      conf.high = round(conf.high, 3),
      p.value = case_when(
        p.value < 0.001 ~ "<0.001",
        TRUE ~ format(round(p.value, 3), nsmall = 3)
      )
    ) |>
    gt::gt() |>
    gt::cols_label(
      term = "Model term",
      estimate = "Estimate",
      std.error = "SE",
      conf.low = "Lower 95% CI",
      conf.high = "Upper 95% CI",
      p.value = gt::md("*p*-value")
    )
),

# DHARMa diagnostics
tar_target(
  F_comp_dharma,
  {
    set.seed(123)
    DHARMa::simulateResiduals(
      fittedModel = mod_F_comp,
      n = 1000
    )
  }
),

tar_target(
  F_comp_dharma_plot,
  plot(F_comp_dharma)
),

# Contrasts at mean climate
tar_target(
  F_comp_emm_mean_climate,
  emmeans::emmeans(
    mod_F_comp,
    ~ G_treat * B_treat,
    at = list(
      temperature_scaled = 1.05,
      precipitation_scaled = 2.21
    )
  )
),

tar_target(
  F_comp_contrasts_mean_climate,
  emmeans::contrast(
    F_comp_emm_mean_climate,
    method = "trt.vs.ctrl",
    ref = 1,
    adjust = "holm"
  ) |>
    summary(type = "response", infer = c(TRUE, TRUE)) |>
    as.data.frame()
),

tar_target(
  F_comp_contrasts_mean_climate_table,
  F_comp_contrasts_mean_climate |>
    mutate(
      ratio = round(ratio, 2),
      asymp.LCL = round(asymp.LCL, 2),
      asymp.UCL = round(asymp.UCL, 2),
      `95% CI` = paste0(asymp.LCL, "–", asymp.UCL),
      p.value = case_when(
        p.value < 0.001 ~ "<0.001",
        TRUE ~ format(round(p.value, 3), nsmall = 3)
      )
    ) |>
    select(contrast, ratio, `95% CI`, p.value) |>
    gt::gt() |>
    gt::cols_label(
      contrast = "Contrast",
      ratio = "Biomass ratio",
      p.value = gt::md("Adjusted *p*-value")
    )
),

# Temperature-dependent contrasts; precipitation fixed at mean
tar_target(
  F_comp_emm_temperature,
  emmeans::emmeans(
    mod_F_comp,
    ~ G_treat * B_treat | temperature_scaled,
    at = list(
      temperature_scaled = c(1.05 - 0.22, 1.05, 1.05 + 0.22),
      precipitation_scaled = 2.21
    )
  )
),

tar_target(
  F_comp_contrasts_temperature,
  emmeans::contrast(
    F_comp_emm_temperature,
    method = "trt.vs.ctrl",
    ref = 1,
    adjust = "holm"
  ) |>
    summary(type = "response", infer = c(TRUE, TRUE)) |>
    as.data.frame()
),


# Precipitation-dependent contrasts; temperature fixed at mean
tar_target(
  F_comp_emm_precipitation,
  emmeans::emmeans(
    mod_F_comp,
    ~ G_treat * B_treat | precipitation_scaled,
    at = list(
      precipitation_scaled = c(0.87, 2.22, 4.65),
      temperature_scaled = 1.05
    )
  )
),

tar_target(
  F_comp_contrasts_precipitation,
  emmeans::contrast(
    F_comp_emm_precipitation,
    method = "trt.vs.ctrl",
    ref = 1,
    adjust = "holm"
  ) |>
    summary(type = "response", infer = c(TRUE, TRUE)) |>
    as.data.frame()
),



# ============================================================
# G FUNCTIONAL-GROUP COMPENSATION MODEL
# G retained; effects of F and B removal
# ============================================================

tar_target(
  G_comp_dat,
  standing_biomass_22_wide |>
    filter(G_treat == 0, G > 0)
),

tar_target(
  mod_G_comp,
  glmmTMB(
    G ~
      F_treat * B_treat * temperature_scaled +
      F_treat * B_treat * precipitation_scaled +
      temperature_scaled:precipitation_scaled +
      (1 | siteID/blockID),
    family = Gamma(link = "log"),
    data = G_comp_dat
  )
),

# Fixed-effect coefficient output
tar_target(
  G_comp_model_results,
  broom.mixed::tidy(
    mod_G_comp,
    effects = "fixed",
    conf.int = TRUE
  )
),

tar_target(
  G_comp_model_results_table,
  G_comp_model_results |>
    mutate(
      estimate = round(estimate, 3),
      std.error = round(std.error, 3),
      conf.low = round(conf.low, 3),
      conf.high = round(conf.high, 3),
      p.value = case_when(
        p.value < 0.001 ~ "<0.001",
        TRUE ~ format(round(p.value, 3), nsmall = 3)
      )
    ) |>
    gt::gt() |>
    gt::cols_label(
      term = "Model term",
      estimate = "Estimate",
      std.error = "SE",
      conf.low = "Lower 95% CI",
      conf.high = "Upper 95% CI",
      p.value = gt::md("*p*-value")
    )
),

# DHARMa diagnostics
tar_target(
  G_comp_dharma,
  {
    set.seed(123)
    DHARMa::simulateResiduals(
      fittedModel = mod_G_comp,
      n = 1000
    )
  }
),

tar_target(
  G_comp_dharma_plot,
  plot(G_comp_dharma)
),

# Contrasts at mean climate
tar_target(
  G_comp_emm_mean_climate,
  emmeans::emmeans(
    mod_G_comp,
    ~ F_treat * B_treat,
    at = list(
      temperature_scaled = 1.05,
      precipitation_scaled = 2.21
    )
  )
),

tar_target(
  G_comp_contrasts_mean_climate,
  emmeans::contrast(
    G_comp_emm_mean_climate,
    method = "trt.vs.ctrl",
    ref = 1,
    adjust = "holm"
  ) |>
    summary(type = "response", infer = c(TRUE, TRUE)) |>
    as.data.frame()
),

tar_target(
  G_comp_contrasts_mean_climate_table,
  G_comp_contrasts_mean_climate |>
    mutate(
      ratio = round(ratio, 2),
      asymp.LCL = round(asymp.LCL, 2),
      asymp.UCL = round(asymp.UCL, 2),
      `95% CI` = paste0(asymp.LCL, "–", asymp.UCL),
      p.value = case_when(
        p.value < 0.001 ~ "<0.001",
        TRUE ~ format(round(p.value, 3), nsmall = 3)
      )
    ) |>
    select(contrast, ratio, `95% CI`, p.value) |>
    gt::gt() |>
    gt::cols_label(
      contrast = "Contrast",
      ratio = "Biomass ratio",
      p.value = gt::md("Adjusted *p*-value")
    )
),

# Temperature-dependent contrasts; precipitation fixed at mean
tar_target(
  G_comp_emm_temperature,
  emmeans::emmeans(
    mod_G_comp,
    ~ F_treat * B_treat | temperature_scaled,
    at = list(
      temperature_scaled = c(1.05 - 0.22, 1.05, 1.05 + 0.22),
      precipitation_scaled = 2.21
    )
  )
),

tar_target(
  G_comp_contrasts_temperature,
  emmeans::contrast(
    G_comp_emm_temperature,
    method = "trt.vs.ctrl",
    ref = 1,
    adjust = "holm"
  ) |>
    summary(type = "response", infer = c(TRUE, TRUE)) |>
    as.data.frame()
),



# Precipitation-dependent contrasts; temperature fixed at mean
tar_target(
  G_comp_emm_precipitation,
  emmeans::emmeans(
    mod_G_comp,
    ~ F_treat * B_treat | precipitation_scaled,
    at = list(
      precipitation_scaled = c(0.87, 2.22, 4.65),
      temperature_scaled = 1.05
    )
  )
),

tar_target(
  G_comp_contrasts_precipitation,
  emmeans::contrast(
    G_comp_emm_precipitation,
    method = "trt.vs.ctrl",
    ref = 1,
    adjust = "holm"
  ) |>
    summary(type = "response", infer = c(TRUE, TRUE)) |>
    as.data.frame()
),


















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
