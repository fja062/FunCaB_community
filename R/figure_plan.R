# Figure plan for FunCaB Community Analysis
figure_plan <- list(

  tar_target(
    name = fig_pca,
    command = make_sp_pca_figure(community_pca)
  ),

  tar_target(
    name = fig_fg_richness_effects,
    command = plot_model_effects(fg_richness_analysis$model_2way)
  ),

  tar_target(
    name = fig_fg_identity_effects,
    command = plot_model_effects(fg_identity_analysis$model_2way)
  ),

  tar_target(
    name = fig_fg_identity_interactions,
    command = {
      p1 <- plot_model(fg_identity_analysis$model_2way, type = "pred", terms = c("precipitation_scaled", "fg_removed"))
      p2 <- plot_model(fg_identity_analysis$model_2way, type = "pred", terms = c("temperature_scaled", "fg_removed"))

      p1 + p2 + plot_layout(guides = "collect") & theme_bw()
    }
  ),

  # tar_target(
  #   name = fig_fg_biomass_effects,
  #   command = plot_model_effects(fg_biomass_analysis)
  # ),

  # tar_target(
  #   name = fig_fg_biomass_interactions,
  #   command = {
  #     p1 <- plot_model(fg_biomass_analysis, type = "pred", terms = c("cumulative_removed_biomass", "fg_removed"))
  #     p2 <- plot_model(fg_biomass_analysis, type = "pred", terms = c("temperature_scaled", "fg_removed"))
  #     p3 <- plot_model(fg_biomass_analysis, type = "pred", terms = c("precipitation_scaled", "fg_removed"))

  #     p1 + p2 + p3 + plot_layout(guides = "collect") & theme_bw()
  #   }
  # ),

  ## Part 2

  # fg richness focals
  tar_target(
    name = fig_fg_richness_focal_effects,
    command = plot_tidy_effects_facet(data = fg_richness_focal_tidy, 
                                      facet_var = "focal_fg", 
                                      term_order = rev(c("fg_richness", "T", "P", "fg_richness:T", "fg_richness:P", "P:T")))
  ),

  # fg identity focals
  tar_target(
    name = fig_fg_identity_focal_effects,
    command = plot_tidy_effects_facet(data = fg_identity_focal_tidy, 
                                      facet_var = "focal_fg")
  ),

    tar_target(
    name = fig_fg_identity_focal_interactions,
    command = {
      p1 <- plot_model(fg_identity_focal_analysis$bryophytes$model_2way,
        type = "pred", 
        terms = c("precipitation_scaled", "fg_removed"),
        title = "Bryophytes")
      p2 <- plot_model(fg_identity_focal_analysis$forbs$model_2way, 
        type = "pred", 
        terms = c("precipitation_scaled", "fg_removed"),
        title = "Forbs")
      p3 <- plot_model(fg_identity_focal_analysis$graminoids$model_2way, 
        type = "pred", 
        terms = c("precipitation_scaled", "fg_removed"),
        title = "Graminoids")
      p4 <- plot_model(fg_identity_focal_analysis$bryophytes$model_2way, 
        type = "pred", 
        terms = c("temperature_scaled", "fg_removed"),
        title = "Bryophytes")
      p5 <- plot_model(fg_identity_focal_analysis$forbs$model_2way, 
        type = "pred", 
        terms = c("temperature_scaled", "fg_removed"),
        title = "Forbs")
      p6 <- plot_model(fg_identity_focal_analysis$graminoids$model_2way, 
        type = "pred", 
        terms = c("temperature_scaled", "fg_removed"),
        title = "Graminoids")

      (p1 + p2 + p3) / (p4 + p5 + p6) + plot_layout(guides = "collect") & theme_bw()
    }
  ),

  tar_target(
    name = fig_removed_biomass_stacked,
    command = {
      standardised_removed_biomass_site %>%
        mutate(fg_removed = factor(fg_removed, levels = c("G", "F", "B", "GF", "GB", "FB"))) %>%
        ggplot(aes(x = fg_removed, y = removed_biomass, fill = removed_fg)) +
        geom_col() +
        scale_fill_manual(
          values = c("G" = "green", "F" = "purple", "B" = "orange"),
          name = "Removed FG"
        ) +
        facet_grid(temperature_level ~ precipitation_level) +
        theme_bw() +
        labs(
          x = "Functional group removed",
          y = "Removed biomass"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  ),

  tar_target(
    name = fig_prop_removed_biomass_stacked,
    command = {
      standardised_removed_biomass_site %>%
        mutate(fg_removed = factor(fg_removed, levels = c("G", "F", "B", "GF", "GB", "FB"))) %>%
        ggplot(aes(x = fg_removed, y = prop_removed_biomass_trt, fill = removed_fg)) +
        geom_col() +
        scale_fill_manual(
          values = c("G" = "green", "F" = "purple", "B" = "orange"),
          name = "Removed FG"
        ) +
        facet_grid(temperature_level ~ precipitation_level) +
        theme_bw() +
        labs(
          x = "Functional group removed",
          y = "Proportion removed biomass"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  ),

  tar_target(
    name = fig_standing_biomass_stacked,
    command = {
      standardised_standing_biomass %>%
        mutate(fg_removed = factor(fg_removed, levels = c("G", "F", "B", "GF", "GB", "FB"))) %>%
        ggplot(aes(x = fg_removed, y = biomass, fill = removed_fg)) +
        geom_col() +
        scale_fill_manual(
          values = c("G" = "green", "F" = "purple", "B" = "orange"),
          name = "Functional group"
        ) +
        facet_grid(temperature_level ~ precipitation_level) +
        theme_bw() +
        labs(
          x = "Functional group removed",
          y = "Standing biomass"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  ),

  tar_target(
    name = fig_removed_biomass_comparison,
    command = {
      standardised_removed_biomass_site %>%
        ggplot(aes(x = removed_biomass, y = prop_removed_biomass_trt, color = removed_fg)) +
        geom_point(alpha = 0.6, size = 2) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
        scale_color_manual(
          values = c("G" = "green", "F" = "purple", "B" = "orange"),
          name = "Functional group"
        ) +
        facet_grid(temperature_level ~ precipitation_level) +
        theme_bw() +
        labs(
          x = "Removed biomass (raw)",
          y = "Proportional removed biomass (standardized)"
        )
    }
  ),

  tar_target(
    name = fig_control_biomass_by_site,
    command = {
      removed_biomass %>%
        filter(fg_removed == "FGB") %>%
        group_by(siteID, removed_fg) %>%
        summarise(total_control_biomass = sum(removed_biomass), .groups = "drop") %>%
        ggplot(aes(x = siteID, y = total_control_biomass, fill = removed_fg)) +
        geom_col() +
        scale_fill_manual(
          values = c("G" = "green", "F" = "purple", "B" = "orange"),
          name = "Functional group"
        ) +
        theme_bw() +
        labs(y = "Total control biomass", x = "Site") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  )




)