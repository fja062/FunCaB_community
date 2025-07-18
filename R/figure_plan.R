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

  tar_target(
    name = fig_fg_biomass_effects,
    command = plot_model_effects(fg_biomass_analysis)
  ),

  tar_target(
    name = fig_fg_biomass_interactions,
    command = {
      p1 <- plot_model(fg_biomass_analysis, type = "pred", terms = c("cumulative_removed_biomass", "fg_removed"))
      p2 <- plot_model(fg_biomass_analysis, type = "pred", terms = c("temperature_scaled", "fg_removed"))
      p3 <- plot_model(fg_biomass_analysis, type = "pred", terms = c("precipitation_scaled", "fg_removed"))

      p1 + p2 + p3 + plot_layout(guides = "collect") & theme_bw()
    }
  ),

  tar_target(
    name = fig_fg_richness_G_effects,
    command = plot_model_effects(fg_richness_G_analysis$model_2way)
  ),

  tar_target(
    name = fig_fg_identity_G_effects,
    command = plot_model_effects(fg_identity_G_analysis$model_2way)
  ),

  tar_target(
    name = fig_fg_identity_G_interactions,
    command = {
      p1 <- plot_model(fg_identity_G_analysis$model_2way, type = "pred", terms = c("precipitation_scaled", "fg_removed"))
      p2 <- plot_model(fg_identity_G_analysis$model_2way, type = "pred", terms = c("temperature_scaled", "fg_removed"))

      p1 + p2 + plot_layout(guides = "collect") & theme_bw()
    }
  )




)