# Figure plan for FunCaB Community Analysis
figure_plan <- list(

  tar_target(
    name = fig_fg_richness_effects,
    command = plot_model_effects(fg_richness_analysis$model_2way)
  )

)