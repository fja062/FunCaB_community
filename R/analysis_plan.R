# data analysis plan
analysis_plan <- list(

  # prep biomass
  tar_target(
    name = community_pca,
    command = make_sp_pca(cover)
  )

)
