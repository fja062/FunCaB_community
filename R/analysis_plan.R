# data analysis plan
analysis_plan <- list(

  # make pca
  tar_target(
    name = community_pca,
    command = make_sp_pca(cover)
  )

)
