# data analysis plan
analysis_plan <- list(

    # calculate diversity metrics
    tar_target(
      name = diversity,
      command = calc_diversity(cover)
    ),


  # make pca
  tar_target(
    name = community_pca,
    command = make_sp_pca(cover)
  )

)
