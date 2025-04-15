#download_data
download_plan <- list(

  # download data
  # biomass
  tar_target(
    name = biomass_download,
    command =  get_file(node = "4c5v2",
                        file = "FunCaB_clean_biomass_2015-2021.csv",
                        path = "data",
                        remote_path = "1_Biomass_removal"),
  format = "file"
  ),

  # community composition
  tar_target(
    name = community_download,
    command =  get_file(node = "4c5v2",
                        file = "FunCaB_clean_composition_2015-2019.csv",
                        path = "data",
                        remote_path = "3_Plant_composition"),
    format = "file"
  ),

  # import data
  # biomass
  tar_target(
    name = biomass_raw,
    command =  read_csv(biomass_download)
  ),

  # community composition
  tar_target(
    name = community_raw,
    command =  read_csv(community_download)
  )
)

