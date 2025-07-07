#download_data
download_plan <- list(

  # download data
  # biomass
  tar_target(
    name = removed_biomass_download,
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

    # species corrections
  tar_target(
    name = species_corrections_download,
    command =  get_file(node = "tx9r2",
                        file = "FUNCAB_species_corrections.csv",
                        path = "data",
                        remote_path = "1_Vegetation/Raw_data"),
    format = "file"
  ),

    # plant functional traits
    tar_target(
      name = traits_download,
      command =  get_file(node = "npfa9",
                          file = "VCG_clean_trait_data_2012-2016.csv",
                          path = "data",
                          remote_path = "5_Trait_data"),
      format = "file"
    ),

    # climate data
    tar_target(
      name = gridded_climate_download,
      command =  get_file(node = "npfa9",
                          file = "VCG_clean_gridded_daily_climate_2008-2022.csv",
                          path = "data",
                          remote_path = "8_Environmental_data"),
      format = "file"
    ),

  # import data
  # biomass
  tar_target(
    name = removed_biomass_raw,
    command =  read_csv(removed_biomass_download)
  ),

  # community composition
  tar_target(
    name = community_raw,
    command =  read_csv(community_download)
  ),

    # species corrections
  tar_target(
    name = species_corrections_raw,
    command =  read_csv(species_corrections_download)
  ),

    # plant functional traits
    tar_target(
      name = traits_raw,
      command =  read_csv(traits_download)
    ),

    # climate data
    tar_target(
      name = gridded_climate_raw,
      command =  read_csv(gridded_climate_download)
    )
)

