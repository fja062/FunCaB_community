# data curation plan
transformation_plan <- list(

  # prep biomass
  tar_target(
    name = biomass,
    command = biomass_raw |>
      # remove extra plots in 2016
      filter(treatment != "XC") |>
      # sum biomass from different rounds
      group_by(year, siteID, temperature_level, precipitation_level, blockID, plotID, treatment, removed_fg) |>
      summarise(biomass = sum(biomass))
  ),

  # prep cover
  tar_target(
    name = cover,
    command = community_raw |>
      # remove extra plots in 2016
      filter(treatment != "XC") |>
      select(year:treatment, species, cover, functional_group) |>
      mutate(temperature_level = case_when(siteID %in% c("Ulvehaugen", "Skjelingahaugen", "Lavisdalen", "Gudmedalen") ~ "alpine",
                                           siteID %in% c("Alrust", "Veskre", "Rambera", "Hogsete") ~ "sub-alpine",
                                           TRUE ~ "boreal"),
             temperature_level = factor(temperature_level, levels = c("alpine", "sub-alpine", "boreal")),
             precipitation_level = case_when(siteID %in% c("Fauske", "Alrust", "Ulvehaugen") ~ 1,
                                             siteID %in% c("Vikesland", "Hogsete", "Lavisdalen") ~ 2,
                                             siteID %in% c("Arhelleren", "Rambera", "Gudmedalen") ~ 3,
                                             TRUE ~ 4))
  )

)
