# data curation plan
transformation_plan <- list(

  # mean climate
  tar_target(
    name = gridded_climate,
    command = {
      dat <- gridded_climate_raw |> 
      filter(variable %in% c("temperature", "precipitation"),
              year(date) %in% c(2008:2019)) |> 
      pivot_wider(names_from = variable, values_from = value) |> 
      mutate(year = year(date))
      
      left_join(dat |> 
        group_by(year, siteID) |> 
        summarise(precipitation = sum(precipitation, na.rm = TRUE)) |> 
        ungroup() |> 
        group_by(siteID) |> 
        summarise(precipitation = mean(precipitation, na.rm = TRUE)),
      dat |> 
        mutate(month = month(date)) |> 
        filter(month %in% c(6, 7, 8)) |> 
        group_by(year, month, siteID) |> 
        summarise(temperature = mean(temperature)) |> 
        ungroup() |>
        group_by(siteID) |> 
        summarise(temperature = mean(temperature)),
    by = "siteID")
      
    }
  
      ),


  # prep biomass
  tar_target(
    name = removed_biomass,
    command = removed_biomass_raw |> 
      # remove extra plots in 2016
      filter(treatment != "XC") |>
      # sum biomass from different rounds
      group_by(year, siteID, temperature_level, precipitation_level, blockID, plotID, treatment, removed_fg) |>
      summarise(removed_biomass = sum(biomass)) |>
      ungroup()
  ),

  # 2016 controls
  ### PROBLEM WITH THIS DATA, ONE DUPLICATE???
  tar_target(
    name = standing_biomass,
    command = removed_biomass_raw |> 
      # control plots in 2016
      filter(treatment == "XC") |> 
      rename(standing_biomass = biomass)
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
  ),

tar_target(
  name = fg_cover,
  command = community_raw |>
    # remove extra plots in 2016
    filter(treatment != "XC") |>
    select(year:treatment, total_graminoids, total_forbs, total_bryophytes) |>
    tidylog::distinct() |> 
    #remove duplicates
    dplyr::mutate(n = dplyr::n(), .by = c(year, siteID, blockID, plotID, removal, treatment)) |>
    tidylog::filter(!c(n == 2 & is.na(total_graminoids))) |> 
    # remove last duplicate
    filter(!c(year == 2019 & plotID == "Alr3C" & total_bryophytes == 2)) |> 
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
