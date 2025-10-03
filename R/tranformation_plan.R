# data curation plan
transformation_plan <- list(

  # mean climate
  tar_target(
    name = gridded_climate,
    command = {
      dat <- gridded_climate_raw |>
        filter(
          variable %in% c("temperature", "precipitation"),
          year(date) %in% c(2008:2019)
        ) |>
        pivot_wider(names_from = variable, values_from = value) |>
        mutate(year = year(date))

      left_join(
        dat |>
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
        by = "siteID"
      )
    }
  ),

  # prep biomass
  # 2015-2021
  tar_target(
    name = biomass,
    command = removed_biomass_raw |>
      # Clean 2018 Ovstedalen duplicates
      clean_ovstedalen_2018_duplicates() |>
      mutate(plotID = if_else(
        treatment == "GF" & str_detect(plotID, "FG"),
        str_replace(plotID, "FG", "GF"),
        plotID
      )) |>
      # remove extra plots in 2016
      filter(treatment != "XC") %>%
      # fix blockID
      funcabization(., convert_to = "Funder") %>%
      make_fancy_data(., gridded_climate, fix_treatment = TRUE) |>
      # remove invalid removed_fg rows
      remove_invalid_removed_fg_rows() |>
      mutate(
        fg_status = "removed",
        # scale precipitation and temperature
        precipitation_scaled = precipitation / 1000,
        temperature_scaled = temperature / 10
      )
  ),

  # prep biomass (2022 only)
  tar_target(
    name = biomass_22,
    command = removed_biomass_final_raw %>%
      # convert to Funder format
      funcabization(., convert_to = "Funder") %>%
      make_fancy_data(., gridded_climate, fix_treatment = TRUE) |>
      # remove litter
      filter(removed_fg != "L") |>
      # annotate 2022 fg status
      annotate_2022_fg_status() |>
      select(-no_treatment) |>
      # scale precipitation and temperature
      mutate(
        precipitation_scaled = precipitation / 1000,
        temperature_scaled = temperature / 10
      )
  ),

  # prep standing biomass 2022 (contains reminaing biomass)
  tar_target(
    name = standing_biomass_22,
    command = biomass_22 |>
      filter(fg_status == "remaining" | fg_removed == "FGB") |>
      mutate(biomass = if_else(fg_removed == "FGB", 0, biomass)) |>
      mutate(fg_removed = factor(fg_removed,
             levels = c("none", "G", "F", "B", "GF", "GB", "FB", "FGB")))
  ),

  # standardised standing biomass
  tar_target(
    name = standardised_standing_biomass,
    command = standing_biomass_22 |>
      filter(!fg_remaining %in% c("FGB", "none")) |>
      # join to the site-level mean for controls per functional group in 2022
      tidylog::left_join(standing_biomass_22 |>
                           filter(fg_remaining == "FGB") |>
                           group_by(siteID, removed_fg, fg_status, temperature_level, precipitation_level, temperature_scaled, precipitation_scaled) |>
                           summarise(mean_standing_biomass = mean(biomass)),
                         by = join_by(siteID, removed_fg, fg_status, temperature_level, precipitation_level, temperature_scaled, precipitation_scaled)) |>
      # create deltas from the site-level means
      mutate(delta_standing_biomass = biomass-mean_standing_biomass)

  ),


  # filter for biomass in 2015 (without XC)
  tar_target(
    name = removed_biomass,
    command = biomass |>
      # remove mistakenly cut forb biomass from Ovs1B in 2019
      # MADE A FUNCTION THAT REMOVES THIS. SHOULD REMOVED_FG == B ALSO BE REMOVED?
      # filter(!(plotID == "Ovs1B" & removed_fg == "F")) |>
      # add 2022 data (only removed biomass)
      bind_rows(biomass_22 |>
        filter(fg_status == "removed" | fg_removed == "none") |>
        mutate(biomass = if_else(fg_removed == "none", 0, biomass))) |>
      filter(year == 2015) |>
      # sum biomass across the growing season
      group_by(siteID, blockID, plotID, fg_removed, removed_fg, fg_remaining, fg_richness, fg_status, temperature_level, precipitation_level, temperature_scaled, precipitation_scaled) |>
      summarise(removed_biomass = sum(biomass)) |>
      ungroup()
  ),


  # standardised removed biomass
  tar_target(
    name = standardised_removed_biomass,
    command =
      {biomass_proportions_control <- removed_biomass |>
          filter(fg_removed == "FGB") |>

          # calculate mean biomass per FG in control plots (per site)
          group_by(siteID, removed_fg, fg_status) |>
          mutate(mean_removed_biomass_ctrl = mean(removed_biomass)) |>
          ungroup() |>

          # calculate total biomass in each control plot per site
          group_by(siteID, plotID, fg_status) |>
          mutate(total_removed_biomass_ctrl = sum(removed_biomass)) |>

          # calculate site-level mean of total biomass in control plots
          group_by(siteID, fg_status) |>
          mutate(mean_total_removed_biomass_ctrl = mean(total_removed_biomass_ctrl)) |>

          # calculate proportion of each functional group in 2015 control plots
          mutate(prop_removed_biomass_ctrl = mean_removed_biomass_ctrl/mean_total_removed_biomass_ctrl) |>
          ungroup() |>
          # extract site-level biomass proportion and mean biomass of focal FG
          distinct(siteID, removed_fg, fg_status, prop_removed_biomass_ctrl, mean_removed_biomass_ctrl)

      removed_biomass |>
        filter(fg_removed != "FGB") |>
        tidylog::left_join(biomass_proportions_control,
                           by = join_by(siteID, removed_fg, fg_status)) |>

        # calculate cross product to get proportion of each FG in treatment plots
        mutate(prop_removed_biomass_trt = (prop_removed_biomass_ctrl * removed_biomass)/mean_removed_biomass_ctrl)
      }
  ),



  # make community data, impute missing cover values, construct FG cover coefficients
  tar_target(
    name = community,
    command = fg_cleaning(community_raw, gridded_climate, species_corrections_raw) %>%
      make_fg_cover_coefficients()
  ),


  # prep cover
  tar_target(
    name = cover_data,
    command = community |>
      # remove extra plots in 2016
      filter(fg_removed != "XC") |>
      select(year:fg_removed, species, cover, functional_group, temperature_level:fg_remaining)
  ),


  tar_target(
    name = fg_cover,
    command = community |>
      # remove extra plots in 2016
      filter(fg_removed != "XC") |>
      select(year:fg_removed, vegetation_height, moss_height, total_graminoids, total_forbs, total_bryophytes) |>
      tidylog::distinct() |>
      # remove duplicates
      dplyr::mutate(n = dplyr::n(), .by = c(year, siteID, blockID, plotID, removal, fg_removed)) |>
      tidylog::filter(!c(n == 2 & is.na(total_graminoids))) |>
      # remove last duplicate
      filter(!c(year == 2019 & plotID == "Alr3C" & total_bryophytes == 2))
  ),


  # calculate diversity metrics
  tar_target(
    name = diversity,
    command = calc_diversity(cover_data)
  ),

  tar_target(
    name = traits,
    command = traits_raw |>
      select(-date, -flag) |>
      # log transform size traits
      mutate(
        value_trans = if_else(
          trait %in% c(
            "height",
            "fresh_mass",
            "dry_mass",
            "leaf_area",
            "leaf_thickness"
          ),
          true = suppressWarnings(log(value)), # suppress warnings from log(-value) in isotopes (these are calculated but not kept)
          false = value
        ),
        trait_trans = recode(
          trait,
          "height" = "height_log",
          "fresh_mass" = "fresh_mass_log",
          "dry_mass" = "dry_mass_log",
          "leaf_area" = "leaf_area_log",
          "leaf_thickness" = "leaf_thickness_log"
        ),
        trait_trans = factor(trait_trans,
          levels = c("height_log", "fresh_mass_log", "dry_mass_log", "leaf_area_log", "leaf_thickness_log", "SLA", "LDMC", "C", "N", "CN_ratio", "d13C", "d15N")
        )
      ) %>%
      make_fancy_data(., gridded_climate, fix_treatment = FALSE)
  ),

  # bootstrapping
  # trait imputation
  tar_target(
    name = imputed_traits,
    command = make_trait_impute(cover_data, traits)
  ),

  # bootstrapping for CWM
  tar_target(
    name = trait_means,
    command = make_bootstrapping(imputed_traits)
  ),

  # analysis data
  # standing biomass
    tar_target(
      name = sb_long,
      command = standardised_standing_biomass |>
        mutate(fg_status = "remaining") |>
        select(-year, -removed_fg, -fg_status, -comments, -temperature, -precipitation) |>
        rename(standing_biomass = biomass)
    ),

    tar_target(
      name = sb_wide,
      command = standardised_standing_biomass |>
        mutate(fg_status = "remaining") |>
        select(-year, -fg_status, -comments, -temperature, -precipitation) |>
        rename(standing_biomass = biomass) |>
        pivot_wider(names_from = removed_fg, values_from = standing_biomass, names_prefix = "sb_")
    ),


    # removed biomass long
    tar_target(
      name = removed_biomass_long,
      command = standardised_removed_biomass |>
        mutate(fg_status = "removed") |>
        select(-fg_status)
    ),

    # removed biomass wide
    tar_target(
      name = removed_biomass_wide,
      command = standardised_removed_biomass |>
        mutate(fg_status = "removed") |>
        select(-fg_status) |>
        pivot_wider(names_from = removed_fg, values_from = prop_removed_biomass_trt, names_prefix = "rem_")
    )

)
