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
    name = biomass,
    command = removed_biomass_raw |>
    mutate(plotID = if_else(
      treatment == "GF" & str_detect(plotID, "FG"),
        str_replace(plotID, "FG", "GF"),
        plotID)) %>%
    # fix blockID
    funcabization(., convert_to = "Funder") %>%
    make_fancy_data(., gridded_climate, fix_treatment = TRUE)
  ),

  # sum biomass between 2015 and 2019 (without XC)
  tar_target(
    name = removed_biomass,
    command = biomass |>
      # remove extra plots in 2016
      filter(fg_removed != "XC") |>
      # remove 2020 data |>
      filter(year < 2020) |>
      # sum biomass across years
      group_by(siteID, temperature_level, precipitation_level, blockID, plotID, fg_removed, removed_fg) |>
      summarise(removed_biomass = sum(biomass)) |>
      ungroup()
  ),


  # standing biomass for 2016 controls
  ### PROBLEM WITH THIS DATA, ONE DUPLICATE???
  tar_target(
    name = standing_biomass,
    command = biomass |>
      # control plots in 2016
      filter(fg_removed == "XC") |>
      rename(standing_biomass = biomass)
  ),


  # make community data
  tar_target(
    name = community,
    command = {
      transition_community <- community_raw %>%
      mutate(blockID = case_when(
        plotID == "Fau1XC" ~ "Fau1",
        plotID == "Fau4XC" ~ "Fau4",
        plotID == "Fau3XC" ~ "Fau5",
        plotID == "Gud1XC" ~ "Gud12",
        plotID == "Gud4XC" ~ "Gud15",
        TRUE ~ blockID
      )) |>
      # imputing 0s in moss height where moss cover is low
      mutate(moss_height = case_when(
        (is.na(moss_height) & total_bryophytes <= 5) ~ 0,
        TRUE ~ moss_height
      )) |>
        # create sum of covers
        group_by(year, plotID, functional_group) |>
        mutate(sum_cover = sum(cover)) |>
        # keep only the TTC controls in Alrust
      filter(!(plotID == "Alr3C" & is.na(turfID))) %>%
      funcabization(., convert_to = "Funder") %>%
      make_fancy_data(., gridded_climate, fix_treatment = TRUE) %>%
        select(-sumcover) |>
        ungroup()

      impute_ctrls <- transition_community |>
        left_join(
          transition_community |>
            select(year:fg_removed, fg_remaining, vegetation_height, moss_height, total_graminoids, total_forbs, total_bryophytes) |>
        filter(fg_removed == "C") |>
        distinct() |>
        group_by(plotID) |>
        mutate(moss_height2 = case_when(
          (is.na(moss_height)) ~ mean(moss_height, na.rm = TRUE),
          TRUE ~ moss_height))
        ) |>
        mutate(moss_height = if_else((is.na(moss_height) & fg_removed == "C"), moss_height2, moss_height)) |>
        select(-moss_height2) |>
        ungroup()

      impute_trts <- impute_ctrls |>
        left_join(impute_ctrls |>
        select(year:fg_removed, fg_remaining, vegetation_height, moss_height, total_graminoids, total_forbs, total_bryophytes) |>
        tidylog::distinct() |>
        #filter(fg_removed %in% c("C", "XC")) |>
        group_by(siteID, year) |>
        mutate(site_moss_height = mean(moss_height, na.rm = TRUE),
               moss_height2 = if_else(is.na(moss_height), site_moss_height, moss_height))
        ) |>
        mutate(moss_height = if_else((is.na(moss_height)), moss_height2, moss_height),
               imputed_height = if_else(moss_height == site_moss_height, TRUE, FALSE)) |>
        select(-moss_height2, -site_moss_height) |>
        ungroup()

      # testing for outliers in imputed values
      # community |> select(year:fg_removed, fg_remaining, vegetation_height, moss_height, total_graminoids, total_forbs, total_bryophytes, moss_height_imputed, site_moss_height) |> distinct() |> mutate(diff_height = moss_height_imputed - site_moss_height, is_imputed = if_else(moss_height_imputed == site_moss_height, TRUE, FALSE))|> filter(year == 2016) |> ggplot(aes(moss_height_imputed, fill = is_imputed)) + geom_histogram()
      }
  ),


  # prep cover
  tar_target(
    name = cover,
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
    #remove duplicates
    dplyr::mutate(n = dplyr::n(), .by = c(year, siteID, blockID, plotID, removal, fg_removed)) |>
    tidylog::filter(!c(n == 2 & is.na(total_graminoids))) |>
    # remove last duplicate
    filter(!c(year == 2019 & plotID == "Alr3C" & total_bryophytes == 2))

),

  #merge biomass with community
  tar_target(
    name = remaining_biomass_merged,
    command = merge_community_biomass(community, standing_biomass)
  ),

  # make biomass coefficients
  tar_target(
    name = biomass_coefficients,
    command = make_biomass_coefficients(remaining_biomass_merged)
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
      true = suppressWarnings(log(value)),# suppress warnings from log(-value) in isotopes (these are calculated but not kept)
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
      levels = c("height_log", "fresh_mass_log", "dry_mass_log", "leaf_area_log", "leaf_thickness_log", "SLA", "LDMC", "C", "N", "CN_ratio", "d13C", "d15N"))) %>%
        make_fancy_data(., gridded_climate, fix_treatment = FALSE)

),

# bootstrapping
# trait imputation
tar_target(
  name = imputed_traits,
  command = make_trait_impute(cover, traits)
),

# bootstrapping for CWM
tar_target(
  name = trait_means,
  command = make_bootstrapping(imputed_traits)
),

# join response and explanatory variables for analysis
  tar_target(
    name = analysis_data,
    command = biomass_coefficients |>
      # filter for only 2019 data
      filter(year == 2019) |>

      # join with removed biomass
      # 147 biomass_coefficients plots do not join, because removed_biomass has no controls
      # 5 plots from removed_biomass do no join, because they are missing in biomass_coefficients
      tidylog::left_join(removed_biomass, by = join_by(siteID, plotID, fg_removed)) |>
      # add missing blockID for control plots
      mutate(blockID = if_else(is.na(blockID), str_sub(plotID, 1, 4), blockID)) |>

      # join with diversity
      tidylog::left_join(diversity |>
        ungroup() |>
        filter(year == 2019) |>
        select(-removal, -functional_group, -c(temperature_level:temperature)),
          by = join_by(year, siteID, plotID, fg_removed, fg_remaining))

  ),

  # join response and explanatory variables for trait analysis
  tar_target(
    name = analysis_traits,
    command = biomass_coefficients |>
      # filter for only 2019 data
      filter(year == 2019) |>

      # join with removed biomass
      # 147 biomass_coefficients plots do not join, because removed_biomass has no controls
      # 5 plots from removed_biomass do no join, because they are missing in biomass_coefficients
      tidylog::left_join(removed_biomass, by = join_by(siteID, plotID, fg_removed)) |>
      # add missing blockID for control plots
      mutate(blockID = if_else(is.na(blockID), str_sub(plotID, 1, 4), blockID)) |>

      # join with traits
      # WHY DO 720 NOT JOIN???!!!
      tidylog::anti_join(trait_means)

  )

)

