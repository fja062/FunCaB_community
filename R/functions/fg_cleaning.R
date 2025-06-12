fg_cleaning <- function(community_raw, gridded_climate){
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

  impute_trts
}


