# Biomass

# Clean 2018 Ovstedalen duplicates
# Remove duplicate entries from Ovstedalen 2018 while preserving legitimate duplicates from 2016-2017
clean_ovstedalen_2018_duplicates <- function(data) {
  # Remove duplicates from 2018 Ovstedalen
  # Keep only the first occurrence of each plot-functional group combination
  cleaned_data <- data |>
    filter(!(siteID == "Ovstedalen" & year == 2018)) |>
    bind_rows(
      data |>
        filter(siteID == "Ovstedalen", year == 2018) |>
        group_by(plotID, removed_fg) |>
        slice(1) |>
        ungroup()
    )

  return(cleaned_data)
}

#' Remove rows where removed_fg contains letters not present in fg_removed
#' @param df A data frame with columns removed_fg and fg_removed
#' @return The input data frame with invalid rows removed
remove_invalid_removed_fg_rows <- function(df) {
  is_valid_removed_fg <- function(removed_fg, fg_removed) {
    removed_letters <- str_split(removed_fg, "", simplify = TRUE)
    fg_letters <- str_split(fg_removed, "", simplify = TRUE)
    all(removed_letters %in% fg_letters)
  }
  df <- df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(valid = is_valid_removed_fg(removed_fg, fg_removed)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(valid) %>%
    dplyr::select(-valid)
  return(df)
}

#' Annotate 2022 biomass rows with functional group status
#' Adds a column fg_status: 'removed' if all letters in removed_fg are in fg_removed, 'remaining' otherwise
#' @param df A data frame with columns removed_fg and fg_removed
#' @return The input data frame with an added fg_status column
annotate_2022_fg_status <- function(df) {
  df %>%
    rowwise() %>%
    mutate(fg_status = if (all(str_split(removed_fg, "", simplify = TRUE) %in% str_split(fg_removed, "", simplify = TRUE))) "removed" else "remaining") %>%
    ungroup()
}
