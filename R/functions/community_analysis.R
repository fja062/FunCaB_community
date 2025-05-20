### community analysis functions

calc_diversity <- function(cover){

  cover %>%
     group_by(year, siteID, blockID, plotID, removal, fg_removed, fg_remaining,
      functional_group, temperature_level, precipitation_level) %>%
     summarise(richness = n(),
               diversity = diversity(cover),
               evenness = diversity/log(richness))
}



### MAKE SP PCA
make_sp_pca <- function(cover){

  comm_wide <- cover |>
    filter(!is.na(cover)) |>
    select(-functional_group) |>
    mutate(cover = sqrt(cover)) |>
    pivot_wider(names_from = species, values_from = cover, values_fill = 0)

  comm_sp <- comm_wide |>
    select(-c(year:precipitation_level))

  comm_info <- comm_wide |>
    select(c(year:precipitation_level))


  # make pca
  res <- rda(comm_sp)

  out <- bind_cols(comm_info, fortify(res) |>
                     filter(score == "sites"))

  sp <- fortify(res) |>
    filter(score == "species")

  return(list(out, sp, res))

}

get_fg_cover <- function(fg_cover){

  fg_cover |>
    select(year:treatment, graminoid = total_graminoids, forb = total_forbs,
      bryophyte = total_bryophytes) |>
    pivot_wider(names_from = treatment,
                values_from = c(graminoid, forb, bryophyte)) |>
    pivot_longer()

}

