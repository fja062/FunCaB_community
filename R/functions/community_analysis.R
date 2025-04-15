### community analysis functions

calc_diversity <- function(cover){
  
  cover %>%
     group_by(year, siteID, blockID, plotID, removal, treatment, 
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
  

cover |> 
  filter(treatment == "B")
  distinct(functional_group)
  pivot_wider(names_from = treatment, values_from = cover)