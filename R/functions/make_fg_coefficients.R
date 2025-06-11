# 2 use sum of covers when fg cover is missing, *according to n of forbs*
# vegetation height
make_fg_cover_coefficients <- function(data){

# graminoids
graminoid_cor <- data |>
  select(year:fg_removed, fg_remaining, vegetation_height, moss_height, total_graminoids, total_forbs, total_bryophytes, functional_group, cover, sum_cover, species, temperature_level, precipitation_level) |>
  filter(!is.na(total_graminoids), fg_remaining %in% c("G", "GB", "GF", "FGB"), functional_group == "graminoid")

# linear model to calculate graminoid cover coefficient
graminoid_coefficient <- summary(lm(total_graminoids ~ sum_cover*vegetation_height, data = graminoid_cor)) |> broom::tidy() |> filter(term == "sum_cover:vegetation_height")
broom::glance(lm(total_graminoids ~ sum_cover, data = graminoid_cor))

graminoid_cor |>
  ggplot(aes(x = sum_cover, y = total_graminoids, size = vegetation_height)) +
  geom_point() +
  geom_smooth(method = "lm")


# forbs
forb_cor <- data |>
  select(year:fg_removed, fg_remaining, vegetation_height, moss_height, total_graminoids, total_forbs, total_bryophytes, functional_group, cover, sum_cover, species, temperature_level, precipitation_level) |>
  filter(!is.na(total_forbs), fg_remaining %in% c("F", "FB", "GF", "FGB"), functional_group == "forb") |>
  group_by(plotID, year) |>
  mutate(n_species = n_distinct(species)) |>
  ungroup()

# linear model to calculate graminoid cover coefficient
forb_coefficient <- lmer(total_forbs ~ sum_cover*vegetation_height + (1|n_species), data = forb_cor) |> broom.mixed::tidy() |> filter(term == "sum_cover:vegetation_height")
broom.mixed::glance(lmer(total_forbs ~ sum_cover*vegetation_height + (1|n_species), data = forb_cor))


forb_cor |>
  ggplot(aes(x = sum_cover, y = total_forbs, size = vegetation_height)) +
  geom_point() +
  geom_smooth(method = "lm")


# multiply sum of covers by fg coefficients to impute missing total_forb/graminoid values
community <- data |>
  mutate(total_graminoids = if_else(is.na(total_graminoids) & fg_remaining %in% c("G", "GB", "GF", "FGB"), sum_cover*graminoid_coefficient$estimate, total_graminoids),
         total_forbs = if_else(is.na(total_forbs) & fg_remaining %in% c("F", "FB", "GF", "FGB"), sum_cover*forb_coefficient$estimate, total_forbs))

community

}




#community |>
#  select(year:fg_removed, fg_remaining, vegetation_height, moss_height, total_graminoids, total_forbs, #total_bryophytes, sum_cover) |>
#  tidylog::distinct() |>
#  filter(is.na(total_bryophytes), fg_remaining %in% c("GB", "B", "FGB", "FB"))


