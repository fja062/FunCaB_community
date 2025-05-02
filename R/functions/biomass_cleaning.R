# cover
## correct height data of specific plots
fg_cover |>
  mutate(vegetation_height = case_when(
  plotID == "Ram4F" & year == 2015 ~ 80,
  plotID == "Alr2C" & year == 2015 ~ 197.5,
  TRUE ~ vegetation_height
  )
)


# biomass
standing_biomass_test <- standing_biomass |>
  select(siteID, temperature_level, precipitation_level, blockID, plotID, functional_group, standing_biomass) |>
  filter(functional_group %in% c("graminoids", "forb" , "bryophytes")) |>
  mutate(functional_group = case_when(
    plotID == "Hog1XC" & standing_biomass == 29.44 ~ "graminoids",
    TRUE ~ functional_group
  )) |>
  mutate(standing_biomass = standing_biomass/0.0625) |>   # recalculate to g/m2
  left_join(
    # FG cover in XC plots from 2016
    community_raw |>
      filter(treatment == "XC") |>
      select(year:treatment, vegetation_height, moss_height, total_graminoids, total_forbs, total_bryophytes) |>
      tidylog::distinct() |>
      #remove duplicates
      dplyr::mutate(n = dplyr::n(), .by = c(year, siteID, blockID, plotID, removal, treatment)) |>
      tidylog::filter(!c(n == 2 & is.na(total_graminoids))) |>
      # remove last duplicate
      mutate(temperature_level = case_when(siteID %in% c("Ulvehaugen", "Skjelingahaugen", "Lavisdalen", "Gudmedalen") ~ "alpine",
                                           siteID %in% c("Alrust", "Veskre", "Rambera", "Hogsete") ~ "sub-alpine",
                                           TRUE ~ "boreal"),
             temperature_level = factor(temperature_level, levels = c("alpine", "sub-alpine", "boreal")),
             precipitation_level = case_when(siteID %in% c("Fauske", "Alrust", "Ulvehaugen") ~ 1,
                                             siteID %in% c("Vikesland", "Hogsete", "Lavisdalen") ~ 2,
                                             siteID %in% c("Arhelleren", "Rambera", "Gudmedalen") ~ 3,
                                             TRUE ~ 4)) |>
      pivot_longer(c(total_graminoids, total_bryophytes, total_forbs), names_to = "functional_group", values_to = "cover") |>
      mutate(functional_group = gsub("total_", "", functional_group)) |>
      select(-blockID)
  ) |>
  group_by(plotID, functional_group) |>
  mutate(cover_height = case_when(
   functional_group == "bryophytes" ~ cover * moss_height,
   functional_group == "graminoids" ~  cover * vegetation_height,
   functional_group == "forbs" ~  cover * vegetation_height
  )
) |>
  ungroup()

####### biomass regressions
standing_biomass_test |>
  filter(functional_group == "bryophytes") |>
  ggplot(aes(cover, standing_biomass))+
  geom_point()+
  stat_smooth(method = "lm")

standing_biomass_test |>
  filter(functional_group == "bryophytes") |>
  ggplot(aes(cover_height, standing_biomass))+
  geom_point()+
  stat_smooth(method = "lm")

b2<-ggplot(composition_biomass, aes(moss_coverXheight, bryophytes))+
  geom_point()+
  stat_smooth(method = "lm")

g1<-ggplot(composition_biomass, aes(graminoidCov, graminoids))+
  geom_point()+
  stat_smooth(method = "lm")

g2<-ggplot(composition_biomass, aes(vegetationHeight, graminoids))+
  geom_point()+
  stat_smooth(method = "lm")

f1<-ggplot(composition_biomass, aes(forbCov, forbs))+
  geom_point()+
  stat_smooth(method = "lm")

f2<-ggplot(composition_biomass, aes(vegetationHeight, forbs))+
  geom_point()+
  stat_smooth(method = "lm")

# supplementary material fig of cover by height
# add r2 values
plot_grid(b1, b2, g1, g2, f1, f2, labels = c('Bcover', 'BcoverXheight','Gcover', 'GcoverXheight', 'Fcover', 'FcoverXheight' ), label_size = 12)


ggsave(filename = "~/OneDrive - University of Bergen/research/FunCaB/paper 2/figures/supFig_1.jpg", dpi = 300, width = 9, height = 6)

# linear model by functionalGroup
lmForb <- summary(lm(standing_biomass ~ 0 + cover, data = standing_biomass_test |> filter(functional_group == "forbs")))$coefficients %>% as_tibble()
lmBryo <- summary(lm(bryophytes ~ 0 + moss_coverXheight, data = composition_biomass))$coefficients %>% as_tibble()
lmGram <- summary(lm(graminoids ~ 0 + graminoidCov, data = composition_biomass))$coefficients %>% as_tibble()

coefs <- bind_rows("forb" = lmForb, "graminoid" = lmGram, "bryophyte" = lmBryo, .id = "functionalGroup")


# create biomass estimates
community_1516 <- community_1516 %>%
  filter(!Treatment == "XC", Year == 2015) %>%
  mutate(biomassGram = graminoidCov*lmGram$Estimate,
         biomassForb = forbCov*lmForb$Estimate,
         biomassBryo = bryophyteCov*lmBryo$Estimate)

# save data
save(community_1516, file = "~/OneDrive - University of Bergen/research/FunCaB/Data/secondary/communityBiomass_cleaned.RData")
