# Figure plan for FunCaB Community Analysis
figure_plan <- list(

  # Plot total_forbs cover vs removed_biomass for forb removal plots
  tar_target(
    name = forbs_cover_biomass_plot,
    command = {
      # Filter for plots where forbs have been removed (fg_removed contains "F")
      forb_removal_data <- fg_cover_biomass |>
        filter(str_detect(fg_removed, "F")) |>
        # Only keep rows where forbs were actually removed (removed_fg == "F")
        filter(removed_fg == "F")
      
      # Create the plot
      ggplot(forb_removal_data, aes(x = total_forbs, y = removed_biomass)) +
        geom_point(alpha = 0.7, size = 2) +
        geom_smooth(method = "lm", se = TRUE, color = "red") +
        labs(
          title = "Forb Cover vs Removed Biomass",
          subtitle = "Plots where forbs were removed",
          x = "Total Forb Cover (%)",
          y = "Removed Biomass (g/m²)",
          caption = "Data from FunCaB Community Analysis"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 12),
          axis.title = element_text(size = 11),
          axis.text = element_text(size = 10)
        )
    }
  )

)