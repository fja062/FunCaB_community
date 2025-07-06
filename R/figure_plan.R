# Figure plan for FunCaB Community Analysis
figure_plan <- list(

  # Two-panel plot: Graminoid and Forb cover vs removed biomass
  tar_target(
    name = fg_cover_biomass_panels,
    command = {
      # Filter for plots where graminoids have been removed (fg_removed contains "G")
      graminoid_removal_data <- fg_cover_biomass |>
        filter(str_detect(fg_removed, "G")) |>
        # Only keep rows where graminoids were actually removed (removed_fg == "G")
        filter(removed_fg == "G")
      
      # Filter for plots where forbs have been removed (fg_removed contains "F")
      forb_removal_data <- fg_cover_biomass |>
        filter(str_detect(fg_removed, "F")) |>
        # Only keep rows where forbs were actually removed (removed_fg == "F")
        filter(removed_fg == "F")
      
      # Calculate R² for graminoid plot
      graminoid_model <- lm(removed_biomass ~ total_graminoids, data = graminoid_removal_data)
      graminoid_r2 <- round(summary(graminoid_model)$r.squared, 3)
      
      # Create graminoid plot
      p1 <- ggplot(graminoid_removal_data, aes(x = total_graminoids, y = removed_biomass)) +
        geom_point(alpha = 0.7, size = 2, color = "blue") +
        geom_smooth(method = "lm", se = TRUE, color = "darkblue") +
        annotate("text", x = Inf, y = Inf, 
                label = paste("R² =", graminoid_r2), 
                hjust = 1.1, vjust = 1.5, 
                size = 4, fontface = "bold", color = "darkblue") +
        labs(
          title = "Graminoid Cover vs Removed Biomass",
          subtitle = "Plots where graminoids were removed",
          x = "Total Graminoid Cover (%)",
          y = "Removed Biomass (g/m²)"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 12, face = "bold"),
          plot.subtitle = element_text(size = 10),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 9)
        )
      
      # Calculate R² for forb plot
      forb_model <- lm(removed_biomass ~ total_forbs, data = forb_removal_data)
      forb_r2 <- round(summary(forb_model)$r.squared, 3)
      
      # Create forb plot
      p2 <- ggplot(forb_removal_data, aes(x = total_forbs, y = removed_biomass)) +
        geom_point(alpha = 0.7, size = 2, color = "red") +
        geom_smooth(method = "lm", se = TRUE, color = "darkred") +
        annotate("text", x = Inf, y = Inf, 
                label = paste("R² =", forb_r2), 
                hjust = 1.1, vjust = 1.5, 
                size = 4, fontface = "bold", color = "darkred") +
        labs(
          title = "Forb Cover vs Removed Biomass",
          subtitle = "Plots where forbs were removed",
          x = "Total Forb Cover (%)",
          y = "Removed Biomass (g/m²)"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 12, face = "bold"),
          plot.subtitle = element_text(size = 10),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 9)
        )
      
      # Combine plots using patchwork
      combined_plot <- p1 + p2 +
        plot_layout(ncol = 2) +
        plot_annotation(
          title = "Functional Group Cover vs Removed Biomass",
          subtitle = "Relationship between standing cover and removed biomass",
          caption = "Data from FunCaB Community Analysis",
          theme = theme(
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 12),
            plot.caption = element_text(size = 10)
          )
        )
      
      combined_plot
    }
  )

)