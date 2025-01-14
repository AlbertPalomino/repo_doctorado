library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)

# Create a list to store all plots
df_names <- c("Carlini", "Dismal Island", "Escudero", "Esperanza", "Fossil Bluff", "Gabriel  de Castilla", "Hugo Island", "Juan Carlos I", "King Sejong", "Kirkwood Island", "O'Higgins", "Palmer", "Prat", "Racer Rock", "Rothera", "San Martin", "Vernadsky")

i = 1
# Iterate through each data frame in the data_frames
for (i in seq_along(data_frames)) {
  df <- data_frames[[i]]
  df_name <- names(data_frames)[i]
  df_title <- df_names[i]
  
  # Identify valid columns (exclude 'date' and columns with '_')
  valid_columns <- names(df)[!grepl("_", names(df)) & names(df) != "date"]
  
  # List to store plots for the current data frame
  plot_list <- list()
  
  # Iterate through valid columns
  for (j in seq_along(valid_columns)) {
    col_name <- valid_columns[j]
    col_flag <- grep(paste0("^", col_name, "_"), names(df), value = TRUE)
    
    # Create shading ranges based on the flag column
    shading_ranges <- df %>%
      mutate(is_shaded = !!sym(col_flag) == 1) %>%
      group_by(group = cumsum(!is_shaded)) %>%
      filter(is_shaded) %>%
      summarize(
        xmin = first(date),
        xmax = last(date),
        .groups = "drop"
      )
    
    # Plot raw meteo data with valid periods as shaded areas
    plot <- ggplot(df) +
      geom_rect(
        data = shading_ranges,
        aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
        fill = "darkolivegreen3", alpha = 0.5
      ) +
      geom_line(aes(x = date, y = .data[[col_name]]), color = "black", linewidth = 0.5) +
      labs(
        title = "",
        x = "",
        y = col_name
      ) +
      theme_minimal()
    
    # Store the plot in the list
    plot_list[[j]] <- plot
  }

  # Export plots for the current data frame
combined_plot <- grid.arrange(
  grobs = c(
    list(textGrob(
      paste0(df_title, " time series"),
      gp = gpar(fontsize = 12, fontface = "bold"),
      just = "center")), plot_list),
  ncol = 1,
  heights = c(1, rep(10, length(plot_list))) # Adjust heights: 1 for title, 10 for each plot
)

png(filename = paste0(df_title, "_timeseries.png"), width = 3000, height = 3000, res = 300)
grid.draw(combined_plot) 
dev.off()  
}