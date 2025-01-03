
# Reshape the data for plotting
df_long <- flag_dfs$jci_flagged %>%
  pivot_longer(cols = -date, names_to = "variable", values_to = "value")

# Plot all lines in the same plot with red for 1 and black for 0
ggplot(df_long, aes(x = date, y = variable, colour = factor(value))) +
  geom_point() +
  labs(title = df_name, x = "", y = "") +
  theme_minimal() +
  scale_color_manual(values = c("0" = "black", "1" = "red")) +
  scale_x_datetime(
    breaks = "1 year",  # Set breaks at 1-year intervals
    labels = scales::date_format("%Y")  # Show only the year in labels
  ) +
  guides(colour = guide_legend(title = "Valid periods (red)")) 

library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)

# Loop through each data frame in the flag_dfs list
lapply(names(flag_dfs), function(name) {

  df_long <- flag_dfs[[name]] %>%
    pivot_longer(cols = -date, names_to = "variable", values_to = "value")
  
  plot <- ggplot(df_long, aes(x = date, y = variable, colour = factor(value))) +
    geom_point() +
    labs(title = name, x = "", y = "") +
    theme_minimal() +
    scale_color_manual(values = c("0" = "black", "1" = "red")) +
    scale_x_datetime(
      breaks = "1 year",  # Set breaks at 1-year intervals
      labels = date_format("%Y")) + # Show only the year in labels
    guides(colour = guide_legend(title = "Valid periods (red)"))
  
  ggsave(
    filename = paste0(flag_dfs[[name]], ".jpeg"),
    plot = plot,
    width = 10,
    height = 10,
    dpi = 300
  )
})

names(plots) <- names(flag_dfs)

# Display all plots
for (plot in plots) {
  print(plot)
}

# Save each plot as a separate file
lapply(plots, function(x) {
  ggsave(
    filename = paste0(x, "_valid_periods.jpeg"),
    plot = plots[[x]],
    width = 10,
    height = 10,
    dpi = 300
  )
})
