
# Reshape the data for plotting
df_long <- flags$jci_flagged %>%
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

# Loop through each data frame in the flags list
lapply(names(flags), function(name) {

  df_long <- flags[[1]] %>%
    pivot_longer(cols = -date, names_to = "variable", values_to = "value")
  
plot_title <- "carlini"

plot_flags <- function(df_long, plot_title) {
  ggplot(df_long, aes(x = date, y = variable, colour = factor(value))) +
    geom_point() +
    labs(title = plot_title, x = "", y = "") +
    theme_minimal() +
    scale_color_manual(values = c("0" = "black", "1" = "red")) +
    scale_x_datetime(
      #breaks = "1 year",  # Set breaks at 1-year intervals
      labels = date_format("%Y")) + # Show only the year in labels
    guides(colour = guide_legend(title = "Valid periods (red)"))
}

# Create a vector of names
df_names <- c("Carlini", "Dismal Island", "Escudero", "Esperanza", "Fossil Bluff", "Gabriel  de Castilla", "Hugo Island", "Juan Carlos I", "King Sejong", "Kirkwood Island", "O'Higgins", "Palmer", "Prat", "Racer Rock", "Rothera", "San Martin", "Vernadsky")

# Loop through each data frame in the list using their position
for (i in seq_along(flags)) {
  # Get the current data frame
  df_long <- flags[[i]]  %>%
    pivot_longer(cols = -date, names_to = "variable", values_to = "value")
  plot_title <- df_names[i]
  plot <- plot_flags(df_long, plot_title)
  png(filename = paste0(plot_title, ".png"), width = 3000, height = 1000, res = 300)
  print(plot)
  dev.off()
}

names(plots) <- names(flags)

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
