library(ggplot2)
library(tidyr)
library(scales)

setwd("/Users/albert/Desktop/")

d <- read.csv("/Users/albert/Desktop/areas_subcolonias.csv", header = TRUE, sep = ";", check.names = FALSE)

df_long <- d %>% 
  pivot_longer(cols = everything(), names_to = "Year", values_to = "Value")

# Plot histograms
hist <- ggplot(stack(d), aes(x = values, fill = ind)) +
  geom_histogram(aes(y = ..density..), binwidth = 100, color = "black") +
  labs(title = "Subcolony Size Distribution", x = "Number of nests", y = "Frequency") +
  facet_wrap(~ ind, ncol = 2) +
  scale_x_continuous(breaks = function(x) seq(min(0), max(x), 100)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = -1),
        panel.grid.major.x = element_blank(),
        legend.position = "none") 
hist
ggsave("histogram.jpeg", width = 14, height = 8)  

# Boxplot
ggplot(df_long, aes(y = Value, x = Year)) +
  geom_boxplot(fill = "cyan4") +
  labs(title = "Subcolony size evolution", x = "", y = "Number of nests")
ggsave("boxplot1.png", width = 7, height = 6)  

# Linear model
m <- lm(Value ~ Year, data = df_long)
summary(m)
