library(readr)
library(dplyr)
library(ggplot2)

#---------------------------------------
# Import data
#---------------------------------------

TB_gas_data <- read_csv(
  "~/Documents/Kuru_Projects/HiRes_Gas_Sampling/Trout_Bog_06_11_2026_High_Res_Gas.csv"
)

SS_gas_data <- read_csv(
  "~/Documents/Kuru_Projects/HiRes_Gas_Sampling/South_Sparkling_06_12_2026_High_Res_Gas.csv"
)

#---------------------------------------
# Process Trout Bog
#---------------------------------------

TB_plot <- TB_gas_data %>%
  filter(Turbidity < 50) %>%                 # Remove sediment measurements
  mutate(Depth = round(Depth / 0.05) * 0.05) %>%
  group_by(Depth) %>%
  summarize(
    Turbidity = mean(Turbidity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Lake = "Trout Bog")

#---------------------------------------
# Process South Sparkling Bog
#---------------------------------------

SS_plot <- SS_gas_data %>%
  filter(Turbidity < 50) %>%                 # Remove sediment measurements
  mutate(Depth = round(Depth / 0.05) * 0.05) %>%
  group_by(Depth) %>%
  summarize(
    Turbidity = mean(Turbidity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Lake = "South Sparkling Bog")

#---------------------------------------
# Combine datasets
#---------------------------------------

gas_profiles <- bind_rows(TB_plot, SS_plot)

#---------------------------------------
# Plot
#---------------------------------------

ggplot(gas_profiles, aes(Turbidity, Depth)) +
  
  geom_path(
    color = "darkgreen",
    linewidth = 1.2
  ) +
  
  facet_wrap(~Lake, nrow = 1) +
  
  scale_y_reverse(
    limits = c(7.5, 0),
    breaks = seq(0, 7, 1),
    expand = c(0, 0)
  ) +
  
  scale_x_continuous(
    limits = c(0, 10),
    breaks = seq(0, 10, 2),
    position = "top",
    expand = c(0, 0)
  ) +
  
  labs(
    x = "Turbidity (FNU)",
    y = "Depth (m)"
  ) +
  
  theme_classic(base_size = 16) +
  
  theme(
    strip.background = element_blank(),
    strip.text = element_text(
      face = "bold",
      size = 18
    ),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    panel.spacing = unit(1.5, "lines")
  )

