
library(readr)
library(dplyr)
library(ggplot2)

#=======================================================
# Read Trout Bog sonde data
#=======================================================

TB_June_raw <- read_csv(
  "~/Documents/Kuru_Projects/HiRes_Gas_Sampling/Trout_Bog_06_11_2026_High_Res_Gas.csv"
)

TB_August_raw <- read_csv(
  "~/Documents/Kuru_Projects/HiRes_Gas_Sampling/Trout_Bog_08_11_2026_High_Res_Gas.csv"
)

#=======================================================
# Process June temperature profile
#=======================================================

TB_June_profile <-
  
  TB_June_raw %>%
  
  filter(!is.na(Temp), !is.na(Depth)) %>%
  
  mutate(
    Depth = round(Depth / 0.05) * 0.05
  ) %>%
  
  group_by(Depth) %>%
  
  summarize(
    Temperature = mean(Temp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  mutate(Sampling = "June 11, 2026")

#=======================================================
# Process August temperature profile
#=======================================================

TB_August_profile <-
  
  TB_August_raw %>%
  
  filter(!is.na(Temp), !is.na(Depth)) %>%
  
  mutate(
    Depth = round(Depth / 0.05) * 0.05
  ) %>%
  
  group_by(Depth) %>%
  
  summarize(
    Temperature = mean(Temp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  mutate(Sampling = "August 11, 2026")

#=======================================================
# Combine profiles
#=======================================================

TB_temperature_profiles <-
  
  bind_rows(
    TB_June_profile,
    TB_August_profile
  ) %>%
  
  arrange(Sampling, Depth)

#=======================================================
# Plot temperature profiles
#=======================================================

ggplot(
  TB_temperature_profiles,
  aes(
    x = Temperature,
    y = Depth,
    color = Sampling,
    group = Sampling
  )
) +
  
  geom_path(
    linewidth = 1.4
  ) +
  
  geom_point(
    size = 1.8
  ) +
  
  scale_y_reverse(
    limits = c(7.5, 0),
    breaks = seq(0, 7, 1),
    expand = c(0, 0)
  ) +
  
  scale_x_continuous(
    name = "Temperature (°C)",
    breaks = seq(0, 25, 5),
    limits = c(0, 25),
    position = "top"
  ) +
  
  scale_color_manual(
    values = c(
      "June 11, 2026" = "steelblue",
      "August 11, 2026" = "firebrick"
    )
  ) +
  
  labs(
    y = "Depth (m)",
    color = NULL,
    title = "Trout Bog Temperature Profiles",
    subtitle = "June vs. August 2026"
  ) +
  
  theme_classic(base_size = 16) +
  
  theme(
    axis.title = element_text(
      face = "bold",
      size = 18
    ),
    
    axis.text = element_text(size = 14),
    
    plot.title = element_text(
      face = "bold",
      size = 20
    ),
    
    legend.position = "top",
    legend.text = element_text(size = 14)
  )





