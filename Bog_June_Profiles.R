library(readr)
library(dplyr)
library(ggplot2)

#=======================================================
# Read sonde data
#=======================================================

TB_sonde_raw <- read_csv(
  "~/Documents/Kuru_Projects/HiRes_Gas_Sampling/Trout_Bog_06_11_2026_High_Res_Gas.csv"
)

SS_sonde_raw <- read_csv(
  "~/Documents/Kuru_Projects/HiRes_Gas_Sampling/South_Sparkling_06_12_2026_High_Res_Gas.csv"
)

#=======================================================
# Process Trout Bog sonde profile
#=======================================================

TB_sonde_profile <-
  
  TB_sonde_raw %>%
  
  filter(Turbidity < 50) %>%
  
  mutate(
    ODO.1 = pmax(ODO.1, 0),
    Depth = round(Depth / 0.05) * 0.05
  ) %>%
  
  group_by(Depth) %>%
  
  summarize(
    Turbidity = mean(Turbidity, na.rm = TRUE),
    DO = mean(ODO.1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  mutate(Lake = "Trout Bog")

#=======================================================
# Process South Sparkling Bog sonde profile
#=======================================================

SS_sonde_profile <-
  
  SS_sonde_raw %>%
  
  filter(Turbidity < 50) %>%
  
  mutate(
    ODO.1 = pmax(ODO.1, 0),
    Depth = round(Depth / 0.05) * 0.05
  ) %>%
  
  group_by(Depth) %>%
  
  summarize(
    Turbidity = mean(Turbidity, na.rm = TRUE),
    DO = mean(ODO.1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  mutate(Lake = "South Sparkling Bog")

#=======================================================
# Scale DO for plotting
#=======================================================

max_DO <- max(
  c(
    TB_sonde_profile$DO,
    SS_sonde_profile$DO
  ),
  na.rm = TRUE
)

TB_sonde_profile <-
  
  TB_sonde_profile %>%
  
  mutate(
    DO_scaled = DO / max_DO * 10
  )

SS_sonde_profile <-
  
  SS_sonde_profile %>%
  
  mutate(
    DO_scaled = DO / max_DO * 10
  )

#=======================================================
# Combine profiles
#=======================================================

sonde_profiles <-
  
  bind_rows(
    TB_sonde_profile,
    SS_sonde_profile
  )

#=======================================================
# Plot
#=======================================================

ggplot(sonde_profiles, aes(y = Depth)) +
  
  geom_path(
    aes(x = Turbidity),
    color = "darkgreen",
    linewidth = 1.4
  ) +
  
  geom_path(
    aes(x = DO_scaled),
    color = "steelblue",
    linewidth = 1.4
  ) +
  
  facet_wrap(~Lake, nrow = 1) +
  
  scale_y_reverse(
    limits = c(7.5, 0),
    breaks = seq(0, 7, 1),
    expand = c(0, 0)
  ) +
  
  scale_x_continuous(
    name = "Dissolved Oxygen (mg/L)",
    limits = c(0, 10),
    breaks = seq(0, 10, 2),
    
    sec.axis = sec_axis(
      ~ . / 10 * max_DO,
      name = "Turbidity (FNU)"
    )
  ) +
  
  labs(y = "Depth (m)") +
  
  theme_classic(base_size = 16) +
  
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 18),
    
    axis.title.y = element_text(face = "bold", size = 18),
    
    axis.title.x.bottom = element_text(
      face = "bold",
      color = "steelblue",
      size = 18
    ),
    
    axis.text.x.bottom = element_text(
      color = "steelblue",
      size = 14
    ),
    
    axis.title.x.top = element_text(
      face = "bold",
      color = "darkgreen",
      size = 18
    ),
    
    axis.text.x.top = element_text(
      color = "darkgreen",
      size = 14
    ),
    
    panel.spacing = unit(1.5, "lines")
  )
