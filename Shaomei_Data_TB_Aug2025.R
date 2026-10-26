############################################
## Trout Bog High-Resolution CH4 Profile
## August 2025
## Data collected by Shaomei
############################################

library(tidyverse)
library(lubridate)
library(patchwork)


#==================================================
# File path
#==================================================

data_file <- "~/Documents/Kuru_Projects/DEEP-Bogs/Shaomei_DEEPBogs_Methane_Aug2025.csv"


#==================================================
# Read August 2025 data
#==================================================

CH4_raw <- read_csv(
  data_file,
  show_col_types = FALSE
)


#==================================================
# Inspect data
#==================================================

head(CH4_raw)
glimpse(CH4_raw)

CH4_raw %>%
  count(Lake)

CH4_raw %>%
  filter(Lake == "TB") %>%
  count(`Depth (m)`) %>%
  arrange(`Depth (m)`)


#==================================================
# Select Trout Bog
#==================================================

TB_CH4_raw <-
  CH4_raw %>%
  filter(Lake == "TB")


#==================================================
# Summarize CH4 by depth
#==================================================

TB_CH4_profile <-
  TB_CH4_raw %>%
  group_by(`Depth (m)`) %>%
  summarize(
    
    n = sum(!is.na(`CH4 conc. (ppm)`)),
    
    CH4_mean = mean(
      `CH4 conc. (ppm)`,
      na.rm = TRUE
    ),
    
    CH4_sd = sd(
      `CH4 conc. (ppm)`,
      na.rm = TRUE
    ),
    
    CH4_se = CH4_sd / sqrt(n),
    
    .groups = "drop"
    
  ) %>%
  rename(
    Depth = `Depth (m)`
  ) %>%
  arrange(Depth)


#==================================================
# View summary
#==================================================

TB_CH4_profile


#==================================================
# Plot CH4 profile
#==================================================

plot_CH4_profile <- function(
    data,
    log_scale = TRUE
) {
  
  p <-
    ggplot(
      data,
      aes(
        x = CH4_mean,
        y = Depth
      )
    ) +
    
    # Turbidity peak: 1.25–2.0 m
    annotate(
      "rect",
      xmin = 5,
      xmax = 30000,
      ymin = 1.25,
      ymax = 2.0,
      fill = "grey97",
      color = NA
    ) +
    
    # SE error bars
    geom_errorbar(
      aes(
        xmin = CH4_mean - CH4_se,
        xmax = CH4_mean + CH4_se
      ),
      orientation = "y",
      width = 0,
      linewidth = 0.6
    ) +
    
    # Connect depths
    geom_path(
      linewidth = 1.2
    ) +
    
    # Data points
    geom_point(
      size = 2.5
    ) +
    
    # Depth increases downward
    scale_y_reverse(
      limits = c(7.5, 0.5),
      breaks = seq(0.5, 7.5, by = 0.5),
      expand = c(0, 0)
    ) +
    
    labs(
      x = expression(CH[4] ~ "(ppmv)"),
      y = "Depth (m)"
    ) +
    
    theme_bw(base_size = 16) +
    
    theme(
      axis.title = element_text(
        size = 18
      ),
      
      axis.text = element_text(
        size = 14
      ),
      
      panel.grid.minor = element_blank(),
      
      aspect.ratio = 1
    )
  
  
  if (log_scale) {
    
    p <- p +
      scale_x_log10(
        limits = c(5, 30000),
        breaks = c(
          10,
          100,
          1000,
          10000
        ),
        labels = scales::label_number(),
        
        # No expansion so gray band reaches both edges
        expand = c(0, 0)
      )
    
  } else {
    
    p <- p +
      scale_x_continuous(
        limits = c(0, 20000),
        breaks = seq(
          0,
          20000,
          by = 5000
        ),
        expand = expansion(
          mult = c(0, 0.02)
        )
      )
  }
  
  p
}

p_Aug2025_CH4_TB <-
  plot_CH4_profile(
    TB_CH4_profile,
    log_scale = TRUE
  )

p_Aug2025_CH4_TB



p_Aug2025_CH4_TB <-
  ggplot(TB_CH4_profile, aes(x = CH4_mean, y = Depth)) +
  
  # Turbidity peak: 1.25–2.0 m
  annotate(
    "rect",
    xmin = 5,
    xmax = 30000,
    ymin = 1.25,
    ymax = 2.0,
    fill = "grey97",
    color = NA
  ) +
  
  # SE bars
  geom_errorbar(
    aes(
      xmin = CH4_mean - CH4_se,
      xmax = CH4_mean + CH4_se
    ),
    orientation = "y",
    width = 0,
    linewidth = 0.6,
    color = "black"
  ) +
  
  # Connecting line
  geom_path(
    linewidth = 1.2,
    color = "black"
  ) +
  
  # Data points
  geom_point(
    size = 2.5,
    color = "black"
  ) +
  
  # Depth axis
  scale_y_reverse(
    limits = c(7.5, 0.5),
    breaks = seq(0.5, 7.5, by = 0.5),
    expand = c(0, 0)
  ) +
  
  # Log CH4 axis
  scale_x_log10(
    limits = c(5, 30000),
    breaks = c(10, 100, 1000, 10000),
    labels = scales::label_number(),
    expand = c(0, 0)
  ) +
  
  labs(
    x = expression(CH[4] ~ "(ppmv)"),
    y = "Depth (m)",
    title = "August 2025"
  ) +
  
  theme_bw(base_size = 16) +
  
  theme(
    axis.title.x = element_text(
      size = 17,
      margin = margin(t = 10)
    ),
    axis.title.y = element_text(
      size = 17,
      margin = margin(r = 10)
    ),
    axis.text = element_text(size = 13),
    
    panel.grid.major.x = element_line(
      color = "grey90",
      linewidth = 0.4
    ),
    panel.grid.major.y = element_line(
      color = "grey90",
      linewidth = 0.4
    ),
    panel.grid.minor = element_blank(),
    
    axis.line = element_line(
      color = "black",
      linewidth = 0.5
    ),
    
    plot.title = element_text(
      size = 17,
      face = "plain",
      hjust = 0.5
    ),
    
    plot.margin = margin(10, 15, 10, 10)
  )



###########################################################
## Plotting June 2026 and August 2025 next to each other ##
###########################################################

library(patchwork)

p_June2026_CH4_TB +
  p_Aug2025_CH4_TB +
  plot_layout(ncol = 2)


