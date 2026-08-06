library(tidyverse)
library(patchwork)

#====================================================
# Common axis limits
#====================================================

do_max <- max(
  TB_sonde_profile$DO,
  SS_sonde_profile$DO,
  na.rm = TRUE
)

turb_max <- max(
  TB_sonde_profile$Turbidity,
  SS_sonde_profile$Turbidity,
  na.rm = TRUE
)

#====================================================
# Clean methane plots
#====================================================

clean_CH4_theme <-
  
  theme(
    axis.title.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank()
  )

p_CH4_TB <- p_CH4_TB + clean_CH4_theme
p_CH4_SS <- p_CH4_SS + clean_CH4_theme

#====================================================
# Plotting function
#====================================================

plot_profile <- function(data,
                         variable,
                         xmax,
                         xlabel,
                         color,
                         depth_max){
  
  ggplot(
    data,
    aes(x = .data[[variable]], y = Depth)
  ) +
    
    geom_path(
      color = color,
      linewidth = 1.2
    ) +
    
    scale_x_continuous(
      limits = c(0, xmax),
      expand = c(0, 0)
    ) +
    
    scale_y_reverse(
      limits = c(depth_max, 0),
      breaks = seq(0.5, depth_max, 0.5),
      expand = c(0, 0)
    ) +
    
    labs(
      x = xlabel,
      y = "Depth (m)"
    ) +
    
    theme_bw(base_size = 16)
}

#====================================================
# Trout Bog
#====================================================

p_DO_TB <-
  
  plot_profile(
    TB_sonde_profile,
    "DO",
    do_max,
    "DO (mg/L)",
    "steelblue",
    6.6
  )

p_Turb_TB <-
  
  plot_profile(
    TB_sonde_profile,
    "Turbidity",
    turb_max,
    "Turbidity (FNU)",
    "darkgreen",
    6.6
  ) +
  
  clean_CH4_theme

TB_figure <-
  
  p_DO_TB +
  p_Turb_TB +
  p_CH4_TB +
  
  plot_layout(widths = c(1, 1, 1.4))

#====================================================
# South Sparkling Bog
#====================================================

p_DO_SS <-
  
  plot_profile(
    SS_sonde_profile,
    "DO",
    do_max,
    "DO (mg/L)",
    "steelblue",
    4.6
  )

p_Turb_SS <-
  
  plot_profile(
    SS_sonde_profile,
    "Turbidity",
    turb_max,
    "Turbidity (FNU)",
    "darkgreen",
    4.6
  ) +
  
  clean_CH4_theme

SS_figure <-
  
  p_DO_SS +
  p_Turb_SS +
  p_CH4_SS +
  
  plot_layout(widths = c(1, 1, 1.4))

#====================================================
# Final figure
#====================================================

TB_figure /
  SS_figure
