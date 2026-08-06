library(tidyverse)
library(patchwork)

profile_theme <-
  
  theme_classic(base_size = 16) +
  
  theme(
    
    panel.border = element_blank(),
    
    panel.grid = element_blank(),
    
    axis.line.y = element_line(),
    
    plot.margin = margin(5,5,5,5)
    
  )

temp_max <- max(
  TB_sonde$Temp,
  SS_sonde$Temp,
  na.rm = TRUE
)

do_max <- max(
  TB_sonde$DO,
  SS_sonde$DO,
  na.rm = TRUE
)

turb_max <- max(
  TB_sonde$Turbidity,
  SS_sonde$Turbidity,
  na.rm = TRUE
)

ch4_max <- max(
  TB_plot$CH4_mean,
  SS_plot$CH4_mean,
  na.rm = TRUE
)

ch4_min <- min(
  TB_plot$CH4_mean,
  SS_plot$CH4_mean,
  na.rm = TRUE
)

p_Temp_SS <-
  
  ggplot(SS_sonde,
         aes(Temp,Depth)) +
  
  geom_path(
    color="firebrick",
    linewidth=1.2
  ) +
  
  geom_point(
    color="firebrick",
    size=2
  ) +
  
  scale_x_continuous(
    
    limits=c(0,temp_max),
    
    position="top",
    
    expand=c(0,0)
    
  ) +
  
  scale_y_reverse(
    
    limits=c(4.6,0),
    
    breaks=seq(0.5,4.5,.5),
    
    expand=c(0,0)
    
  ) +
  
  labs(
    
    x="Temperature (°C)",
    
    y="Depth (m)"
    
  ) +
  
  profile_theme +
  
  theme(
    
    axis.line.x.top=element_line(color="firebrick"),
    
    axis.text.x.top=element_text(color="firebrick"),
    
    axis.title.x.top=element_text(color="firebrick")
    
  )

p_DO_SS <-
  
  ggplot(SS_sonde,
         aes(DO,Depth)) +
  
  geom_path(
    color="steelblue",
    linewidth=1.2
  )+
  
  geom_point(
    color="steelblue",
    size=2
  )+
  
  scale_x_continuous(
    
    limits=c(0,do_max),
    
    position="top",
    
    expand=c(0,0)
    
  )+
  
  scale_y_reverse(
    
    limits=c(4.6,0),
    
    breaks=seq(.5,4.5,.5),
    
    expand=c(0,0)
    
  )+
  
  labs(
    
    x="DO (mg/L)"
    
  )+
  
  profile_theme +
  
  theme(
    
    axis.title.y=element_blank(),
    
    axis.text.y=element_blank(),
    
    axis.ticks.y=element_blank(),
    
    axis.line.x.top=element_line(color="steelblue"),
    
    axis.text.x.top=element_text(color="steelblue"),
    
    axis.title.x.top=element_text(color="steelblue")
    
  )

p_Turb_SS <-
  
  ggplot(SS_sonde,
         aes(Turbidity,Depth)) +
  
  geom_path(
    color="darkgreen",
    linewidth=1.2
  )+
  
  geom_point(
    color="darkgreen",
    size=2
  )+
  
  scale_x_continuous(
    
    limits=c(0,turb_max),
    
    position="top",
    
    expand=c(0,0)
    
  )+
  
  scale_y_reverse(
    
    limits=c(4.6,0),
    
    breaks=seq(.5,4.5,.5),
    
    expand=c(0,0)
    
  )+
  
  labs(
    
    x="Turbidity (FNU)"
    
  )+
  
  profile_theme +
  
  theme(
    
    axis.title.y=element_blank(),
    
    axis.text.y=element_blank(),
    
    axis.ticks.y=element_blank(),
    
    axis.line.x.top=element_line(color="darkgreen"),
    
    axis.text.x.top=element_text(color="darkgreen"),
    
    axis.title.x.top=element_text(color="darkgreen")
    
  )

p_CH4_SS <-
  
  ggplot(
    SS_plot,
    aes(CH4_mean,Depth)
    
  )+
  
  geom_errorbar(
    
    aes(
      
      xmin=CH4_mean-CH4_se,
      
      xmax=CH4_mean+CH4_se
      
    ),
    
    orientation="y",
    
    width=0
    
  )+
  
  geom_path(
    color="purple4",
    linewidth=1.2
  )+
  
  geom_point(
    color="purple4",
    size=2
  )+
  
  scale_x_log10(
    
    limits=c(ch4_min,ch4_max),
    
    position="top"
    
  )+
  
  scale_y_reverse(
    
    limits=c(4.6,0),
    
    breaks=seq(.5,4.5,.5),
    
    expand=c(0,0)
    
  )+
  
  labs(
    
    x="Mean CH₄ Peak Area"
    
  )+
  
  profile_theme +
  
  theme(
    
    axis.title.y=element_blank(),
    
    axis.text.y=element_blank(),
    
    axis.ticks.y=element_blank(),
    
    axis.line.x.top=element_line(color="purple4"),
    
    axis.text.x.top=element_text(color="purple4"),
    
    axis.title.x.top=element_text(color="purple4")
    
  )

SS_figure <-
  
  p_Temp_SS +
  p_DO_SS +
  p_Turb_SS +
  p_CH4_SS +
  
  plot_layout(
    
    widths=c(1,1,1,1.4)
    
  )

SS_figure
