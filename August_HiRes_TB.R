#############################################
## Trout Bog August 2026 CH4 Profile
#############################################

GHG_raw_Aug2026 <- read_csv(
  "/Users/keeleykuru/Documents/Kuru_Projects/HiRes_Gas_Sampling/HiRes_Methane_Sampling_Summer26/20261022_GHG.csv"
)

names(GHG_raw_Aug2026)

GHG_Aug2026 <- GHG_raw_Aug2026 %>%
  filter(
    str_detect(
      sample_id,
      regex("^TB_Aug_", ignore_case = TRUE)
    )
  )

nrow(GHG_Aug2026)

GHG_Aug2026 %>%
  count(sample_id) %>%
  print(n = Inf)

GHG_processed_Aug2026 <- GHG_Aug2026 %>%
  separate(
    sample_id,
    into = c("Lake", "Month", "Site", "Rep"),
    sep = "_",
    remove = FALSE
  ) %>%
  mutate(
    Lake = str_to_upper(Lake),
    Rep = as.integer(Rep),
    
    CH4 = case_when(
      !is.na(CH4_FID) & CH4_FID < 5000 ~ CH4_FID,
      TRUE ~ CH4_TCD
    ),
    
    CO2 = case_when(
      !is.na(CO2_FID) & CO2_FID < 2000 ~ CO2_FID,
      TRUE ~ CO2_TCD
    )
  )

GHG_summary_Aug2026 <- GHG_processed_Aug2026 %>%
  group_by(Lake, Site) %>%
  summarize(
    n = sum(!is.na(CH4)),
    CH4_mean = mean(CH4, na.rm = TRUE),
    CH4_sd = sd(CH4, na.rm = TRUE),
    CH4_se = CH4_sd / sqrt(n),
    .groups = "drop"
  ) %>%
  left_join(depth_key, by = "Site")

TB_CH4_profile_Aug2026 <- GHG_summary_Aug2026 %>%
  filter(Lake == "TB") %>%
  arrange(Depth)

TB_CH4_profile_Aug2026


TB_CH4_profile_Aug2026 %>%
  print(n = Inf)


p_Aug2026_CH4_TB <-
  ggplot(TB_CH4_profile_Aug2026, aes(x = CH4_mean, y = Depth)) +
  
  annotate(
    "rect",
    xmin = 0.1,
    xmax = 40000,
    ymin = 1.25,
    ymax = 2.0,
    fill = "grey97",
    color = NA
  ) +
  
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
  
  geom_path(
    linewidth = 1.2,
    color = "black"
  ) +
  
  geom_point(
    size = 2.5,
    color = "black"
  ) +
  
  scale_y_reverse(
    limits = c(6.5, 0.5),
    breaks = seq(0.5, 6.5, 0.5),
    expand = c(0, 0)
  ) +
  
  scale_x_log10(
    limits = c(0.1, 40000),
    breaks = c(0.1, 1, 10, 100, 1000, 10000),
    labels = scales::label_number(),
    expand = c(0, 0)
  ) +
  
  labs(
    x = expression(CH[4] ~ "(ppmv)"),
    y = "Depth (m)",
    title = "August 2026"
  ) +
  
  theme_bw(base_size = 16) +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    panel.grid.minor = element_blank(),
    aspect.ratio = 1
  )

p_Aug2026_CH4_TB


#########################
# Verifying data points #
#########################

GHG_processed_Aug2026 %>%
  filter(Site == "E") %>%
  select(sample_id, CH4_FID, CH4_TCD, CH4) %>%
  arrange(sample_id)
GHG_processed_Aug2026 %>%
  filter(Site %in% c("D", "E", "F")) %>%
  select(sample_id, Site, CH4_FID, CH4_TCD, CH4) %>%
  arrange(Site, sample_id)




p_June2026_CH4_TB + p_Aug2026_CH4_TB +
  plot_layout(ncol = 2)



