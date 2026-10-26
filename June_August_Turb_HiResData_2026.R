library(tidyverse)
library(readxl)
library(patchwork)

# -----------------------------
# Read June and August 2026
# -----------------------------

TB_June2026_sonde <- read_excel(
  "/Users/keeleykuru/Documents/Kuru_Projects/HiRes_Gas_Sampling/HiRes_Methane_Sampling_Summer26/Trout_Bog_06_11_2026_High_Res_Gas.xlsx"
)

TB_Aug2026_sonde <- read_excel(
  "/Users/keeleykuru/Documents/Kuru_Projects/HiRes_Gas_Sampling/HiRes_Methane_Sampling_Summer26/Trout_Bog_08_11_2026_High_Res_Gas.xlsx"
)


# -----------------------------
# June 2026
# -----------------------------

p_June2026_Turbidity <-
  ggplot(
    TB_June2026_sonde,
    aes(x = Turbidity, y = Depth)
  ) +
  geom_path(
    linewidth = 1.1,
    color = "#2C7FB8"
  ) +
  geom_point(
    size = 1.5,
    color = "#2C7FB8"
  ) +
  scale_y_reverse(
    limits = c(7.5, 0),
    breaks = seq(0, 7.5, 0.5),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    limits = c(0, 4),
    breaks = seq(0, 4, 0.5),
    expand = c(0, 0)
  ) +
  labs(
    x = "Turbidity (FNU)",
    y = "Depth (m)",
    title = "June 2026"
  ) +
  theme_bw(base_size = 16) +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    panel.grid.minor = element_blank(),
    aspect.ratio = 1
  )


# -----------------------------
# August 2026
# -----------------------------

p_Aug2026_Turbidity <-
  ggplot(
    TB_Aug2026_sonde,
    aes(x = Turbidity, y = Depth)
  ) +
  geom_path(
    linewidth = 1.1,
    color = "black"
  ) +
  geom_point(
    size = 1.5,
    color = "black"
  ) +
  scale_y_reverse(
    limits = c(7.5, 0),
    breaks = seq(0, 7.5, 0.5),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    limits = c(0, 2.5),
    breaks = seq(0, 2.5, 0.5),
    expand = c(0, 0)
  ) +
  labs(
    x = "Turbidity (FNU)",
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


# -----------------------------
# Put them side-by-side
# -----------------------------

p_June2026_Turbidity +
  p_Aug2026_Turbidity +
  plot_layout(ncol = 2)


