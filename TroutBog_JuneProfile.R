library(readr)
library(dplyr)

TB_gas_data <- read_csv(
  "~/Documents/Kuru_Projects/HiRes_Gas_Sampling/Trout_Bog_06_11_2026_High_Res_Gas.csv"
)

glimpse(TB_gas_data)

SS_gas_data <- read.csv(
  "~/Documents/Kuru_Projects/HiRes_Gas_Sampling/South_Sparkling_06_12_2026_High_Res_Gas.csv"
)

glimpse(SS_gas_data)

library(dplyr)

TB_plot <- TB_gas_data %>%
  select(Depth, Turbidity) %>%
  mutate(Lake = "Trout Bog")

SS_plot <- SS_gas_data %>%
  select(Depth, Turbidity) %>%
  mutate(Lake = "South Sparkling Bog")

gas_profiles <- bind_rows(TB_plot, SS_plot)

ggplot(gas_profiles,
       aes(x = Turbidity, y = Depth)) +
  geom_path(linewidth = 1, color = "darkgreen") +
  scale_y_reverse() +
  scale_x_continuous(
    position = "top",
    breaks = seq(0, 20, 2)
  ) +
  coord_cartesian(xlim = c(0, 20)) +
  facet_wrap(~Lake, nrow = 1) +
  labs(
    x = "Turbidity (FNU)",
    y = "Depth (m)"
  ) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11)
  )

