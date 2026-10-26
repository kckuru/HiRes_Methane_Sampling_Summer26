library(tidyverse)
library(lubridate)
library(patchwork)

gc_folder <- "/Users/keeleykuru/Documents/Kuru_Projects/HiRes_Gas_Sampling/HiRes_Methane_Sampling_Summer26"

gc_files <- list.files(
  gc_folder,
  pattern = "\\.txt$",
  full.names = TRUE
)

extract_gc_report <- function(file) {
  
  txt <- readLines(file, warn = FALSE)
  
  # Locate the beginning of each sample report
  header_lines <- grep("^\\[Header\\]", txt)
  
  results <- vector("list", length(header_lines))
  
  for (i in seq_along(header_lines)) {
    
    end_line <- if (i < length(header_lines)) {
      header_lines[i + 1] - 1
    } else {
      length(txt)
    }
    
    section <- txt[header_lines[i]:end_line]
    
    #--------------------------------------------------
    # Sample information
    #--------------------------------------------------
    
    sample_id <- str_match(
      section[str_detect(section, "^Sample ID")],
      "Sample ID\\s+(.*)"
    )[,2]
    
    sample_name <- str_match(
      section[str_detect(section, "^Sample Name")],
      "Sample Name\\s+(.*)"
    )[,2]
    
    acquisition_date <- str_match(
      section[str_detect(section, "^Acquisition Date")],
      "Acquisition Date\\s+(.*)"
    )[,2]
    
    #--------------------------------------------------
    # Function to extract one chromatographic peak
    #--------------------------------------------------
    
    extract_peak <- function(channel, compound) {
      
      peak_start <- grep(
        paste0("^\\[Peak Table \\(", channel, "\\)\\]"),
        section
      )
      
      if (length(peak_start) == 0)
        return(c(NA, NA))
      
      next_header <- grep("^\\[", section)
      next_header <- next_header[next_header > peak_start]
      
      peak_end <- if (length(next_header) > 0) {
        next_header[1] - 1
      } else {
        length(section)
      }
      
      peak_table <- section[peak_start:peak_end]
      
      peak_line <- peak_table[
        str_detect(
          peak_table,
          paste0("\\b", compound, "\\b")
        )
      ]
      
      if (length(peak_line) == 0)
        return(c(NA, NA))
      
      fields <- str_split(
        peak_line,
        "\t"
      )[[1]]
      
      area <- as.numeric(fields[5])
      height <- as.numeric(fields[6])
      
      c(area, height)
    }
    
    #--------------------------------------------------
    # Extract peak areas and heights
    #--------------------------------------------------
    
    ch4     <- extract_peak("Ch1", "CH4")
    co2_fid <- extract_peak("Ch1", "CO2")
    co2_tcd <- extract_peak("Ch2", "CO2")
    n2o     <- extract_peak("Ch3", "N2O")
    
    #--------------------------------------------------
    # Save results
    #--------------------------------------------------
    
    results[[i]] <- tibble(
      
      File = basename(file),
      
      SampleID = sample_id,
      SampleName = sample_name,
      AcquisitionDate = acquisition_date,
      
      CH4_Area = ch4[1],
      CH4_Height = ch4[2],
      
      CO2_FID_Area = co2_fid[1],
      CO2_FID_Height = co2_fid[2],
      
      CO2_TCD_Area = co2_tcd[1],
      CO2_TCD_Height = co2_tcd[2],
      
      N2O_Area = n2o[1],
      N2O_Height = n2o[2]
    )
  }
  
  bind_rows(results)
}

#==================================================
# Read all GC reports
#==================================================

gc_raw <-
  
  map_dfr(
    gc_files,
    extract_gc_report
  )


#==================================================
# Process GC data
#==================================================

gc_processed <-
  
  gc_raw %>%
  
  filter(SampleID != "blank") %>%
  
  separate(
    SampleID,
    into = c("Lake", "Site", "Rep"),
    sep = "_",
    remove = FALSE
  ) %>%
  
  mutate(
    AcquisitionDate = mdy_hms(AcquisitionDate)
  ) %>%
  
  arrange(AcquisitionDate) %>%
  
  group_by(SampleID) %>%
  
  slice_tail(n = 1) %>%
  
  ungroup()

gc_processed %>%
  count(SampleID) %>%
  filter(n > 1)


#==================================================
# Summarize methane profiles
#==================================================

gc_summary <-
  
  gc_processed %>%
  
  group_by(Lake, Site) %>%
  
  summarize(
    
    CH4_mean = mean(CH4_Area),
    CH4_sd   = sd(CH4_Area),
    CH4_se   = CH4_sd / sqrt(n()),
    
    CO2_mean = mean(CO2_TCD_Area),
    CO2_sd   = sd(CO2_TCD_Area),
    CO2_se   = CO2_sd / sqrt(n()),
    
    N2O_mean = mean(N2O_Area),
    N2O_sd   = sd(N2O_Area),
    N2O_se   = N2O_sd / sqrt(n()),
    
    .groups = "drop"
  ) %>%
  
  left_join(depth_key, by = "Site")


#==================================================
# Separate lake profiles
#==================================================

TB_CH4_profile <-
  
  gc_summary %>%
  filter(Lake == "TB")

SS_CH4_profile <-
  
  gc_summary %>%
  filter(Lake == "SS")

TB_CH4_profile <- TB_CH4_profile %>%
  arrange(Depth)

SS_CH4_profile <- SS_CH4_profile %>%
  arrange(Depth)


plot_CH4_profile <- function(data, title, color) {
  
  ggplot(data, aes(x = CH4_mean, y = Depth)) +
    
    geom_errorbar(
      aes(
        xmin = CH4_mean - CH4_se,
        xmax = CH4_mean + CH4_se
      ),
      orientation = "y",
      width = 0,
      linewidth = 0.6,
      color = color
    ) +
    
    geom_path(
      linewidth = 1.2,
      color = color
    ) +
    
    geom_point(
      size = 3,
      color = color
    ) +
    
    common_x +
    common_y +
    
    labs(
      title = title,
      x = expression("Mean CH"[4] * " Peak Area"),
      y = "Depth (m)"
    ) +
    
    theme_bw(base_size = 16)
}


p_CH4_TB <-
  
  plot_CH4_profile(
    TB_CH4_profile,
    "Trout Bog",
    "#2C7FB8"
  )

p_CH4_SS <-
  
  plot_CH4_profile(
    SS_CH4_profile,
    "South Sparkling Bog",
    "#D95F02"
  ) +
  
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )


p_CH4_TB | p_CH4_SS


#########################
# Concentration data !!!#
#########################

GHG <-
  
  GHG_raw %>%
  
  filter(
    str_detect(sample_id, "^(TB|SS)_"),
    !str_detect(sample_id, "SP26")
  )


GHG_processed <-
  
  GHG %>%
  
  separate(
    sample_id,
    into = c("Lake", "Site", "Rep"),
    sep = "_",
    remove = FALSE
  ) %>%
  
  mutate(
    
    Rep = as.integer(Rep),
    
    # Use the detector recommended by the lab
    CH4 = case_when(
      !is.na(CH4_FID) & CH4_FID < 5000 ~ CH4_FID,
      TRUE ~ CH4_TCD
    ),
    
    CO2 = case_when(
      !is.na(CO2_FID) & CO2_FID < 2000 ~ CO2_FID,
      TRUE ~ CO2_TCD
    )
    
  )

GHG_summary <-
  
  GHG_processed %>%
  
  group_by(Lake, Site) %>%
  
  summarize(
    
    CH4_mean = mean(CH4, na.rm = TRUE),
    CH4_sd   = sd(CH4, na.rm = TRUE),
    CH4_se   = CH4_sd / sqrt(sum(!is.na(CH4))),
    
    CO2_mean = mean(CO2, na.rm = TRUE),
    CO2_sd   = sd(CO2, na.rm = TRUE),
    CO2_se   = CO2_sd / sqrt(sum(!is.na(CO2))),
    
    N2O_mean = mean(N2O_ECD, na.rm = TRUE),
    N2O_sd   = sd(N2O_ECD, na.rm = TRUE),
    N2O_se   = N2O_sd / sqrt(sum(!is.na(N2O_ECD))),
    
    .groups = "drop"
    
  ) %>%
  
  left_join(depth_key, by = "Site")

head(GHG_summary)


# Separate the two lakes #
TB_CH4_profile <-
  
  GHG_summary %>%
  
  filter(Lake == "TB") %>%
  
  arrange(Depth)


SS_CH4_profile <-
  
  GHG_summary %>%
  
  filter(Lake == "SS") %>%
  
  arrange(Depth)

# Make the plotting function #
plot_CH4_profile <- function(data, title, color) {
  
  ggplot(data, aes(x = CH4_mean, y = Depth)) +
    
    geom_errorbar(
      aes(
        xmin = CH4_mean - CH4_se,
        xmax = CH4_mean + CH4_se
      ),
      width = 0,
      linewidth = 0.6,
      color = color
    ) +
    
    geom_path(
      linewidth = 1.2,
      color = color
    ) +
    
    geom_point(
      size = 2.5,
      color = color
    ) +
    
    scale_y_reverse(
      limits = c(6.5, 0.5),
      breaks = seq(0.5, 6.5, by = 0.5),
      expand = c(0, 0)
    ) +
    
    scale_x_log10(
      limits = c(5, 30000),
      breaks = c(10, 100, 1000, 10000),
      labels = scales::label_number(),
      expand = expansion(mult = c(0.02, 0.05))
    ) +
    
    labs(
      title = title,
      x = expression(CH[4]~"(ppmv)"),
      y = "Depth (m)"
    ) +
    
    theme_bw(base_size = 16) +
    
    theme(
      
      plot.title = element_text(
        hjust = 0.5,
        face = "bold"
      ),
      
      axis.title = element_text(size = 18),
      
      axis.text = element_text(size = 14),
      
      panel.grid.minor = element_blank(),
      
      aspect.ratio = 1
      
    )
}

# Make the two plots #
p_June_CH4_TB <-
  
  plot_CH4_profile(
    TB_CH4_profile,
    "Trout Bog",
    "#2C7FB8"
  )

p_June_CH4_SS <-
  
  plot_CH4_profile(
    SS_CH4_profile,
    "South Sparkling Bog",
    "#D95F02"
  ) +
  
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

# Combine plots #
(p_June_CH4_TB | p_June_CH4_SS) +
  plot_layout(widths = c(1, 1))


##########################
# Non-log-scale version #
#########################
plot_CH4_profile <- function(data, title, color, log_scale = TRUE) {
  
  p <-
    
    ggplot(data, aes(x = CH4_mean, y = Depth)) +
    
    geom_errorbar(
      aes(
        xmin = CH4_mean - CH4_se,
        xmax = CH4_mean + CH4_se
      ),
      width = 0,
      linewidth = 0.6,
      color = color
    ) +
    
    geom_path(
      linewidth = 1.2,
      color = color
    ) +
    
    geom_point(
      size = 2.5,
      color = color
    ) +
    
    scale_y_reverse(
      limits = c(6.5, 0.5),
      breaks = seq(0.5, 6.5, by = 0.5),
      expand = c(0, 0)
    ) +
    
    labs(
      title = title,
      x = expression(CH[4]~"(ppmv)"),
      y = "Depth (m)"
    ) +
    
    theme_bw(base_size = 16) +
    
    theme(
      
      plot.title = element_text(
        hjust = 0.5,
        face = "bold"
      ),
      
      axis.title = element_text(size = 18),
      
      axis.text = element_text(size = 14),
      
      panel.grid.minor = element_blank(),
      
      aspect.ratio = 1
      
    )
  
  if (log_scale) {
    
    p <- p +
      
      scale_x_log10(
        limits = c(5, 30000),
        breaks = c(10, 100, 1000, 10000),
        labels = scales::label_number(),
        expand = expansion(mult = c(0.02, 0.05))
      )
    
  } else {
    
    p <- p +
      
      scale_x_continuous(
        limits = c(0, 20000),
        breaks = seq(0, 20000, by = 5000),
        expand = expansion(mult = c(0, 0.02))
      )
    
  }
  
  p
}

p_June_CH4_TB_linear <-
  
  plot_CH4_profile(
    TB_CH4_profile,
    "Trout Bog",
    "#2C7FB8",
    log_scale = FALSE
  )

p_June_CH4_SS_linear <-
  
  plot_CH4_profile(
    SS_CH4_profile,
    "South Sparkling Bog",
    "#D95F02",
    log_scale = FALSE
  ) +
  
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

(p_June_CH4_TB_linear | p_June_CH4_SS_linear) +
  plot_layout(widths = c(1, 1))

#############################################
## Plot TB June data (log style) with ppmv ##
##############################################

GHG_raw <- read_csv(
  "/Users/keeleykuru/Documents/Kuru_Projects/HiRes_Gas_Sampling/HiRes_Methane_Sampling_Summer26/20260806_GHG.csv"
)

names(GHG_raw)

GHG <- GHG_raw %>%
  filter(
    str_detect(sample_id, "^(TB|SS)_"),
    !str_detect(sample_id, "SP26")
  )

GHG_processed <- GHG %>%
  separate(
    sample_id,
    into = c("Lake", "Site", "Rep"),
    sep = "_",
    remove = FALSE
  ) %>%
  mutate(
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

GHG_summary <- GHG_processed %>%
  group_by(Lake, Site) %>%
  summarize(
    n = sum(!is.na(CH4)),
    CH4_mean = mean(CH4, na.rm = TRUE),
    CH4_sd = sd(CH4, na.rm = TRUE),
    CH4_se = CH4_sd / sqrt(n),
    .groups = "drop"
  ) %>%
  left_join(depth_key, by = "Site")

TB_CH4_profile <- GHG_summary %>%
  filter(Lake == "TB") %>%
  arrange(Depth)

TB_CH4_profile


ggplot(TB_CH4_profile, aes(x = CH4_mean, y = Depth)) +
  
  geom_errorbar(
    aes(
      xmin = CH4_mean - CH4_se,
      xmax = CH4_mean + CH4_se
    ),
    orientation = "y",
    width = 0,
    linewidth = 0.6,
    color = "#2C7FB8"
  ) +
  
  geom_path(
    linewidth = 1.2,
    color = "#2C7FB8"
  ) +
  
  geom_point(
    size = 2.5,
    color = "#2C7FB8"
  ) +
  
  scale_y_reverse(
    limits = c(6.5, 0.5),
    breaks = seq(0.5, 6.5, 0.5),
    expand = c(0, 0)
  ) +
  
  scale_x_log10(
    limits = c(5, 30000),
    breaks = c(10, 100, 1000, 10000),
    labels = scales::label_number(),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  
  labs(
    x = expression(CH[4] ~ "(ppmv)"),
    y = "Depth (m)"
  ) +
  
  theme_bw(base_size = 16) +
  
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    panel.grid.minor = element_blank(),
    aspect.ratio = 1
  )

#############################################
## Plot TB June data (log style) with ppmv ##
#############################################

p_June2026_CH4_TB <-
  
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
  
  geom_errorbar(
    aes(
      xmin = CH4_mean - CH4_se,
      xmax = CH4_mean + CH4_se
    ),
    orientation = "y",
    width = 0,
    linewidth = 0.6,
    color = "#2C7FB8"
  ) +
  
  geom_path(
    linewidth = 1.2,
    color = "#2C7FB8"
  ) +
  
  geom_point(
    size = 2.5,
    color = "#2C7FB8"
  ) +
  
  scale_y_reverse(
    limits = c(6.5, 0.5),
    breaks = seq(0.5, 6.5, by = 0.5),
    expand = c(0, 0)
  ) +
  
  scale_x_log10(
    limits = c(5, 30000),
    breaks = c(10, 100, 1000, 10000),
    labels = scales::label_number(),
    expand = c(0, 0)
  ) +
  
  labs(
    x = expression(CH[4] ~ "(ppmv)"),
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

p_June2026_CH4_TB
