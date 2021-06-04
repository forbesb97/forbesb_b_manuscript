# ==============================================================================
#
# TLS, uas, ALS, ZEB ladder fuel calculation using density 
#
# ==============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 6 Dec 2020
# Last commit: 6 Dec 2020
#
# Status: Development
#
# ==============================================================================
#
# Description:
#
# Computes ladder fuel metrics  TLS, HMLS (zeb), ALS, UAS
# 1-2m, 2-3m, 3-4m, 4-5m, 5-6m, 6-7m, 7-8m
#
# ==============================================================================
#
# User inputs:
#
# tls_las_folder = Folder location for TLS .las files
# tls_las_files = list.files function to get tls files to be processed. Can be modified
#   with a pattern to restrict search
# out_file = output .csv file name
#
# ==============================================================================
#
# Package dependences:
#
# sp, raster, lidR, tidyverse, glue
#
# ==============================================================================
#
# Known problems:
#
# ==============================================================================

library(lidR)
library(tidyverse)
library(glue)

# ================================= User inputs ================================

#tls_las_folder <- 'data/las'
tls_las_folder <- 'D:/c1 - Pepperwood/c1_DEMnorm_las_plot'
tls_las_files <- list.files(tls_las_folder, pattern = '.las', full.names = TRUE)

als_las_folder <- 'D:/c1 - Pepperwood/c1_ALS_normplot'
als_las_files <- list.files(als_las_folder, pattern = '.las', full.names = TRUE)

uas_las_folder <- 'D:/c1 - Pepperwood/c1_uas'
uas_las_files <- list.files(uas_las_folder, pattern = '.las', full.names = TRUE)

zeb_las_folder <- 'D:/c1 - Pepperwood/c1_zeb_cut2plot'
zeb_las_files <- list.files(zeb_las_folder, pattern = '.las', full.names = TRUE)

banner_data <- read_csv('D:/Analyses/ladder_fuel_jmp/ladder_fuels_airtable_210426.csv')


# out_file <- 'data/voxel_ladder-fuels.csv'
out_file <- 'D:/Analyses/ladder_fuel_jmp/c1_ladder-fuels_metrics_tls_zeb_als_uas_notfiltered_210426.csv'
out_file_filtered <- 'D:/Analyses/ladder_fuel_jmp/c1_ladder-fuels_metrics_tls_zeb_als_uas_filtered_210426.csv'

# ============ Compute density based ladder fuels metrics for TLS data ===========


tls_combine <- tibble(
  campaign = numeric(),
  plot = numeric(),
  tls_ladder_fuel_1to2 = numeric(),
  tls_ladder_fuel_2to3 = numeric(),
  tls_ladder_fuel_3to4 = numeric(),
  tls_ladder_fuel_4to5 = numeric(),
  tls_ladder_fuel_5to6 = numeric(),
  tls_ladder_fuel_6to7 = numeric(),
  tls_ladder_fuel_7to8 = numeric()
)

tls_combine_filter <- tibble(
  campaign = numeric(),
  plot = numeric(),
  tls_ladder_fuel_1to2 = numeric(),
  tls_ladder_fuel_2to3 = numeric(),
  tls_ladder_fuel_3to4 = numeric(),
  tls_ladder_fuel_4to5 = numeric(),
  tls_ladder_fuel_5to6 = numeric(),
  tls_ladder_fuel_6to7 = numeric(),
  tls_ladder_fuel_7to8 = numeric()
)


for (tls_file in tls_las_files) {
  
  campaign <- str_extract(tls_file, '(?<=c)[:digit:]') %>%
    as.numeric()
  plot <- str_extract(tls_file, '(?<=p)[:digit:]+') %>%
    as.numeric()
  
  message('processing TLS campaign ', campaign, ' plot ', plot)
  
  tls_las <- tls_file %>%
    readLAS(select = '')

  tls_metric <- as_tibble(tls_las$Z) %>% 
    rename(Z='value') %>%
    summarize(
      tls_ladder_fuel_1to2 = (sum(tls_las$Z > 1 & tls_las$Z <= 2) / sum(tls_las$Z <= 2)),
      tls_ladder_fuel_2to3 = (sum(tls_las$Z > 2 & tls_las$Z <= 3) / sum(tls_las$Z <= 3)),
      tls_ladder_fuel_3to4 = (sum(tls_las$Z > 3 & tls_las$Z <= 4) / sum(tls_las$Z <= 4)),
      tls_ladder_fuel_4to5 = (sum(tls_las$Z > 4 & tls_las$Z <= 5) / sum(tls_las$Z <= 5)),
      tls_ladder_fuel_5to6 = (sum(tls_las$Z > 5 & tls_las$Z <= 6) / sum(tls_las$Z <= 6)),
      tls_ladder_fuel_6to7 = (sum(tls_las$Z > 6 & tls_las$Z <= 7) / sum(tls_las$Z <= 7)),
      tls_ladder_fuel_7to8 = (sum(tls_las$Z > 7 & tls_las$Z <= 8) / sum(tls_las$Z <= 8))
    ) %>%
    add_column(campaign, plot, .before = 1)
  
  tls_combine <- tls_combine %>%
    add_row(tls_metric)
  
  tls_metric_filter <- as_tibble(tls_las$Z) %>% 
    rename(Z='value') %>%
    summarize(
      tls_ladder_fuel_1to2 = (sum(tls_las$Z > 1 & tls_las$Z <= 2) / sum(tls_las$Z >= 0 & tls_las$Z <= 2)),
      tls_ladder_fuel_2to3 = (sum(tls_las$Z > 2 & tls_las$Z <= 3) / sum(tls_las$Z >= 0 & tls_las$Z <= 3)),
      tls_ladder_fuel_3to4 = (sum(tls_las$Z > 3 & tls_las$Z <= 4) / sum(tls_las$Z >= 0 & tls_las$Z <= 4)),
      tls_ladder_fuel_4to5 = (sum(tls_las$Z > 4 & tls_las$Z <= 5) / sum(tls_las$Z >= 0 & tls_las$Z <= 5)),
      tls_ladder_fuel_5to6 = (sum(tls_las$Z > 5 & tls_las$Z <= 6) / sum(tls_las$Z >= 0 & tls_las$Z <= 6)),
      tls_ladder_fuel_6to7 = (sum(tls_las$Z > 6 & tls_las$Z <= 7) / sum(tls_las$Z >= 0 & tls_las$Z <= 7)),
      tls_ladder_fuel_7to8 = (sum(tls_las$Z > 7 & tls_las$Z <= 8) / sum(tls_las$Z >= 0 & tls_las$Z <= 8))
    ) %>%
    add_column(campaign, plot, .before = 1)
  
  tls_combine_filter <- tls_combine_filter %>%
    add_row(tls_metric_filter)
  
  rm(tls_las)
  
}

# ============ Compute density based ladder fuels metrics for ALS data ===========


als_combine <- tibble(
  campaign = numeric(),
  plot = numeric(),
  als_ladder_fuel_1to2 = numeric(),
  als_ladder_fuel_2to3 = numeric(),
  als_ladder_fuel_3to4 = numeric(),
  als_ladder_fuel_4to5 = numeric(),
  als_ladder_fuel_5to6 = numeric(),
  als_ladder_fuel_6to7 = numeric(),
  als_ladder_fuel_7to8 = numeric()
)

als_combine_filter <- tibble(
  campaign = numeric(),
  plot = numeric(),
  als_ladder_fuel_1to2 = numeric(),
  als_ladder_fuel_2to3 = numeric(),
  als_ladder_fuel_3to4 = numeric(),
  als_ladder_fuel_4to5 = numeric(),
  als_ladder_fuel_5to6 = numeric(),
  als_ladder_fuel_6to7 = numeric(),
  als_ladder_fuel_7to8 = numeric()
)

for (als_file in als_las_files) {
  
  campaign <- str_extract(als_file, '(?<=c)[:digit:]') %>%
    as.numeric()
  plot <- str_extract(als_file, '(?<=p)[:digit:]+') %>%
    as.numeric()
  
  message('processing als campaign ', campaign, ' plot ', plot)
  
  als_las <- als_file %>%
    readLAS(select = '')
  
  als_metric <- as_tibble(als_las$Z) %>% 
    rename(Z='value') %>%
    summarize(
      als_ladder_fuel_1to2 = (sum(als_las$Z > 1 & als_las$Z <= 2) / sum(als_las$Z <= 2)),
      als_ladder_fuel_2to3 = (sum(als_las$Z > 2 & als_las$Z <= 3) / sum(als_las$Z <= 3)),
      als_ladder_fuel_3to4 = (sum(als_las$Z > 3 & als_las$Z <= 4) / sum(als_las$Z <= 4)),
      als_ladder_fuel_4to5 = (sum(als_las$Z > 4 & als_las$Z <= 5) / sum(als_las$Z <= 5)),
      als_ladder_fuel_5to6 = (sum(als_las$Z > 5 & als_las$Z <= 6) / sum(als_las$Z <= 6)),
      als_ladder_fuel_6to7 = (sum(als_las$Z > 6 & als_las$Z <= 7) / sum(als_las$Z <= 7)),
      als_ladder_fuel_7to8 = (sum(als_las$Z > 7 & als_las$Z <= 8) / sum(als_las$Z <= 8))
    ) %>%
    add_column(campaign, plot, .before = 1)
  
  als_combine <- als_combine %>%
    add_row(als_metric)
  
  als_metric_filter <- as_tibble(als_las$Z) %>% 
    rename(Z='value') %>%
    summarize(
      als_ladder_fuel_1to2 = (sum(als_las$Z > 1 & als_las$Z <= 2) / sum(als_las$Z >= 0 & als_las$Z <= 2)),
      als_ladder_fuel_2to3 = (sum(als_las$Z > 2 & als_las$Z <= 3) / sum(als_las$Z >= 0 & als_las$Z <= 3)),
      als_ladder_fuel_3to4 = (sum(als_las$Z > 3 & als_las$Z <= 4) / sum(als_las$Z >= 0 & als_las$Z <= 4)),
      als_ladder_fuel_4to5 = (sum(als_las$Z > 4 & als_las$Z <= 5) / sum(als_las$Z >= 0 & als_las$Z <= 5)),
      als_ladder_fuel_5to6 = (sum(als_las$Z > 5 & als_las$Z <= 6) / sum(als_las$Z >= 0 & als_las$Z <= 6)),
      als_ladder_fuel_6to7 = (sum(als_las$Z > 6 & als_las$Z <= 7) / sum(als_las$Z >= 0 & als_las$Z <= 7)),
      als_ladder_fuel_7to8 = (sum(als_las$Z > 7 & als_las$Z <= 8) / sum(als_las$Z >= 0 & als_las$Z <= 8))
    ) %>%
    add_column(campaign, plot, .before = 1)
  
  als_combine_filter <- als_combine_filter %>%
    add_row(als_metric_filter)
  
}
# ============ Compute density based ladder fuels metrics for uas data ===========


uas_combine <- tibble(
  campaign = numeric(),
  plot = numeric(),
  uas_ladder_fuel_1to2 = numeric(),
  uas_ladder_fuel_2to3 = numeric(),
  uas_ladder_fuel_3to4 = numeric(),
  uas_ladder_fuel_4to5 = numeric(),
  uas_ladder_fuel_5to6 = numeric(),
  uas_ladder_fuel_6to7 = numeric(),
  uas_ladder_fuel_7to8 = numeric()
)

uas_combine_filter <- tibble(
  campaign = numeric(),
  plot = numeric(),
  uas_ladder_fuel_1to2 = numeric(),
  uas_ladder_fuel_2to3 = numeric(),
  uas_ladder_fuel_3to4 = numeric(),
  uas_ladder_fuel_4to5 = numeric(),
  uas_ladder_fuel_5to6 = numeric(),
  uas_ladder_fuel_6to7 = numeric(),
  uas_ladder_fuel_7to8 = numeric()
)

for (uas_file in uas_las_files) {
  
  campaign <- str_extract(uas_file, '(?<=c)[:digit:]') %>%
    as.numeric()
  plot <- str_extract(uas_file, '(?<=p)[:digit:]+') %>%
    as.numeric()
  
  message('processing uas campaign ', campaign, ' plot ', plot)
  
  uas_las <- uas_file %>%
    readLAS(select = '')
  
  uas_metric <- as_tibble(uas_las$Z) %>% 
    rename(Z='value') %>%
    summarize(
      uas_ladder_fuel_1to2 = (sum(uas_las$Z > 1 & uas_las$Z <= 2) / sum(uas_las$Z <= 2)),
      uas_ladder_fuel_2to3 = (sum(uas_las$Z > 2 & uas_las$Z <= 3) / sum(uas_las$Z <= 3)),
      uas_ladder_fuel_3to4 = (sum(uas_las$Z > 3 & uas_las$Z <= 4) / sum(uas_las$Z <= 4)),
      uas_ladder_fuel_4to5 = (sum(uas_las$Z > 4 & uas_las$Z <= 5) / sum(uas_las$Z <= 5)),
      uas_ladder_fuel_5to6 = (sum(uas_las$Z > 5 & uas_las$Z <= 6) / sum(uas_las$Z <= 6)),
      uas_ladder_fuel_6to7 = (sum(uas_las$Z > 6 & uas_las$Z <= 7) / sum(uas_las$Z <= 7)),
      uas_ladder_fuel_7to8 = (sum(uas_las$Z > 7 & uas_las$Z <= 8) / sum(uas_las$Z <= 8))
    ) %>%
    add_column(campaign, plot, .before = 1)
  
  uas_combine <- uas_combine %>%
    add_row(uas_metric)
  
  uas_metric_filter <- as_tibble(uas_las$Z) %>% 
    rename(Z='value') %>%
    summarize(
      uas_ladder_fuel_1to2 = (sum(uas_las$Z > 1 & uas_las$Z <= 2) / sum(uas_las$Z >= 0 & uas_las$Z <= 2)),
      uas_ladder_fuel_2to3 = (sum(uas_las$Z > 2 & uas_las$Z <= 3) / sum(uas_las$Z >= 0 & uas_las$Z <= 3)),
      uas_ladder_fuel_3to4 = (sum(uas_las$Z > 3 & uas_las$Z <= 4) / sum(uas_las$Z >= 0 & uas_las$Z <= 4)),
      uas_ladder_fuel_4to5 = (sum(uas_las$Z > 4 & uas_las$Z <= 5) / sum(uas_las$Z >= 0 & uas_las$Z <= 5)),
      uas_ladder_fuel_5to6 = (sum(uas_las$Z > 5 & uas_las$Z <= 6) / sum(uas_las$Z >= 0 & uas_las$Z <= 6)),
      uas_ladder_fuel_6to7 = (sum(uas_las$Z > 6 & uas_las$Z <= 7) / sum(uas_las$Z >= 0 & uas_las$Z <= 7)),
      uas_ladder_fuel_7to8 = (sum(uas_las$Z > 7 & uas_las$Z <= 8) / sum(uas_las$Z >= 0 & uas_las$Z <= 8))
    ) %>%
    add_column(campaign, plot, .before = 1)
  
  uas_combine_filter <- uas_combine_filter %>%
    add_row(uas_metric_filter)
  
}

# ============ Compute density based ladder fuels metrics for ZEB data ===========


zeb_combine <- tibble(
  campaign = numeric(),
  plot = numeric(),
  zeb_ladder_fuel_1to2 = numeric(),
  zeb_ladder_fuel_2to3 = numeric(),
  zeb_ladder_fuel_3to4 = numeric(),
  zeb_ladder_fuel_4to5 = numeric(),
  zeb_ladder_fuel_5to6 = numeric(),
  zeb_ladder_fuel_6to7 = numeric(),
  zeb_ladder_fuel_7to8 = numeric()
)

zeb_combine_filter <- tibble(
  campaign = numeric(),
  plot = numeric(),
  zeb_ladder_fuel_1to2 = numeric(),
  zeb_ladder_fuel_2to3 = numeric(),
  zeb_ladder_fuel_3to4 = numeric(),
  zeb_ladder_fuel_4to5 = numeric(),
  zeb_ladder_fuel_5to6 = numeric(),
  zeb_ladder_fuel_6to7 = numeric(),
  zeb_ladder_fuel_7to8 = numeric()
)

for (zeb_file in zeb_las_files) {
  
  campaign <- str_extract(zeb_file, '(?<=c)[:digit:]') %>%
    as.numeric()
  plot <- str_extract(zeb_file, '(?<=p)[:digit:]+') %>%
    as.numeric()
  
  message('processing zeb campaign ', campaign, ' plot ', plot)
  
  zeb_las <- zeb_file %>%
    readLAS(select = '')
  
  zeb_metric <- as_tibble(zeb_las$Z) %>% 
    rename(Z='value') %>%
    summarize(
      zeb_ladder_fuel_1to2 = (sum(zeb_las$Z > 1 & zeb_las$Z <= 2) / sum(zeb_las$Z <= 2)),
      zeb_ladder_fuel_2to3 = (sum(zeb_las$Z > 2 & zeb_las$Z <= 3) / sum(zeb_las$Z <= 3)),
      zeb_ladder_fuel_3to4 = (sum(zeb_las$Z > 3 & zeb_las$Z <= 4) / sum(zeb_las$Z <= 4)),
      zeb_ladder_fuel_4to5 = (sum(zeb_las$Z > 4 & zeb_las$Z <= 5) / sum(zeb_las$Z <= 5)),
      zeb_ladder_fuel_5to6 = (sum(zeb_las$Z > 5 & zeb_las$Z <= 6) / sum(zeb_las$Z <= 6)),
      zeb_ladder_fuel_6to7 = (sum(zeb_las$Z > 6 & zeb_las$Z <= 7) / sum(zeb_las$Z <= 7)),
      zeb_ladder_fuel_7to8 = (sum(zeb_las$Z > 7 & zeb_las$Z <= 8) / sum(zeb_las$Z <= 8))
    ) %>%
    add_column(campaign, plot, .before = 1)
  
  zeb_combine <- zeb_combine %>%
    add_row(zeb_metric)
  
  zeb_metric_filter <- as_tibble(zeb_las$Z) %>% 
    rename(Z='value') %>%
    summarize(
      zeb_ladder_fuel_1to2 = (sum(zeb_las$Z > 1 & zeb_las$Z <= 2) / sum(zeb_las$Z >= 0 & zeb_las$Z <= 2)),
      zeb_ladder_fuel_2to3 = (sum(zeb_las$Z > 2 & zeb_las$Z <= 3) / sum(zeb_las$Z >= 0 & zeb_las$Z <= 3)),
      zeb_ladder_fuel_3to4 = (sum(zeb_las$Z > 3 & zeb_las$Z <= 4) / sum(zeb_las$Z >= 0 & zeb_las$Z <= 4)),
      zeb_ladder_fuel_4to5 = (sum(zeb_las$Z > 4 & zeb_las$Z <= 5) / sum(zeb_las$Z >= 0 & zeb_las$Z <= 5)),
      zeb_ladder_fuel_5to6 = (sum(zeb_las$Z > 5 & zeb_las$Z <= 6) / sum(zeb_las$Z >= 0 & zeb_las$Z <= 6)),
      zeb_ladder_fuel_6to7 = (sum(zeb_las$Z > 6 & zeb_las$Z <= 7) / sum(zeb_las$Z >= 0 & zeb_las$Z <= 7)),
      zeb_ladder_fuel_7to8 = (sum(zeb_las$Z > 7 & zeb_las$Z <= 8) / sum(zeb_las$Z >= 0 & zeb_las$Z <= 8))
    ) %>%
    add_column(campaign, plot, .before = 1)
  
  zeb_combine_filter <- zeb_combine_filter %>%
    add_row(zeb_metric_filter)
  
  
}
# ================================= banner data ================================

banner_data$Plot <- stringr::str_replace(banner_data$Plot, 'p', '')


banner_summary <- banner_data %>%
  group_by(Plot) %>%
  summarize(
    trad_ladder_fuel_1to2 = mean(trad_ladder_fuel_1to2, na.rm = TRUE),
    trad_ladder_fuel_2to3 = mean(trad_ladder_fuel_2to3, na.rm = TRUE),
    trad_ladder_fuel_3to4 = mean(trad_ladder_fuel_3to4, na.rm = TRUE)
  ) %>%
  rename(plot=Plot) %>%
  mutate_at('plot', as.numeric)

# ==============================================================================

combined_metrics <- tls_combine %>%
  full_join(als_combine, by = c('campaign','plot')) %>%
  full_join(zeb_combine, by = c('campaign','plot')) %>%
  full_join(uas_combine, by = c('campaign','plot')) %>%
  full_join(banner_summary, by = 'plot')

combined_metrics_filter <- tls_combine_filter %>%
  full_join(als_combine_filter, by = c('campaign','plot')) %>%
  full_join(zeb_combine_filter, by = c('campaign','plot')) %>%
  full_join(uas_combine_filter, by = c('campaign','plot')) %>%
  full_join(banner_summary, by = 'plot')

write_csv(combined_metrics, out_file)
write_csv(combined_metrics_filter, out_file_filtered)


# ==============================================================================


