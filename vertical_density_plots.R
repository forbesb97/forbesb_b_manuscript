# ==============================================================================
#
# TLS and ALS point cloud canopy base height
#
# ==============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 26 Jan 2021
#
# Status: Under development
#
# ==============================================================================
#
# Description:
#
# Vertical density plots of TLS, ALS, UAS, and Zeb
#
# ==============================================================================
#
# User inputs: - INCOMPLETE -
#
# tls_las_folder = Folder location for TLS .las files
# tls_las_files = list.files function to get tls files to be processed. Can be modified
#   with a pattern to restrict search
# als_las_folder = Folder location for ALS .las files
# resolution = Vector of resolutions (in meters) to use for chm and canopy cover
# rolling_window_size = Size (in meters) of window to apply for rolling mean smoothing
#   of VPP. Should be a multiple of resolution
# vpp_fig_output = folder for vpp graphs to be written to
#
# ==============================================================================
#
# Package dependences:
#
# sp, raster, lidR, tidyverse, glue, ggplot2, VoxR
#
# ==============================================================================
#
# Current state:
#
# Working through processing of single file visualizations
#
# ==============================================================================

library(lidR)
library(tidyverse)
library(glue)
library(ggplot2)
library(VoxR)
library(ggpubr)
library(viridisLite)

# ================================= User inputs ================================

 tls_las_folder <- 'D:/Analyses/ladder_fuel_vvp/tls_ladder_fuel_vvp'
#tls_las_folder <- 'data/las/tls'

 als_las_folder <- 'D:/Analyses/ladder_fuel_vvp/als_ladder_fuel_vvp'
#als_las_folder <- 'data/las/als'

uas_las_folder <-  'D:/Analyses/ladder_fuel_vvp/uas_ladder_fuel_vvp'
  
zeb_las_folder <-  'D:/Analyses/ladder_fuel_vvp/zeb_ladder_fuel_vvp'

vvp_fig_output <- 'D:/Analyses/ladder_fuel_vvp/ladder_fuel_vvp_full_figure_210510.png'

high_plot <- 26

medium_plot <- 11
  
low_plot <- 1304

eight_plot <- 1332

window_size <- 5

# ============================== ggplot theme set ==============================

theme_set(
  theme(
    text = element_text(family = 'serif', face = 'plain'),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    line = element_line(size = 0.5),
    axis.line = element_line(),
    axis.ticks = element_line(),
    panel.background = element_rect(color = 'black', linetype='solid', size = 0.5),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.key = element_blank(),
    legend.spacing = unit(0, "cm"),
    legend.margin = margin(0, 5, 0, 5)
  )
)

# ======================== Generate high plot =========================

z <- tibble(
  Z = as.numeric(),
  n = as.numeric(),
  p = as.numeric(),
  p_smooth = as.numeric(),
  method = as.character(),
  plot = as.character()
)

transect <- tibble(
  X = as.numeric(),
  Z = as.numeric(),
  method = as.character(),
  plot = as.character()
)

for (plot_type in c('high_plot', 'medium_plot', 'low_plot', 'eight_plot')) { 
  
  if (plot_type == 'high_plot') {
    plot = high_plot
  } else if (plot_type == 'medium_plot') {
    plot = medium_plot
  } else if (plot_type == 'low_plot') {
    plot = low_plot
  } else if (plot_type == 'eight_plot') {
    plot = eight_plot
  }
  
  # TLS
  tls <- list.files(tls_las_folder,
               pattern = glue('p{plot}'),
               full.names = TRUE) %>%
    readLAS(select = '')
  
  p_min <- as.numeric(tls@bbox[, 1])
  p_max <- as.numeric(tls@bbox[, 2])
  
  tls_transect <- clip_transect(
    tls,
    p1 = p_min,
    p2 = p_max,
    width = 3
  )
  
  tls_transect <- tls_transect@data %>%
    select(X, Z) %>%
    add_column(method = 'TLS',
               plot = plot_type)
  
  tls_z <- tls@data %>%
    mutate(Z = round(Z, digits = 1)) %>%
    group_by(Z) %>%
    summarize(n = n()) %>%
    filter(Z > 0.5) %>%
    mutate(p = n/sum(n, na.rm = TRUE)*100) %>%
    add_column(method = 'TLS',
               plot = plot_type) %>%
    mutate(p_smooth = data.table::frollmean(
      x = p,
      n = window_size,
      align = 'center'))
  
  rm(tls)
  
  
  # ZEB
  zeb <- list.files(zeb_las_folder,
               pattern = glue('p{plot}'),
               full.names = TRUE)%>%
    readLAS(select = '')

  zeb_transect <- clip_transect(
    zeb,
    p1 = p_min,
    p2 = p_max,
    width = 3
  )

  zeb_transect <- zeb_transect@data %>%
    select(X, Z) %>%
    add_column(method = 'MLS',
               plot = plot_type)


zeb_z <- zeb@data %>%
    mutate(Z = round(Z, digits = 1)) %>%
    group_by(Z) %>%
    summarize(n = n()) %>%
    filter(Z > 0.5) %>%
    mutate(p = n/sum(n, na.rm = TRUE)*100) %>%
    add_column(method = 'MLS',
               plot = plot_type) %>%
    mutate(p_smooth = data.table::frollmean(
      x = p,
      n = window_size,
      align = 'center'))

  rm(zeb)
  
  # UAS
  uas <- list.files(uas_las_folder,
               pattern = glue('p{plot}'),
               full.names = TRUE) %>%
    readLAS(select = '')

  uas_transect <- clip_transect(
    uas,
    p1 = p_min,
    p2 = p_max,
    width = 3
  )

  uas_transect <- uas_transect@data %>%
    select(X, Z) %>%
    add_column(method = 'UAS',
               plot = plot_type)

  uas_z <- uas@data %>%
    mutate(Z = round(Z, digits = 1)) %>%
    group_by(Z) %>%
    summarize(n = n()) %>%
    filter(Z > 0.5) %>%
    mutate(p = n/sum(n, na.rm = TRUE)*100) %>%
    add_column(method = 'UAS',
               plot = plot_type) %>%
    mutate(p_smooth = data.table::frollmean(
      x = p,
      n = window_size,
      align = 'center'))

  rm(uas)

  
  # ALS
  als <- list.files(als_las_folder,
               pattern = glue('p{plot}'),
               full.names = TRUE) %>%
    readLAS(select = '')

  als_transect <- clip_transect(
    als,
    p1 = p_min,
    p2 = p_max,
    width = 3
  )
  
  als_transect <- als_transect@data %>%
    select(X, Z) %>%
    add_column(method = 'ALS',
               plot = plot_type)
  
  als_z <- als@data %>%
    mutate(Z = round(Z, digits = 1)) %>%
    group_by(Z) %>%
    summarize(n = n()) %>%
    filter(Z > 0.5) %>%
    mutate(p = n/sum(n, na.rm = TRUE)*100) %>%
    add_column(method = 'ALS',
               plot = plot_type) %>%
    mutate(p_smooth = data.table::frollmean(
      x = p,
      n = window_size,
      align = 'center'))
  
  rm(als)

  transect <- transect %>%
    add_row(tls_transect) %>%
    add_row(als_transect) %>%
    add_row(zeb_transect) %>%
    add_row(uas_transect)
  
  z <- z %>%
    add_row(tls_z) %>%
    add_row(als_z) %>%
    add_row(zeb_z) %>%
    add_row(uas_z)
  
}


z$method <- factor(z$method, levels = c("TLS", "MLS", "ALS", "UAS"))
# =========================== Low vvp and transect ============================

low_vvp <-
  ggplot(data = z %>% 
           filter(plot == 'low_plot') %>%
           arrange(Z),
         mapping = aes(x = p_smooth, y = Z, color = method)) +
  geom_path(size=1) +
  scale_colour_viridis_d(option = 'C', end = 0.9)+
  xlab('Percentage of points (%)')+
  ylab('Low CBH plot \nabove ground height (m)')+
  geom_hline(yintercept = 8, linetype="dashed",col = 'grey50') +
  ylim(0,40) +
  theme(axis.text.x=element_text(color='black'),
        legend.position = c(0.7,0.8),
        legend.title = element_blank()) +
  xlim(0,4) 
low_vvp

 # plasma(4,end = 0.9 ) 
 # "#0D0887FF"- Purple/BLue TLS,  "#900DA4FF" - Purple ZEB, "#E16462FF" - Orange ALS, "#FCCE25FF" - gold UAS
tls_transect_low <-
  ggplot(data = transect %>%
           filter(method == 'TLS') %>%
           filter(plot == 'low_plot') %>%
           sample_n(1000000),
         mapping = aes(x = X, y = Z)) +
  geom_point(color='#0D0887FF', size = 0.4) +  
  coord_equal()+
  theme(legend.position = 'none',
        axis.text.x=element_text(color='white', size = 16),
        axis.text.y=element_blank(),
        axis.title.x=element_text(color='white'),
        axis.title.y=element_blank(),
        plot.title = element_text(hjust=0.5, size= 20, face='bold'),
        axis.ticks.x = element_blank())+
  geom_hline(yintercept = 8, linetype="dashed",col = 'grey50') +
  ylim(0,40)+
  xlim(525939, 525961)+
  xlab('')
tls_transect_low

zeb_transect_low <-
  ggplot(data = transect %>%
           filter(method == 'MLS') %>%
           filter(plot == 'low_plot') %>%
           sample_n(1000000),
         mapping = aes(x = X, y = Z)) +
  geom_point(color='#900DA4FF', size = 0.4) +
  coord_equal()+
  theme(legend.position = 'none',
        axis.text.x=element_text(color='white', size = 16),
        axis.text.y=element_blank(),
        axis.title.x=element_text(color='white'),
        axis.title.y=element_blank(),
        plot.title = element_text(hjust=0.5, size= 20, face='bold'),
        axis.ticks.x = element_blank())+
  geom_hline(yintercept = 8, linetype="dashed",col = 'grey50') +
  ylim(0,40)+
  xlim(525939, 525961)+
  xlab('')
zeb_transect_low

als_transect_low <-
  ggplot(data = transect %>%
           filter(method == 'ALS') %>%
           filter(plot == 'low_plot'),
         mapping = aes(x = X, y = Z)) +
  geom_point(size = 0.4, color="#E16462FF") +
  coord_equal()+
  theme(legend.position = 'none',
        axis.text.x=element_text(color='white', size = 16),
        axis.text.y=element_blank(),
        axis.title.x=element_text(color='white'),
        axis.title.y=element_blank(),
        plot.title = element_text(hjust=0.5, size= 20, face='bold'),
        axis.ticks.x = element_blank())+
  geom_hline(yintercept = 8, linetype="dashed",col = 'grey50') +
  ylim(0,40) +
  xlim(525939, 525961)+
  xlab('')
als_transect_low

uas_transect_low <-
  ggplot(data = transect %>%
           filter(method == 'UAS') %>%
           filter(plot == 'low_plot'),
         mapping = aes(x = X, y = Z)) +
  geom_point(size = 0.4, color="#FCCE25FF") +
  coord_equal() +
  theme(legend.position = 'none',
        axis.text.x=element_text(color='white', size = 16),
        axis.text.y=element_blank(),
        axis.title.x=element_text(color='white'),
        axis.title.y=element_blank(),
        plot.title = element_text(hjust=0.5, size= 20, face='bold'),
        axis.ticks.x = element_blank())+
  geom_hline(yintercept = 8, linetype="dashed",col = 'grey50') +
  ylim(0,40)+
  xlim(525939, 525961)+
  xlab('')
uas_transect_low


# ========================= Medium plot and transect ==========================

medium_vvp <-
  ggplot(data = z %>% 
           filter(plot == 'medium_plot') %>%
           arrange(Z),
         mapping = aes(x = p_smooth, y = Z, color = method)) +
  geom_path(size=1) +
  scale_colour_viridis_d('Method', option = 'C', end = 0.9)+
  ylab('Mean CBH plot \nabove ground height (m)')+
  geom_hline(yintercept = 8, linetype="dashed",col = 'grey50') +
  ylim(0,40)+
  xlim(0,4) +
  theme(axis.text.x = element_blank(),
        legend.position = c(0.7,0.8),
        legend.title = element_blank(),
        axis.title.x =element_blank())
medium_vvp

tls_transect_medium <-
  ggplot(data = transect %>%
           filter(method == 'TLS') %>%
           filter(plot == 'medium_plot') %>%
           sample_n(1000000),
         mapping = aes(x = X, y = Z)) +
  geom_point(color='#0D0887FF', size = 0.4) +  
  coord_equal()+
  theme(legend.position = 'none', 
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        plot.title = element_text(hjust=0.5, size= 20, face='bold'),
        axis.ticks.x = element_blank())+
  geom_hline(yintercept = 8, linetype="dashed",col = 'grey50') +
  ylim(0,40)+
  xlim(533165, 533187)
tls_transect_medium

zeb_transect_medium <-
  ggplot(data = transect %>%
           filter(method == 'MLS') %>%
           filter(plot == 'medium_plot') %>%
           sample_n(100000),
         mapping = aes(x = X, y = Z)) +
  geom_point(color='#900DA4FF', size = 0.4) +
  coord_equal()+
  theme(legend.position = 'none', 
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        plot.title = element_text(hjust=0.5, size= 20, face='bold'),
        axis.ticks.x = element_blank()) +
  geom_hline(yintercept = 8, linetype="dashed",col = 'grey50') +
  ylim(0,40)+
  xlim(533165, 533187)
zeb_transect_medium

als_transect_medium <-
  ggplot(data = transect %>%
           filter(method == 'ALS') %>%
           filter(plot == 'medium_plot'),
         mapping = aes(x = X, y = Z)) +
  geom_point(size = 0.4, color="#E16462FF") +
  coord_equal()+
  theme(legend.position = 'none', 
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        plot.title = element_text(hjust=0.5, size= 20, face='bold'),
        axis.ticks.x = element_blank())+
  geom_hline(yintercept = 8, linetype="dashed",col = 'grey50') +
  ylim(0,40) +
  xlim(533165, 533187)
als_transect_medium

uas_transect_medium <-
  ggplot(data = transect %>%
           filter(method == 'UAS') %>%
           filter(plot == 'medium_plot'),
         mapping = aes(x = X, y = Z)) +
  geom_point(size = 0.4, color="#FCCE25FF") +
  coord_equal()+  
  theme(legend.position = 'none', 
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        plot.title = element_text(hjust=0.5, size= 20, face='bold'),
        axis.ticks.x = element_blank())+
  geom_hline(yintercept = 8, linetype="dashed",col = 'grey50') +
  ylim(0,40)+
  xlim(533165, 533187)
uas_transect_medium

# 
# theme(legend.position = 'none',
#       axis.text.x=element_text(color='white', size = 16),
#       axis.text.y=element_blank(),
#       axis.title.x=element_text(color='white'),
#       axis.title.y=element_blank(),
#       plot.title = element_text(hjust=0.5, size= 20, face='bold'),
#       axis.ticks.x = element_blank())

# ============================ 8m plot and transect ============================

eight_vvp <-
  ggplot(data = z %>% 
           filter(plot == 'eight_plot') %>%
           arrange(Z),
         mapping = aes(x = p_smooth, y = Z, color = method)) +
  geom_path(size=1) +
  scale_colour_viridis_d('Method', option = 'C', end = 0.9)+
  xlab('Percentage of points (%)')+
  ylab('8m CBH plot \nabove ground height (m)')+
  geom_hline(yintercept = 8, linetype="dashed",col = 'grey50') +
  ylim(0,40)+
  xlim(0,4)+   
  theme(plot.title = element_text(hjust=0.5, size= 20, face='bold'),
                     axis.text.x = element_blank(),
                     axis.title.x =element_blank(),
                     legend.position = c(0.7,0.8),
                     legend.title = element_blank())
 
eight_vvp

tls_transect_eight <-
  ggplot(data = transect %>%
           filter(method == 'TLS') %>%
           filter(plot == 'eight_plot') %>%
           sample_n(1000000),
         mapping = aes(x = X, y = Z)) +
  geom_point(color='#0D0887FF', size = 0.4) +  
  coord_equal()+
  theme(legend.position = 'none', 
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        plot.title = element_text(hjust=0.5, size= 20, face='bold'),
        axis.ticks.x = element_blank())+
  geom_hline(yintercept = 8, linetype="dashed",col = 'grey50') +
  ylim(0,40)+
  xlim(524965, 524987)+ 
  xlab('')
tls_transect_eight

zeb_transect_eight <-
  ggplot(data = transect %>%
           filter(method == 'MLS') %>%
           filter(plot == 'eight_plot') %>%
           sample_n(100000),
         mapping = aes(x = X, y = Z)) +
  geom_point(color='#900DA4FF', size = 0.4) +
  coord_equal()+
  theme(legend.position = 'none', 
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        plot.title = element_text(hjust=0.5, size= 20, face='bold'),
        axis.ticks.x = element_blank()) +
  geom_hline(yintercept = 8, linetype="dashed",col = 'grey50') +
  ylim(0,40)+
  xlim(524965, 524987)+ 
  xlab('')
zeb_transect_eight

als_transect_eight <-
  ggplot(data = transect %>%
           filter(method == 'ALS') %>%
           filter(plot == 'eight_plot'),
         mapping = aes(x = X, y = Z)) +
  geom_point(size = 0.4, color="#E16462FF") +
  coord_equal()+
  theme(legend.position = 'none', 
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        plot.title = element_text(hjust=0.5, size= 20, face='bold'),
        axis.ticks.x = element_blank())+
  geom_hline(yintercept = 8, linetype="dashed",col = 'grey50') +
  ylim(0,40) +
  xlim(524965, 524987)+ 
  xlab('')
als_transect_eight

uas_transect_eight <-
  ggplot(data = transect %>%
           filter(method == 'UAS') %>%
           filter(plot == 'eight_plot'),
         mapping = aes(x = X, y = Z)) +
  geom_point(size = 0.4, color="#FCCE25FF") +
  coord_equal()+  
  theme(legend.position = 'none', 
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        plot.title = element_text(hjust=0.5, size= 20, face='bold'),
        axis.ticks.x = element_blank())+
  geom_hline(yintercept = 8, linetype="dashed",col = 'grey50') +
  ylim(0,40) +
  xlim(524965, 524987)+ 
  xlab('')
uas_transect_eight

# =========================== high vvp and transect ============================

high_vvp <-
  ggplot(data = z %>% 
           filter(plot == 'high_plot') %>%
           arrange(Z),
         mapping = aes(x = p_smooth, y = Z, color = method)) +
  geom_path(size=1) +
  scale_colour_viridis_d(option = 'C', end = 0.9)+
  ylab('High CBH plot \nabove ground height (m)')+
  geom_hline(yintercept = 8, linetype="dashed",col = 'grey50') +
  ylim(0,40) +
  ggtitle ('Vertical Vegetation Profile') +
  theme(plot.title = element_text(hjust=0.5, size= 20, face='bold'),
        axis.text.x = element_blank(),
        axis.title.x =element_blank(),
        legend.position = c(0.7,0.8),
        legend.title = element_blank()) +
  xlim(0,4) 
high_vvp

# plasma(4,end = 0.9 ) 
# "#0D0887FF"- Purple/BLue TLS,  "#900DA4FF" - Purple ZEB, "#E16462FF" - Orange ALS, "#FCCE25FF" - gold UAS
tls_transect_high <-
  ggplot(data = transect %>%
           filter(method == 'TLS') %>%
           filter(plot == 'high_plot') %>%
           sample_n(1000000),
         mapping = aes(x = X, y = Z)) +
  geom_point(color='#0D0887FF', size = 0.4) +  
  coord_equal()+
  ggtitle('TLS')+
  theme(legend.position = 'none',
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        plot.title = element_text(hjust=0.5, size= 20, face='bold'),
        axis.ticks.x = element_blank())+
  geom_hline(yintercept = 8, linetype="dashed",col = 'grey50') +
  ylim(0,40)+
  xlim(532621, 532643)
tls_transect_high

zeb_transect_high <-
  ggplot(data = transect %>%
           filter(method == 'MLS') %>%
           filter(plot == 'high_plot') %>%
           sample_n(1000000),
         mapping = aes(x = X, y = Z)) +
  geom_point(color='#900DA4FF', size = 0.4) +
  coord_equal()+
  ggtitle('HMLS')+
  theme(legend.position = 'none',
        axis.text.x=element_blank(),
        axis.text.y=element_blank(), 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        plot.title = element_text(hjust=0.5, size= 20, face='bold'),
        axis.ticks.x = element_blank()) +
  geom_hline(yintercept = 8, linetype="dashed",col = 'grey50') +
  ylim(0,40)+
  xlim(532621, 532643)
zeb_transect_high

als_transect_high <-
  ggplot(data = transect %>%
           filter(method == 'ALS') %>%
           filter(plot == 'high_plot'),
         mapping = aes(x = X, y = Z)) +
  geom_point(size = 0.4, color="#E16462FF") +
  coord_equal()+
  ggtitle('ALS')+
  theme(legend.position = 'none', 
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        plot.title = element_text(hjust=0.5, size= 20, face='bold'),
        axis.ticks.x = element_blank())+
  geom_hline(yintercept = 8, linetype="dashed",col = 'grey50') +
  ylim(0,40) +
  xlim(532621, 532643)
als_transect_high

uas_transect_high <-
  ggplot(data = transect %>%
           filter(method == 'UAS') %>%
           filter(plot == 'high_plot'),
         mapping = aes(x = X, y = Z)) +
  geom_point(size = 0.4, color="#FCCE25FF") +
  coord_equal()+  
  ggtitle('UAS') +
  theme(legend.position = 'none', 
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(), 
        plot.title = element_text(hjust=0.5, size= 20, face='bold'),
        axis.ticks.x = element_blank())+
  geom_hline(yintercept = 8, linetype="dashed",col = 'grey50') +
  ylim(0,40)+
  xlim(532621, 532643)
uas_transect_high

# ============================ Full VVP and transect ===========================

full_figure <-
  ggarrange(high_vvp,
            tls_transect_high,
            zeb_transect_high,
            uas_transect_high,
            als_transect_high,
            eight_vvp,
            tls_transect_eight,
            zeb_transect_eight,
            uas_transect_eight,
            als_transect_eight,
            medium_vvp,
            tls_transect_medium,
            zeb_transect_medium,
            uas_transect_medium,
            als_transect_medium,
            low_vvp,
            tls_transect_low,
            zeb_transect_low,
            uas_transect_low,
            als_transect_low,
            ncol = 5,
            nrow = 4,
            widths = c(4, 2, 2, 2, 2),
            heights = c(4.5, 4.5, 4.5, 5)
  )


ggsave(
  vvp_fig_output,
  plot=full_figure,
  width = 12,
  height = 14.5,
  units = 'in',
  dpi = 300 )



# ==============================================================================
