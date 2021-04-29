# ==============================================================================
#
# Create ladder fuel distribution by plot
#
# ==============================================================================
#
# Author: Brieanne Forbes
#
# Created: 8 April 2021
#
# ==============================================================================

library(tidyverse)
library(ggplot2)
library(glue)
library(ggpubr)

# ================================= User inputs ================================
tls_file <- read_csv('D:/Analyses/ladder_fuel_jmp/ladder-fuels_metrics_tls_zeb_als_uas_filtered_burned_210426.csv') 

histogram_out <- 'D:/Analyses/ladder_fuel_jmp/ladder_fuel_distribution_histogram_filtered_210427.png'
bar_out <- 'D:/Analyses/ladder_fuel_jmp/ladder_fuel_distribution_barplot_filtered_210427.png'

# ============================ wrangle the data ================================
tls_1to2 <- tls_file %>%
  select(plot, rbr_class, tls_ladder_fuel_1to2)%>%
  rename(ladder_fuel=tls_ladder_fuel_1to2)%>%
  add_column(variable='1to2')

tls_2to3 <- tls_file %>%
  select(plot, rbr_class, tls_ladder_fuel_2to3)%>%
  rename(ladder_fuel=tls_ladder_fuel_2to3)%>%
  add_column(variable='2to3')

tls_3to4 <- tls_file %>%
  select(plot, rbr_class, tls_ladder_fuel_3to4)%>%
  rename(ladder_fuel=tls_ladder_fuel_3to4)%>%
  add_column(variable='3to4')

tls_4to5 <- tls_file %>%
  select(plot, rbr_class, tls_ladder_fuel_4to5)%>%
  rename(ladder_fuel=tls_ladder_fuel_4to5)%>%
  add_column(variable='4to5')

tls_5to6 <- tls_file %>%
  select(plot, rbr_class, tls_ladder_fuel_5to6)%>%
  rename(ladder_fuel=tls_ladder_fuel_5to6)%>%
  add_column(variable='5to6')

tls_6to7 <- tls_file %>%
  select(plot, rbr_class, tls_ladder_fuel_6to7)%>%
  rename(ladder_fuel=tls_ladder_fuel_6to7)%>%
  add_column(variable='6to7')

tls_7to8 <- tls_file %>%
  select(plot, rbr_class, tls_ladder_fuel_7to8)%>%
  rename(ladder_fuel=tls_ladder_fuel_7to8)%>%
  add_column(variable='7to8')
  
tls_file2 <- tls_1to2 %>%
  add_row(tls_2to3) %>%
  add_row(tls_3to4) %>%
  add_row(tls_4to5) %>%
  add_row(tls_5to6) %>%
  add_row(tls_6to7)
  add_row(tls_7to8)

#===============================Set theme=======================================
theme_set(
  theme(
    text = element_text(family = 'serif', face = 'plain'),
    axis.title = element_text(size = 24),
    axis.text = element_text(size = 24),
    line = element_line(size = 1.5),
    axis.line = element_line(size = 1.5),
    panel.background = element_rect(color = 'white'),
    panel.border = element_rect(colour = 'black', fill = NA, size = 1.5),
    legend.title = element_text(size = 28),
    legend.text = element_text(size = 28),
    legend.spacing = unit(0, "cm"),
    legend.margin = margin(0, 5, 0, 5),
    legend.text.align = 0, 
    plot.title = element_text(size = 25, face = "bold")
  )
)


# ================================ histogram ===================================

tls_file$rbr_class <- factor(tls_file$rbr_class, levels = c('NC', 'Low', 'Medium', 'High'))

histogram_1to2 <-ggplot(data=tls_file, aes(x=tls_ladder_fuel_1to2, fill=rbr_class)) + 
  geom_histogram(binwidth = 0.025)+
  facet_wrap(~rbr_class)+
  scale_fill_manual(values = c('green3', 'yellow2','orange' , 'red'), limits=c('NC', 'Low', 'Medium', 'High')) +
  xlab('Ladder Fuel 1-2m')+
  ylab('Count')+
  theme(legend.position = 'none', strip.text.x = element_text(size = 16))
histogram_1to2

histogram_2to3 <-ggplot(data=tls_file, aes(x=tls_ladder_fuel_2to3, fill=rbr_class)) + 
  geom_histogram(binwidth = 0.025)+
  facet_wrap(~rbr_class)+
  scale_fill_manual(values = c('green3', 'yellow2','orange' , 'red'), limits=c('NC', 'Low', 'Medium', 'High')) +
  xlab('Ladder Fuel 2-3m')+
  ylab('Count')+
  theme(legend.position = 'none', strip.text.x = element_text(size = 16))
histogram_2to3

histogram_3to4 <-ggplot(data=tls_file, aes(x=tls_ladder_fuel_3to4, fill=rbr_class)) + 
  geom_histogram(binwidth = 0.025)+
  facet_wrap(~rbr_class)+
  scale_fill_manual(values = c('green3', 'yellow2','orange' , 'red'), limits=c('NC', 'Low', 'Medium', 'High')) +
  xlab('Ladder Fuel 3-4m')+
  ylab('Count')+
  theme(legend.position = 'none', strip.text.x = element_text(size = 16))
histogram_3to4

histogram_4to5 <-ggplot(data=tls_file, aes(x=tls_ladder_fuel_4to5, fill=rbr_class)) + 
  geom_histogram(binwidth = 0.025)+
  facet_wrap(~rbr_class)+
  scale_fill_manual(values = c('green3', 'yellow2','orange' , 'red'), limits=c('NC', 'Low', 'Medium', 'High')) +
  xlab('Ladder Fuel 4-5m')+
  ylab('Count')+
  theme(legend.position = 'none', strip.text.x = element_text(size = 16))
histogram_4to5

histogram_5to6 <-ggplot(data=tls_file, aes(x=tls_ladder_fuel_5to6, fill=rbr_class)) + 
  geom_histogram(binwidth = 0.025)+
  facet_wrap(~rbr_class)+
  scale_fill_manual(values = c('green3', 'yellow2','orange' , 'red'), limits=c('NC', 'Low', 'Medium', 'High')) +
  xlab('Ladder Fuel 5-6m')+
  ylab('Count')+
  theme(legend.position = 'none', strip.text.x = element_text(size = 16))
histogram_5to6

histogram_6to7 <-ggplot(data=tls_file, aes(x=tls_ladder_fuel_6to7, fill=rbr_class)) + 
  geom_histogram(binwidth = 0.025)+
  facet_wrap(~rbr_class)+
  scale_fill_manual(values = c('green3', 'yellow2','orange' , 'red'), limits=c('NC', 'Low', 'Medium', 'High')) +
  xlab('Ladder Fuel 6-7m')+
  ylab('Count')+
  theme(legend.position = 'none', strip.text.x = element_text(size = 16))
histogram_6to7

histogram_7to8<-ggplot(data=tls_file, aes(x=tls_ladder_fuel_7to8, fill=rbr_class)) + 
  geom_histogram(binwidth = 0.025)+
  facet_wrap(~rbr_class)+
  scale_fill_manual(values = c('green3', 'yellow2','orange' , 'red'), limits=c('NC', 'Low', 'Medium', 'High')) +
  xlab('Ladder Fuel 7-8m')+
  ylab('Count')+
  theme(legend.position = 'none', strip.text.x = element_text(size = 16))
histogram_7to8

histogram <- ggarrange(histogram_1to2, 
                       histogram_2to3, 
                       histogram_3to4, 
                       histogram_4to5, 
                       histogram_5to6, 
                       histogram_6to7, 
                       histogram_7to8,
                       ncol = 4,
                       nrow = 2,
                       widths = c(6, 6, 6, 6),
                       heights= c(6, 6))

ggsave(
  histogram_out,
  plot=histogram,
  width = 26,
  height = 13,
  units = 'in',
  dpi = 300 )

# ================================ bar plot ====================================


bar<-ggplot(data=tls_file2, aes(x=as.factor(plot),y=ladder_fuel)) +
  geom_bar(stat="identity", aes(fill=rbr_class,color=variable, x=reorder(plot, -ladder_fuel)))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_color_manual(values = c('black', 'grey40', 'grey70'))+
  scale_fill_manual(values = c('green3', 'yellow2','orange' , 'red'))
bar

bar_1to2<-ggplot(data=tls_file2%>%filter(variable=='1to2'), aes(x=as.factor(plot),y=ladder_fuel)) +
  geom_bar(stat="identity", aes(fill=rbr_class, x=reorder(plot, -ladder_fuel)))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_fill_manual(values = c('green3', 'yellow2','orange' , 'red'))
bar_1to2

bar_1to3<-ggplot(data=tls_file2%>%filter(variable=='1to3'), aes(x=as.factor(plot),y=ladder_fuel)) +
  geom_bar(stat="identity", aes(fill=rbr_class, x=reorder(plot, -ladder_fuel)))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_fill_manual(values = c('green3', 'yellow2','orange' , 'red'))
bar_1to3

bar_7to8<-ggplot(data=tls_file2%>%filter(variable=='7to8'), aes(x=as.factor(plot),y=ladder_fuel)) +
  geom_bar(stat="identity", aes(fill=rbr_class, x=reorder(plot, -ladder_fuel)))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_fill_manual(values = c('green3', 'yellow2','orange' , 'red'))
bar_7to8
