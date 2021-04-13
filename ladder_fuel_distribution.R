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

# ================================= User inputs ================================
tls_file <- read_csv('D:/Analyses/ladder_fuel_full_process_take4/not_filtered/ladder_fuel_tls_210331.csv') 

histogram_out <- 'D:/Analyses/ladder_fuel_full_process_take4/not_filtered/ladder_fuel_distribution_histogram_210408'
bar_out <- 'D:/Analyses/ladder_fuel_full_process_take4/not_filtered/ladder_fuel_distribution_barplot_210408'

# ============================ wrangle the data ================================
tls_1to2 <- tls_file %>%
  select(plot, RBR_3x3avg, ladder_fuel_1to2)%>%
  rename(ladder_fuel=ladder_fuel_1to2)%>%
  add_column(variable='1to2')

tls_1to3 <- tls_file %>%
  select(plot, RBR_3x3avg, ladder_fuel_1to3)%>%
  rename(ladder_fuel=ladder_fuel_1to3)%>%
  add_column(variable='1to3')

tls_7to8 <- tls_file %>%
  select(plot, RBR_3x3avg, ladder_fuel_7to8)%>%
  rename(ladder_fuel=ladder_fuel_7to8)%>%
  add_column(variable='7to8')
  
tls_file2 <- tls_1to2 %>%
  add_row(tls_1to3) %>%
  add_row(tls_7to8)

#===============================Set theme=======================================
theme_set(
  theme(
    text = element_text(family = 'serif', face = 'plain'),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    line = element_line(size = 1.5),
    axis.line = element_line(size = 1.5),
    panel.background = element_rect(color = 'white'),
    panel.border = element_rect(colour = 'black', fill = NA, size = 1.5),
    legend.title = element_text(size = 24),
    legend.text = element_text(size = 24),
    legend.spacing = unit(0, "cm"),
    legend.margin = margin(0, 5, 0, 5),
    legend.text.align = 0, 
    plot.title = element_text(size = 25, face = "bold")
  )
)

tls_file2$rbr_class <-
  cut(
    tls_file2$RBR_3x3avg,
    breaks = c( 0,  35, 130, 298, Inf),
    label = c('NC', 'Low', 'Medium', 'High')
  )
#tls_file$rbr_class <- factor(tls_file$rbr_class, levels = c("High", "Medium", "Low", "NC"))

# ================================ histogram ===================================


histogram <-ggplot(data=tls_file, aes(x=ladder_fuel_1to2, fill=rbr_class)) + 
  geom_histogram(binwidth = 0.05)+
  scale_color_manual(values = c('green3', 'yellow2','orange' , 'red'))+
  scale_fill_manual(values = c('green3', 'yellow2','orange' , 'red'))
histogram


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
