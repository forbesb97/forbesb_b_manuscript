# ==============================================================================
#
# Create heat map for ladder fuel metrics from trad, ALS, UAS, ZEB, and banner
#
# ==============================================================================
#
# Author: Brieanne Forbes
#
# Created: 5 Jan 2021 
# Last commit: 
#
# ==============================================================================
#
# Known problems:
#
# 
#
# ==============================================================================

library(tidyverse)
library(ggplot2)
library(reshape2)
library(glue)

# ================================= User inputs ================================

data <- read_csv('D:/Analyses/ladder_fuel_jmp/ladder-fuels_metrics_tls_zeb_als_uas_filtered_offset_210524.csv')
#data <- read_csv('D:/Analyses/ladder_fuel_full_process_take4/filtered_zero_and_above/ladder-fuels_metrics_tls_zeb_als_uas_banner_filtered_210326.csv')


control_plots = c('1301','1303','1304','1306','1307','1309','1310','1312','1313',
                  '1315','1319','1325','1329','1332','1335','1336','1337','1340',
                  '1342','1349','1851','1852','1853','1854','2','3','5','8','11',
                  '13','15','17','20','24','26')

methods <- c('ALS', 'HMLS', 'UAS', 'Banner') #do not include tls, pulled out separately 

outfile <- 'D:/Analyses/ladder_fuel_jmp/ladder-fuel_heatmap_210528.png'
# outfile <- 'D:/Analyses/ladder_fuel_full_process_take4/filtered_zero_and_above/ladder-fuel_heatmap_filtered_210231.png'

# tidy_tls_outfile <- 'D:/Analyses/ladder_fuel_full_process_take4/not_filtered/ladder_fuel_tls_210331.csv'
# tidy_zeb_outfile <- 'D:/Analyses/ladder_fuel_full_process_take4/not_filtered/ladder_fuel_hmls_210331.csv'
# tidy_uas_outfile <- 'D:/Analyses/ladder_fuel_full_process_take4/not_filtered/ladder_fuel_uas_210331.csv'
# tidy_als_outfile <- 'D:/Analyses/ladder_fuel_full_process_take4/not_filtered/ladder_fuel_als_210331.csv'
# tidy_banner_outfile <- 'D:/Analyses/ladder_fuel_full_process_take4/not_filtered/ladder_fuel_banner_210331.csv'

# tidy_tls_outfile <- 'D:/Analyses/ladder_fuel_full_process_take4/filtered_zero_and_above/ladder_fuel_tls_filtered_210331.csv'
# tidy_zeb_outfile <- 'D:/Analyses/ladder_fuel_full_process_take4/filtered_zero_and_above/ladder_fuel_hmls_filtered_210331.csv'
# tidy_uas_outfile <- 'D:/Analyses/ladder_fuel_full_process_take4/filtered_zero_and_above/ladder_fuel_uas_filtered_210331.csv'
# tidy_als_outfile <- 'D:/Analyses/ladder_fuel_full_process_take4/filtered_zero_and_above/ladder_fuel_als_filtered_210331.csv'
# tidy_banner_outfile <- 'D:/Analyses/ladder_fuel_full_process_take4/filtered_zero_and_above/ladder_fuel_filtered_banner_210331.csv'

# =============================== tidy the data ================================
 data <- data %>%
   filter(plot %in% control_plots)


tidy_data_tls <- data %>%
  select(plot, tls_ladder_fuel_1to2,tls_ladder_fuel_2to3, 
         tls_ladder_fuel_3to4,tls_ladder_fuel_4to5,
         tls_ladder_fuel_5to6,tls_ladder_fuel_6to7,
         tls_ladder_fuel_7to8) %>% 
  add_column(method = 'TLS', .after = 'plot') %>%
  rename(ladder_fuel_1to2=tls_ladder_fuel_1to2) %>%
  rename(ladder_fuel_2to3=tls_ladder_fuel_2to3) %>%
  rename(ladder_fuel_3to4=tls_ladder_fuel_3to4) %>%
  rename(ladder_fuel_4to5=tls_ladder_fuel_4to5) %>%
  rename(ladder_fuel_5to6=tls_ladder_fuel_5to6) %>%
  rename(ladder_fuel_6to7=tls_ladder_fuel_6to7) %>%
  rename(ladder_fuel_7to8=tls_ladder_fuel_7to8)

tidy_data_als <- data %>%
  select(plot, als_ladder_fuel_1to2,als_ladder_fuel_2to3, 
         als_ladder_fuel_3to4,als_ladder_fuel_4to5,
         als_ladder_fuel_5to6,als_ladder_fuel_6to7,
         als_ladder_fuel_7to8) %>% 
  add_column(method = 'ALS', .after = 'plot') %>%
  rename(ladder_fuel_1to2=als_ladder_fuel_1to2) %>%
  rename(ladder_fuel_2to3=als_ladder_fuel_2to3) %>%
  rename(ladder_fuel_3to4=als_ladder_fuel_3to4) %>%
  rename(ladder_fuel_4to5=als_ladder_fuel_4to5) %>%
  rename(ladder_fuel_5to6=als_ladder_fuel_5to6) %>%
  rename(ladder_fuel_6to7=als_ladder_fuel_6to7) %>%
  rename(ladder_fuel_7to8=als_ladder_fuel_7to8)

tidy_data_zeb <- data %>%
  select(plot, zeb_ladder_fuel_1to2,zeb_ladder_fuel_2to3, 
         zeb_ladder_fuel_3to4,zeb_ladder_fuel_4to5,
         zeb_ladder_fuel_5to6,zeb_ladder_fuel_6to7,
         zeb_ladder_fuel_7to8) %>% 
  add_column(method = 'HMLS', .after = 'plot') %>%
  rename(ladder_fuel_1to2=zeb_ladder_fuel_1to2) %>%
  rename(ladder_fuel_2to3=zeb_ladder_fuel_2to3) %>%
  rename(ladder_fuel_3to4=zeb_ladder_fuel_3to4) %>%
  rename(ladder_fuel_4to5=zeb_ladder_fuel_4to5) %>%
  rename(ladder_fuel_5to6=zeb_ladder_fuel_5to6) %>%
  rename(ladder_fuel_6to7=zeb_ladder_fuel_6to7) %>%
  rename(ladder_fuel_7to8=zeb_ladder_fuel_7to8)

tidy_data_uas <- data %>%
  select(plot, uas_ladder_fuel_1to2,uas_ladder_fuel_2to3, 
         uas_ladder_fuel_3to4,uas_ladder_fuel_4to5,
         uas_ladder_fuel_5to6,uas_ladder_fuel_6to7,
         uas_ladder_fuel_7to8) %>% 
  add_column(method = 'UAS', .after = 'plot') %>%
  rename(ladder_fuel_1to2=uas_ladder_fuel_1to2) %>%
  rename(ladder_fuel_2to3=uas_ladder_fuel_2to3) %>%
  rename(ladder_fuel_3to4=uas_ladder_fuel_3to4) %>%
  rename(ladder_fuel_4to5=uas_ladder_fuel_4to5) %>%
  rename(ladder_fuel_5to6=uas_ladder_fuel_5to6) %>%
  rename(ladder_fuel_6to7=uas_ladder_fuel_6to7) %>%
  rename(ladder_fuel_7to8=uas_ladder_fuel_7to8)

tidy_data_banner <- data %>%
  select(plot, trad_ladder_fuel_1to2,trad_ladder_fuel_2to3, 
         trad_ladder_fuel_3to4) %>%
  add_column(method = 'Banner', .after = 'plot') %>%
  rename(ladder_fuel_1to2=trad_ladder_fuel_1to2) %>%
  rename(ladder_fuel_2to3=trad_ladder_fuel_2to3) %>%
  rename(ladder_fuel_3to4=trad_ladder_fuel_3to4) 

tidy_data <- tidy_data_tls %>%
  add_row(tidy_data_als)%>%
  add_row(tidy_data_zeb)%>%
  add_row(tidy_data_uas)%>%
  add_row(tidy_data_banner)           
                                  
# =============================== create matrix ================================

combined_correlation <- tibble(var1=as.character(), var2=as.character(), correlation=as.numeric(), p.value=as.numeric())

tls <- tidy_data %>%
  filter(method=='TLS')

for (i in methods) {
  method_data <- tidy_data %>%
    filter(method == i) 
  
  tls <- tidy_data %>%
    filter(method=='TLS')
  
  if (i == 'Banner') {  
  metric_1to2 <-
    cor(tls$ladder_fuel_1to2,  method_data$ladder_fuel_1to2, use='na.or.complete')
  metric_2to3 <-
    cor(tls$ladder_fuel_2to3, method_data$ladder_fuel_2to3, use='na.or.complete')
  metric_3to4 <-
    cor(tls$ladder_fuel_3to4,  method_data$ladder_fuel_3to4, use='na.or.complete')
  metric_4to5 <-
    NA
  metric_5to6 <-
    NA
  metric_6to7 <-
    NA
  metric_7to8 <-
    NA
  
  lm_model_1to2 <- lm(
    formula = tls$ladder_fuel_1to2 ~ method_data$ladder_fuel_1to2)
  lm_model_coef_1to2 = summary(lm_model_1to2)$coefficients
  p.value_1to2 <-  lm_model_coef_1to2[2, 'Pr(>|t|)']
  
  lm_model_2to3 <- lm(
    formula = tls$ladder_fuel_2to3 ~ method_data$ladder_fuel_2to3)
  lm_model_coef_2to3 = summary(lm_model_2to3)$coefficients
  p.value_2to3 <-  lm_model_coef_2to3[2, 'Pr(>|t|)']
  
  lm_model_3to4 <- lm(
    formula = tls$ladder_fuel_3to4 ~ method_data$ladder_fuel_3to4)
  lm_model_coef_3to4 = summary(lm_model_3to4)$coefficients
  p.value_3to4 <-  lm_model_coef_3to4[2, 'Pr(>|t|)']
  
  p.value_4to5 <-  NA
  p.value_5to6 <-  NA
  p.value_6to7 <-  NA
  p.value_7to8 <-  NA
  
  correlation <- tibble(
    var1 = i,
    var2 = c('1-2m', '2-3m', '3-4m', '4-5m', '5-6m', '6-7m', '7-8m'),
    correlation = c(
      metric_1to2,
      metric_2to3,
      metric_3to4,
      metric_4to5,
      metric_5to6,
      metric_6to7,
      metric_7to8
    ),
    p.value = c(
      p.value_1to2,
      p.value_2to3,
      p.value_3to4,
      p.value_4to5,
      p.value_5to6,
      p.value_6to7,
      p.value_7to8
    )
  )
  } 
  
  else {
    
  metric_1to2 <-
    cor(tls$ladder_fuel_1to2, method_data$ladder_fuel_1to2, use='na.or.complete')
  metric_2to3 <-
    cor(tls$ladder_fuel_2to3, method_data$ladder_fuel_2to3, use='na.or.complete')
  metric_3to4 <-
    cor(tls$ladder_fuel_3to4, method_data$ladder_fuel_3to4, use='na.or.complete')
  metric_4to5 <-
    cor(tls$ladder_fuel_4to5, method_data$ladder_fuel_4to5, use='na.or.complete')
  metric_5to6 <-
    cor(tls$ladder_fuel_5to6, method_data$ladder_fuel_5to6, use='na.or.complete')
  metric_3to4 <-
    cor(tls$ladder_fuel_6to7, method_data$ladder_fuel_6to7, use='na.or.complete')
  metric_7to8 <-
    cor(tls$ladder_fuel_7to8, method_data$ladder_fuel_7to8, use='na.or.complete')
  
  lm_model_1to2 <- lm(
    formula = tls$ladder_fuel_1to2 ~ method_data$ladder_fuel_1to2)
  lm_model_coef_1to2 = summary(lm_model_1to2)$coefficients
  p.value_1to2 <-  lm_model_coef_1to2[2, 'Pr(>|t|)']
  
  lm_model_2to3 <- lm(
    formula = tls$ladder_fuel_2to3 ~ method_data$ladder_fuel_2to3)
  lm_model_coef_2to3 = summary(lm_model_2to3)$coefficients
  p.value_2to3 <-  lm_model_coef_2to3[2, 'Pr(>|t|)']
  
  lm_model_3to4 <- lm(
    formula = tls$ladder_fuel_3to4 ~ method_data$ladder_fuel_3to4)
  lm_model_coef_3to4 = summary(lm_model_3to4)$coefficients
  p.value_3to4 <-  lm_model_coef_3to4[2, 'Pr(>|t|)']
  
  lm_model_4to5 <- lm(
    formula = tls$ladder_fuel_4to5 ~ method_data$ladder_fuel_4to5)
  lm_model_coef_4to5 = summary(lm_model_4to5)$coefficients
  p.value_4to5 <-  lm_model_coef_4to5[2, 'Pr(>|t|)']
  
  lm_model_5to6 <- lm(
    formula = tls$ladder_fuel_5to6 ~ method_data$ladder_fuel_5to6)
  lm_model_coef_5to6 = summary(lm_model_5to6)$coefficients
  p.value_5to6 <-  lm_model_coef_5to6[2, 'Pr(>|t|)']
  
  lm_model_6to7 <- lm(
    formula = tls$ladder_fuel_6to7 ~ method_data$ladder_fuel_6to7)
  lm_model_coef_6to7 = summary(lm_model_6to7)$coefficients
  p.value_6to7 <-  lm_model_coef_6to7[2, 'Pr(>|t|)']

  lm_model_7to8 <- lm(
    formula = tls$ladder_fuel_7to8 ~ method_data$ladder_fuel_7to8)
  lm_model_coef_7to8 = summary(lm_model_7to8)$coefficients
  p.value_7to8 <-  lm_model_coef_7to8[2, 'Pr(>|t|)']

  correlation <- tibble(
    var1 = i,
    var2 = c('1-2m', '2-3m', '3-4m', '4-5m', '5-6m', '6-7m', '7-8m'),
    correlation = c(
      metric_1to2,
      metric_2to3,
      metric_3to4,
      metric_4to5,
      metric_5to6,
      metric_6to7,
      metric_7to8
    ),
    p.value = c(
      p.value_1to2,
      p.value_2to3,
      p.value_3to4,
      p.value_4to5,
      p.value_5to6,
      p.value_6to7,
      p.value_7to8
    )
  )
  }
  combined_correlation <- combined_correlation %>%
    add_row(correlation)
}
  


combined_correlation$stars <- cut(combined_correlation$p.value, breaks=c(-Inf, 0.001, 0.01, 0.05, Inf), label=c("***", "**", "*", ""))

combined_correlation$corr_stars <- str_c(round(combined_correlation$correlation,2), combined_correlation$stars, sep = ' ') 



# =============================== create heat map ================================
theme_set(
  theme(
    text = element_text(family = 'serif', face = 'plain'),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    line = element_blank(),
    axis.line = element_blank(),
    panel.background = element_rect(color = 'white'),
    panel.border = element_blank(),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.key = element_blank(),
    legend.spacing = unit(0, "cm"),
    legend.margin = margin(0, 5, 0, 5),
    plot.caption = element_text(color = "grey25", face = "italic", size = 12)
  )
)

combined_correlation$var1 <- factor(combined_correlation$var1 , levels = c("HMLS", "UAS","ALS", 'Banner'))

heat_1to4 <-
  ggplot(data = combined_correlation, aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 5) +
  scale_fill_gradient(
    low = 'white',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(0, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x = '', y = 'Structure metrics (TLS)', 
  caption = 
  'A heatmap of Pearson product-moment correlation coefficient between 
  the best structure metrics for predicting RBR as identified by TLS-Riegl 
  (y-axis) and the corresponding metrics from other methods of data 
  collection (x-axis). 
  * p ≤ 0.05, ** p ≤ 0.01, *** p ≤ 0.001') +
  annotate("text", x = 1, y = 4.75, label = "n=34") +
  annotate("text", x = 2, y = 4.75, label = "n=35") +
  annotate("text", x = 3, y = 4.75, label = "n=30") +
  annotate("text", x = 4, y = 4.75, label = "n=34") +
  expand_limits(y=5)
heat_1to4

heat <-
  ggplot(data = combined_correlation %>% filter(var2!='1-4m'), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 5) +
  scale_fill_gradient(
    low = 'white',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(0, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x = '', y = 'Structure metrics (TLS, n=35)', 
       caption = 
         'A heatmap of Pearson product-moment correlation coefficient between 
  the best structure metrics for predicting RBR as identified by TLS-Riegl 
  (y-axis) and the corresponding metrics from other methods of data 
  collection (x-axis). 
  * p ≤ 0.05, ** p ≤ 0.01, *** p ≤ 0.001') +
  annotate("text", x = 1, y = 3.75, label = "n=34") +
  annotate("text", x = 2, y = 3.75, label = "n=30") +
  annotate("text", x = 3, y = 3.75, label = "n=35") +
  annotate("text", x = 4, y = 3.75, label = "n=34") +
  expand_limits(y=4)
heat

ggsave(
  outfile,
  plot=heat,
  width = 7.5,
  height = 7,
  units = 'in',
  dpi = 300)



# ======================== write method files to csv ==========================

rbr <- read_csv('D:/Analyses/kincade_glass_fire_tls_plot_centers_sentinel2a_20m_rbr.csv') 
rbr <- rbr %>%
  select(Plot, RBR_3x3avg) %>%
  rename(plot=Plot) 

rbr$plot <- rbr$plot %>%
  str_replace( 'p', '') %>%
  str_replace('P', '')

rbr <- rbr %>%
  mutate_at('plot', as.numeric)

tidy_data_als_filter <- tidy_data_als %>%
  select(plot, ladder_fuel_1to2, ladder_fuel_1to3, ladder_fuel_1to4, ladder_fuel_7to8) %>% 
  left_join(rbr, by='plot') %>%
  filter(!is.na(RBR_3x3avg))

tidy_data_tls_filter <- tidy_data_tls %>%
  select(plot, ladder_fuel_1to2, ladder_fuel_1to3, ladder_fuel_1to4, ladder_fuel_7to8)%>% 
  left_join(rbr, by='plot') %>%
  filter(!is.na(RBR_3x3avg))

tidy_data_banner_filter <- tidy_data_banner %>%
  select(plot, ladder_fuel_1to2, ladder_fuel_1to3, ladder_fuel_1to4)%>% 
  left_join(rbr, by='plot') %>%
  filter(!is.na(RBR_3x3avg))

tidy_data_zeb_filter <- tidy_data_zeb %>%
  select(plot, ladder_fuel_1to2, ladder_fuel_1to3, ladder_fuel_1to4, ladder_fuel_7to8)%>% 
  left_join(rbr, by='plot') %>%
  filter(!is.na(RBR_3x3avg))

tidy_data_uas_filter <- tidy_data_uas %>%
  select(plot, ladder_fuel_1to2, ladder_fuel_1to3, ladder_fuel_1to4, ladder_fuel_7to8)%>% 
  left_join(rbr, by='plot') %>%
  filter(!is.na(RBR_3x3avg))

write_csv(tidy_data_als_filter, tidy_als_outfile)
write_csv(tidy_data_tls_filter, tidy_tls_outfile)
write_csv(tidy_data_banner_filter, tidy_banner_outfile)
write_csv(tidy_data_zeb_filter, tidy_zeb_outfile)
write_csv(tidy_data_uas_filter, tidy_uas_outfile)



