# ==============================================================================
#
# Create heat map for ladder fuel metrics from trad, ALS, UAV, ZEB, and banner
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

data <- read_csv('D:/Analyses/ladder_fuel_full_process_take3/filtered/ladder-fuels_metrics_tls_zeb_als_uas_filtered_210215.csv')

control_plots = c('1301','1303','1304','1306','1307','1309','1310','1312','1313',
                  '1315','1319','1325','1329','1332','1335','1336','1337','1340',
                  '1342','1349','1851','1852','1853','1854','11','13','15','17',
                  '2','20','24','26','3','5','8')

methods <- c('ALS', 'MLS', 'UAS')

outfile <- 'D:/Analyses/ladder_fuel_full_process_take3/filtered/ladder-fuel_heatmap_filtered_210219.png'

# =============================== tidy the data ================================
data <- data %>%
  filter(plot %in% control_plots)

tidy_data_tls <- data %>%
  select(plot, tls_ladder_fuel_1to2,tls_ladder_fuel_1to3, 
         tls_ladder_fuel_1to4,tls_ladder_fuel_7to8) %>% 
  add_column(method = 'TLS', .after = 'plot') %>%
  rename(ladder_fuel_1to2=tls_ladder_fuel_1to2) %>%
  rename(ladder_fuel_1to3=tls_ladder_fuel_1to3) %>%
  rename(ladder_fuel_1to4=tls_ladder_fuel_1to4) %>%
  rename(ladder_fuel_7to8=tls_ladder_fuel_7to8)

tidy_data_als <- data %>%
  select(plot, als_ladder_fuel_1to2,als_ladder_fuel_1to3, 
         als_ladder_fuel_1to4,als_ladder_fuel_7to8) %>% 
  add_column(method = 'ALS', .after = 'plot') %>%
  rename(ladder_fuel_1to2=als_ladder_fuel_1to2) %>%
  rename(ladder_fuel_1to3=als_ladder_fuel_1to3) %>%
  rename(ladder_fuel_1to4=als_ladder_fuel_1to4) %>%
  rename(ladder_fuel_7to8=als_ladder_fuel_7to8)

tidy_data_zeb <- data %>%
  select(plot, zeb_ladder_fuel_1to2,zeb_ladder_fuel_1to3, 
         zeb_ladder_fuel_1to4,zeb_ladder_fuel_7to8) %>% 
  add_column(method = 'MLS', .after = 'plot') %>%
  rename(ladder_fuel_1to2=zeb_ladder_fuel_1to2) %>%
  rename(ladder_fuel_1to3=zeb_ladder_fuel_1to3) %>%
  rename(ladder_fuel_1to4=zeb_ladder_fuel_1to4) %>%
  rename(ladder_fuel_7to8=zeb_ladder_fuel_7to8)

tidy_data_uav <- data %>%
  select(plot, uav_ladder_fuel_1to2,uav_ladder_fuel_1to3, 
         uav_ladder_fuel_1to4,uav_ladder_fuel_7to8) %>% 
  add_column(method = 'UAS', .after = 'plot') %>%
  rename(ladder_fuel_1to2=uav_ladder_fuel_1to2) %>%
  rename(ladder_fuel_1to3=uav_ladder_fuel_1to3) %>%
  rename(ladder_fuel_1to4=uav_ladder_fuel_1to4) %>%
  rename(ladder_fuel_7to8=uav_ladder_fuel_7to8)

tidy_data_banner <- data %>%
  select(plot, trad_ladder_fuel_1to2,
         trad_ladder_fuel_1to3,
         trad_ladder_fuel_1to4) %>%
  add_column(method = 'Banner', .after = 'plot') %>%
  rename(ladder_fuel_1to2=trad_ladder_fuel_1to2) %>%
  rename(ladder_fuel_1to3=trad_ladder_fuel_1to3) %>%
  rename(ladder_fuel_1to4=trad_ladder_fuel_1to4) 

tidy_data <- tidy_data_tls %>%
  add_row(tidy_data_als)%>%
  add_row(tidy_data_zeb)%>%
  add_row(tidy_data_uav)%>%
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
  
  metric_1to2 <-
    cor(tls$ladder_fuel_1to2, method_data$ladder_fuel_1to2, use='na.or.complete')
  metric_1to3 <-
    cor(tls$ladder_fuel_1to3, method_data$ladder_fuel_1to3, use='na.or.complete')
  metric_1to4 <-
    cor(tls$ladder_fuel_1to4, method_data$ladder_fuel_1to4, use='na.or.complete')
  metric_7to8 <-
    cor(tls$ladder_fuel_7to8, method_data$ladder_fuel_7to8, use='na.or.complete')
  
  lm_model_1to2 <- lm(
    formula = tls$ladder_fuel_1to2 ~ method_data$ladder_fuel_1to2)
  lm_model_coef_1to2 = summary(lm_model_1to2)$coefficients
  p.value_1to2 <-  lm_model_coef_1to2[2, 'Pr(>|t|)']
  
  lm_model_1to3 <- lm(
    formula = tls$ladder_fuel_1to3 ~ method_data$ladder_fuel_1to3)
  lm_model_coef_1to3 = summary(lm_model_1to3)$coefficients
  p.value_1to3 <-  lm_model_coef_1to3[2, 'Pr(>|t|)']
  
  lm_model_1to4 <- lm(
    formula = tls$ladder_fuel_1to4 ~ method_data$ladder_fuel_1to4)
  lm_model_coef_1to4 = summary(lm_model_1to4)$coefficients
  p.value_1to4 <-  lm_model_coef_1to4[2, 'Pr(>|t|)']

  lm_model_7to8 <- lm(
    formula = tls$ladder_fuel_7to8 ~ method_data$ladder_fuel_7to8)
  lm_model_coef_7to8 = summary(lm_model_7to8)$coefficients
  p.value_7to8 <-  lm_model_coef_7to8[2, 'Pr(>|t|)']

  correlation <- tibble(
    var1 = i,
    var2 = c('1-2m', '1-3m', '1-4m', '7-8m'),
    correlation = c(
      metric_1to2,
      metric_1to3,
      metric_1to4,
      metric_7to8
    ),
    p.value = c(
      p.value_1to2,
      p.value_1to3,
      p.value_1to4,
      p.value_7to8
    )
  )
  
  combined_correlation <- combined_correlation %>%
    add_row(correlation)
}
  
  metric_1to2 <-
    cor(tls$ladder_fuel_1to2, tidy_data_banner$ladder_fuel_1to2, use='na.or.complete')
  metric_1to3 <-
    cor(tls$ladder_fuel_1to3,tidy_data_banner$ladder_fuel_1to3, use='na.or.complete')
  metric_1to4 <-
    cor(tls$ladder_fuel_1to4, tidy_data_banner$ladder_fuel_1to4, use='na.or.complete')
  metric_7to8 <-
    NA
  
  lm_model_1to2 <- lm(
    formula = tls$ladder_fuel_1to2 ~ method_data$ladder_fuel_1to2)
  lm_model_coef_1to2 = summary(lm_model_1to2)$coefficients
  p.value_1to2 <-  lm_model_coef_1to2[2, 'Pr(>|t|)']
  
  lm_model_1to3 <- lm(
    formula = tls$ladder_fuel_1to3 ~ method_data$ladder_fuel_1to3)
  lm_model_coef_1to3 = summary(lm_model_1to3)$coefficients
  p.value_1to3 <-  lm_model_coef_1to3[2, 'Pr(>|t|)']
  
  lm_model_1to4 <- lm(
    formula = tls$ladder_fuel_1to4 ~ method_data$ladder_fuel_1to4)
  lm_model_coef_1to4 = summary(lm_model_1to4)$coefficients
  p.value_1to4 <-  lm_model_coef_1to4[2, 'Pr(>|t|)']
  
  p.value_7to8 <-  NA
  
  correlation <- tibble(
    var1 = 'Banner',
    var2 = c('1-2m', '1-3m', '1-4m', '7-8m'),
    correlation = c(
      metric_1to2,
      metric_1to3,
      metric_1to4,
      metric_7to8
    ),
    p.value = c(
      p.value_1to2,
      p.value_1to3,
      p.value_1to4,
      p.value_7to8
    )
    )
  
  combined_correlation <- combined_correlation %>%
    add_row(correlation)

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

combined_correlation$var1 <- factor(combined_correlation$var1 , levels = c("MLS", "ALS", "UAS", 'Banner'))

heat <-
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

write_csv(tidy_data_als_filter, 'D:/Analyses/ladder_fuel_full_process_take3/filtered/ladder_fuel_als_filtered_210218.csv')

tidy_data_tls_filter <- tidy_data_tls %>%
  select(plot, ladder_fuel_1to2, ladder_fuel_1to3, ladder_fuel_1to4, ladder_fuel_7to8)%>% 
  left_join(rbr, by='plot') %>%
  filter(!is.na(RBR_3x3avg))

write_csv(tidy_data_tls_filter, 'D:/Analyses/ladder_fuel_full_process_take3/filtered/ladder_fuel_tls_filtered_210218.csv')

tidy_data_banner_filter <- tidy_data_banner %>%
  select(plot, ladder_fuel_1to2, ladder_fuel_1to3, ladder_fuel_1to4)%>% 
  left_join(rbr, by='plot') %>%
  filter(!is.na(RBR_3x3avg))

write_csv(tidy_data_banner_filter, 'D:/Analyses/ladder_fuel_full_process_take3/filtered/ladder_fuel_banner_filtered_210219.csv')

tidy_data_zeb_filter <- tidy_data_zeb %>%
  select(plot, ladder_fuel_1to2, ladder_fuel_1to3, ladder_fuel_1to4, ladder_fuel_7to8)%>% 
  left_join(rbr, by='plot') %>%
  filter(!is.na(RBR_3x3avg))

write_csv(tidy_data_zeb_filter, 'D:/Analyses/ladder_fuel_full_process_take3/filtered/ladder_fuel_mls_filtered_210218.csv')

tidy_data_uav_filter <- tidy_data_uav %>%
  select(plot, ladder_fuel_1to2, ladder_fuel_1to3, ladder_fuel_1to4, ladder_fuel_7to8)%>% 
  left_join(rbr, by='plot') %>%
  filter(!is.na(RBR_3x3avg))

write_csv(tidy_data_uav_filter, 'D:/Analyses/ladder_fuel_full_process_take3/filtered/ladder_fuel_uas_filtered_210218.csv')



