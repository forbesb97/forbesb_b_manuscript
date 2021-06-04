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
library(ggpubr)
library(gridGraphics)

# ================================= User inputs ================================

data <- read_csv('D:/Analyses/ladder_fuel_jmp/ladder-fuels_metrics_tls_zeb_als_uas_filtered_offset_210601.csv')
#data <- read_csv('D:/Analyses/ladder_fuel_full_process_take4/filtered_zero_and_above/ladder-fuels_metrics_tls_zeb_als_uas_banner_filtered_210326.csv')
tuk_data <- read_csv('D:/Analyses/ladder_fuel_jmp/ladder-fuels_metrics_tls_zeb_als_uas_filtered_210529_tukman.csv')

control_plots = c('1301','1303','1304','1306','1307','1309','1310','1312','1313',
                  '1315','1319','1325','1329','1332','1335','1336','1337','1340',
                  '1342','1349','1851','1852','1853','1854','2','3','5','8','11',
                  '13','15','17','20','24','26')

methods <- c('TLS', 'HMLS', 'UAS','ALS',  'Banner')


tuk_outfile <- 'D:/Analyses/ladder_fuel_jmp/ladder-fuel_heatmap_tukman_210602.png'
outfile <- 'D:/Analyses/ladder_fuel_jmp/ladder-fuel_heatmap_210604.png'

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

tuk_data <- tuk_data %>%
  filter(plot %in% control_plots)

data <- data %>%
  left_join(tuk_data)


tidy_data_tls <- data %>%
  dplyr::select(plot, tls_ladder_fuel_1to2,tls_ladder_fuel_2to3, 
         tls_ladder_fuel_3to4,tls_ladder_fuel_4to5,
         tls_ladder_fuel_5to6,tls_ladder_fuel_6to7,
         tls_ladder_fuel_7to8, tls_ladder_fuel_1to4, 
         tls_ladder_fuel_1to8) %>% 
  add_column(method = 'TLS', .after = 'plot') %>%
  rename(ladder_fuel_1to2=tls_ladder_fuel_1to2) %>%
  rename(ladder_fuel_2to3=tls_ladder_fuel_2to3) %>%
  rename(ladder_fuel_3to4=tls_ladder_fuel_3to4) %>%
  rename(ladder_fuel_4to5=tls_ladder_fuel_4to5) %>%
  rename(ladder_fuel_5to6=tls_ladder_fuel_5to6) %>%
  rename(ladder_fuel_6to7=tls_ladder_fuel_6to7) %>%
  rename(ladder_fuel_7to8=tls_ladder_fuel_7to8)%>%
  rename(ladder_fuel_1to4=tls_ladder_fuel_1to4) %>%
  rename(ladder_fuel_1to8=tls_ladder_fuel_1to8)

tidy_data_als <- data %>%
  dplyr::select(plot, als_ladder_fuel_1to2,als_ladder_fuel_2to3, 
         als_ladder_fuel_3to4,als_ladder_fuel_4to5,
         als_ladder_fuel_5to6,als_ladder_fuel_6to7,
         als_ladder_fuel_7to8, als_ladder_fuel_1to4,
         als_ladder_fuel_1to8) %>% 
  add_column(method = 'ALS', .after = 'plot') %>%
  rename(ladder_fuel_1to2=als_ladder_fuel_1to2) %>%
  rename(ladder_fuel_2to3=als_ladder_fuel_2to3) %>%
  rename(ladder_fuel_3to4=als_ladder_fuel_3to4) %>%
  rename(ladder_fuel_4to5=als_ladder_fuel_4to5) %>%
  rename(ladder_fuel_5to6=als_ladder_fuel_5to6) %>%
  rename(ladder_fuel_6to7=als_ladder_fuel_6to7) %>%
  rename(ladder_fuel_7to8=als_ladder_fuel_7to8)%>%
  rename(ladder_fuel_1to4=als_ladder_fuel_1to4) %>%
  rename(ladder_fuel_1to8=als_ladder_fuel_1to8)

tidy_data_zeb <- data %>%
  dplyr::select(plot, zeb_ladder_fuel_1to2,zeb_ladder_fuel_2to3, 
         zeb_ladder_fuel_3to4,zeb_ladder_fuel_4to5,
         zeb_ladder_fuel_5to6,zeb_ladder_fuel_6to7,
         zeb_ladder_fuel_7to8, zeb_ladder_fuel_1to4,
         zeb_ladder_fuel_1to8) %>% 
  add_column(method = 'HMLS', .after = 'plot') %>%
  rename(ladder_fuel_1to2=zeb_ladder_fuel_1to2) %>%
  rename(ladder_fuel_2to3=zeb_ladder_fuel_2to3) %>%
  rename(ladder_fuel_3to4=zeb_ladder_fuel_3to4) %>%
  rename(ladder_fuel_4to5=zeb_ladder_fuel_4to5) %>%
  rename(ladder_fuel_5to6=zeb_ladder_fuel_5to6) %>%
  rename(ladder_fuel_6to7=zeb_ladder_fuel_6to7) %>%
  rename(ladder_fuel_7to8=zeb_ladder_fuel_7to8)%>%
  rename(ladder_fuel_1to4=zeb_ladder_fuel_1to4) %>%
  rename(ladder_fuel_1to8=zeb_ladder_fuel_1to8)

tidy_data_uas <- data %>%
  dplyr::select(plot, uas_ladder_fuel_1to2,uas_ladder_fuel_2to3, 
         uas_ladder_fuel_3to4,uas_ladder_fuel_4to5,
         uas_ladder_fuel_5to6,uas_ladder_fuel_6to7,
         uas_ladder_fuel_7to8, uas_ladder_fuel_1to4, 
         uas_ladder_fuel_1to8) %>% 
  add_column(method = 'UAS', .after = 'plot') %>%
  rename(ladder_fuel_1to2=uas_ladder_fuel_1to2) %>%
  rename(ladder_fuel_2to3=uas_ladder_fuel_2to3) %>%
  rename(ladder_fuel_3to4=uas_ladder_fuel_3to4) %>%
  rename(ladder_fuel_4to5=uas_ladder_fuel_4to5) %>%
  rename(ladder_fuel_5to6=uas_ladder_fuel_5to6) %>%
  rename(ladder_fuel_6to7=uas_ladder_fuel_6to7) %>%
  rename(ladder_fuel_7to8=uas_ladder_fuel_7to8)%>%
  rename(ladder_fuel_1to4=uas_ladder_fuel_1to4) %>%
  rename(ladder_fuel_1to8=uas_ladder_fuel_1to8)

tidy_data_banner <- data %>%
  dplyr::select(plot, trad_ladder_fuel_1to2,trad_ladder_fuel_2to3, 
         trad_ladder_fuel_3to4, trad_ladder_fuel_1to4) %>%
  add_column(method = 'Banner', .after = 'plot') %>%
  rename(ladder_fuel_1to2=trad_ladder_fuel_1to2) %>%
  rename(ladder_fuel_2to3=trad_ladder_fuel_2to3) %>%
  rename(ladder_fuel_3to4=trad_ladder_fuel_3to4)  %>%
  rename(ladder_fuel_1to4=trad_ladder_fuel_1to4)

tidy_data <- tidy_data_tls %>%
  add_row(tidy_data_als)%>%
  add_row(tidy_data_zeb)%>%
  add_row(tidy_data_uas)%>%
  add_row(tidy_data_banner)           
                                  
# =============================== create matrix ================================

combined_correlation <- tibble(var1=as.character(), var2=as.character(), 
                               correlation=as.numeric(), p.value=as.numeric(), 
                               metric=as.character())





  for (i in methods) {
    for (j in methods) {
    
    var1_data <- tidy_data %>%
      filter(method == i) 
    
    var2_data <- tidy_data %>%
      filter(method == j )
    
    if (i == 'Banner'|| j == 'Banner') {  
    metric_1to2 <-
      cor(var1_data$ladder_fuel_1to2,  var2_data$ladder_fuel_1to2, use='na.or.complete')
    metric_2to3 <-
      cor(var1_data$ladder_fuel_2to3, var2_data$ladder_fuel_2to3, use='na.or.complete')
    metric_3to4 <-
      cor(var1_data$ladder_fuel_3to4,  var2_data$ladder_fuel_3to4, use='na.or.complete')
    metric_4to5 <-
      NA
    metric_5to6 <-
      NA
    metric_6to7 <-
      NA
    metric_7to8 <-
      NA
    metric_1to4 <-
      cor(var1_data$ladder_fuel_1to4,  var2_data$ladder_fuel_1to4, use='na.or.complete')
    metric_1to8 <-
      NA
    
    lm_model_1to2 <- lm(
      formula = var1_data$ladder_fuel_1to2 ~ var2_data$ladder_fuel_1to2)
    lm_model_coef_1to2 = summary(lm_model_1to2)$coefficients
    p.value_1to2 <-  lm_model_coef_1to2[2, 'Pr(>|t|)']
    
    lm_model_2to3 <- lm(
      formula = var1_data$ladder_fuel_2to3 ~ var2_data$ladder_fuel_2to3)
    lm_model_coef_2to3 = summary(lm_model_2to3)$coefficients
    p.value_2to3 <-  lm_model_coef_2to3[2, 'Pr(>|t|)']
    
    lm_model_3to4 <- lm(
      formula = var1_data$ladder_fuel_3to4 ~ var2_data$ladder_fuel_3to4)
    lm_model_coef_3to4 = summary(lm_model_3to4)$coefficients
    p.value_3to4 <-  lm_model_coef_3to4[2, 'Pr(>|t|)']
    
    lm_model_1to4 <- lm(
      formula = var1_data$ladder_fuel_1to4 ~ var2_data$ladder_fuel_1to4)
    lm_model_coef_1to4 = summary(lm_model_1to4)$coefficients
    p.value_1to4 <-  lm_model_coef_1to4[2, 'Pr(>|t|)']
    
    p.value_4to5 <-  NA
    p.value_5to6 <-  NA
    p.value_6to7 <-  NA
    p.value_7to8 <-  NA
    p.value_1to8 <-  NA
    
    correlation <- tibble(
      var1 = i,
      var2 = j,
      correlation = c(
        metric_1to2,
        metric_2to3,
        metric_3to4,
        metric_4to5,
        metric_5to6,
        metric_6to7,
        metric_7to8,
        metric_1to4,
        metric_1to8
      ),
      p.value = c(
        p.value_1to2,
        p.value_2to3,
        p.value_3to4,
        p.value_4to5,
        p.value_5to6,
        p.value_6to7,
        p.value_7to8,
        p.value_1to4,
        p.value_1to8
      ),
      metric = c('1-2m',
        '2-3m',
        '3-4m',
        '4-5m',
        '5-6m',
        '6-7m',
        '7-8m',
        '1-4m',
        '1-8m')
    )
    } 
    
    else {
      
    metric_1to2 <-
      cor(var1_data$ladder_fuel_1to2, var2_data$ladder_fuel_1to2, use='pairwise.complete.obs')
    metric_2to3 <-
      cor(var1_data$ladder_fuel_2to3, var2_data$ladder_fuel_2to3, use='pairwise.complete.obs')
    metric_3to4 <-
      cor(var1_data$ladder_fuel_3to4, var2_data$ladder_fuel_3to4, use='pairwise.complete.obs')
    metric_4to5 <-
      cor(var1_data$ladder_fuel_4to5, var2_data$ladder_fuel_4to5, use='pairwise.complete.obs')
    metric_5to6 <-
      cor(var1_data$ladder_fuel_5to6, var2_data$ladder_fuel_5to6, use='pairwise.complete.obs')
    metric_6to7 <-
      cor(var1_data$ladder_fuel_6to7, var2_data$ladder_fuel_6to7, use='pairwise.complete.obs')
    metric_7to8 <-
      cor(var1_data$ladder_fuel_7to8, var2_data$ladder_fuel_7to8, use='pairwise.complete.obs')
    metric_1to4 <-
      cor(var1_data$ladder_fuel_1to4, var2_data$ladder_fuel_1to4, use='pairwise.complete.obs')
    metric_1to8 <-
      cor(var1_data$ladder_fuel_1to8, var2_data$ladder_fuel_1to8, use='pairwise.complete.obs')
    
    lm_model_1to2 <- lm(
      formula = var1_data$ladder_fuel_1to2 ~ var2_data$ladder_fuel_1to2)
    lm_model_coef_1to2 = summary(lm_model_1to2)$coefficients
    p.value_1to2 <-  lm_model_coef_1to2[2, 'Pr(>|t|)']
    
    lm_model_2to3 <- lm(
      formula = var1_data$ladder_fuel_2to3 ~ var2_data$ladder_fuel_2to3)
    lm_model_coef_2to3 = summary(lm_model_2to3)$coefficients
    p.value_2to3 <-  lm_model_coef_2to3[2, 'Pr(>|t|)']
    
    lm_model_3to4 <- lm(
      formula = var1_data$ladder_fuel_3to4 ~ var2_data$ladder_fuel_3to4)
    lm_model_coef_3to4 = summary(lm_model_3to4)$coefficients
    p.value_3to4 <-  lm_model_coef_3to4[2, 'Pr(>|t|)']
    
    lm_model_4to5 <- lm(
      formula = var1_data$ladder_fuel_4to5 ~ var2_data$ladder_fuel_4to5)
    lm_model_coef_4to5 = summary(lm_model_4to5)$coefficients
    p.value_4to5 <-  lm_model_coef_4to5[2, 'Pr(>|t|)']
    
    lm_model_5to6 <- lm(
      formula = var1_data$ladder_fuel_5to6 ~ var2_data$ladder_fuel_5to6)
    lm_model_coef_5to6 = summary(lm_model_5to6)$coefficients
    p.value_5to6 <-  lm_model_coef_5to6[2, 'Pr(>|t|)']
    
    lm_model_6to7 <- lm(
      formula = var1_data$ladder_fuel_6to7 ~ var2_data$ladder_fuel_6to7)
    lm_model_coef_6to7 = summary(lm_model_6to7)$coefficients
    p.value_6to7 <-  lm_model_coef_6to7[2, 'Pr(>|t|)']
  
    lm_model_7to8 <- lm(
      formula = var1_data$ladder_fuel_7to8 ~ var2_data$ladder_fuel_7to8)
    lm_model_coef_7to8 = summary(lm_model_7to8)$coefficients
    p.value_7to8 <-  lm_model_coef_7to8[2, 'Pr(>|t|)']
    
    lm_model_1to4 <- lm(
      formula = var1_data$ladder_fuel_1to4 ~ var2_data$ladder_fuel_1to4)
    lm_model_coef_1to4 = summary(lm_model_1to4)$coefficients
    p.value_1to4 <-  lm_model_coef_1to4[2, 'Pr(>|t|)']
    
    lm_model_1to8 <- lm(
      formula = var1_data$ladder_fuel_1to8 ~ var2_data$ladder_fuel_1to8)
    lm_model_coef_1to8 = summary(lm_model_1to8)$coefficients
    p.value_1to8 <-  lm_model_coef_1to8[2, 'Pr(>|t|)']
  
    correlation <- tibble(
      var1 = i,
      var2 = j,
      correlation = c(
        metric_1to2,
        metric_2to3,
        metric_3to4,
        metric_4to5,
        metric_5to6,
        metric_6to7,
        metric_7to8,
        metric_1to4,
        metric_1to8
      ),
      p.value = c(
        p.value_1to2,
        p.value_2to3,
        p.value_3to4,
        p.value_4to5,
        p.value_5to6,
        p.value_6to7,
        p.value_7to8,
        p.value_1to4,
        p.value_1to8
      ),
      metric = c('1-2m',
                 '2-3m',
                 '3-4m',
                 '4-5m',
                 '5-6m',
                 '6-7m',
                 '7-8m',
                 '1-4m',
                 '1-8m'
      )
    )
    }

      combined_correlation <- combined_correlation %>%
        add_row(correlation)
  }
  }


combined_correlation$stars <- cut(combined_correlation$p.value, breaks=c(-Inf, 0.001, 0.01, 0.05, Inf), label=c("***", "**", "*", ""))

combined_correlation$correlation[which(is.na(combined_correlation$p.value))] = NA

combined_correlation$corr_stars <- str_c(round(combined_correlation$correlation,2), combined_correlation$stars, sep = ' ') 
 

# =============================== create heat map ================================
theme_set(
  theme(
    text = element_text(family = 'serif', face = 'plain'),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 22),
    line = element_blank(),
    axis.line = element_blank(),
    panel.background = element_rect(color = 'white'),
    panel.border = element_blank(),
    legend.title = element_text(size = 26),
    legend.text = element_text(size = 22),
    legend.key = element_blank(),
    legend.key.size = unit(2.75, 'cm'),
    legend.spacing = unit(0, "cm"),
    legend.margin = margin(0, 5, 0, 5),
    plot.title = element_text(size = 26, hjust = 0.5, face = 'bold')
  )
)


data_1to2 <- combined_correlation %>% 
  filter(metric == '1-2m') 
data_1to2$var1 <- factor(data_1to2$var1 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))
data_1to2$var2 <- factor(data_1to2$var2 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))

heat_1to2 <-
  ggplot(data = data_1to2 %>% filter(row_number() >= which(var1 == var2)) , aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient(
    low = 'white',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(0, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=30", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=35", size=6) +
  annotate("text", x = 5, y = 5.75, label = "n=34", size=6) +
  expand_limits(y=6)+
  ggtitle('Ladder fuels 1-2m')+
  theme(legend.position = 'none')
heat_1to2

data_2to3 <- combined_correlation %>% 
  filter(metric == '2-3m') 
data_2to3$var1 <- factor(data_2to3$var1 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))
data_2to3$var2 <- factor(data_2to3$var2 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))

heat_2to3 <-
  ggplot(data = data_2to3 %>% filter(row_number() >= which(var1 == var2)) , aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient(
    low = 'white',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(0, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=30", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=35", size=6) +
  annotate("text", x = 5, y = 5.75, label = "n=34", size=6) +
  expand_limits(y=6)+
  ggtitle('Ladder fuels 2-3m')+
  theme(legend.position = 'none')
heat_2to3

data_3to4 <- combined_correlation %>% 
  filter(metric == '3-4m') 
data_3to4$var1 <- factor(data_3to4$var1 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))
data_3to4$var2 <- factor(data_3to4$var2 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))

heat_3to4 <-
  ggplot(data = data_3to4 %>% filter(row_number() >= which(var1 == var2)) , aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient(
    low = 'white',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(0, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=30", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=35", size=6) +
  annotate("text", x = 5, y = 5.75, label = "n=34", size=6) +
  expand_limits(y=6)+
  ggtitle('Ladder fuels 3-4m')+
  theme(legend.position = 'none')
heat_3to4

data_4to5 <- combined_correlation %>% 
  filter(metric == '4-5m',
  var1 != 'Banner',
  var2 != 'Banner')
data_4to5$var1 <- factor(data_4to5$var1 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))
data_4to5$var2 <- factor(data_4to5$var2 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))

heat_4to5 <-
  ggplot(data = data_4to5 %>% filter(row_number() >= which(var1 == var2)) , aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient(
    low = 'white',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(0, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=30", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=35", size=6) +
  expand_limits(y=5)+
  ggtitle('Ladder fuels 4-5m')+
  theme(legend.position = 'none')
heat_4to5

data_5to6 <- combined_correlation %>% 
  filter(metric == '5-6m',
         var1 != 'Banner',
         var2 != 'Banner')
data_5to6$var1 <- factor(data_5to6$var1 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))
data_5to6$var2 <- factor(data_5to6$var2 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))

heat_5to6 <-
  ggplot(data = data_5to6 %>% filter(row_number() >= which(var1 == var2)) , aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient(
    low = 'white',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(0, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=30", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=35", size=6) +
  expand_limits(y=5)+
  ggtitle('Ladder fuels 5-6m')+
  theme(legend.position = 'none')
heat_5to6

data_6to7 <- combined_correlation %>% 
  filter(metric == '6-7m',
         var1 != 'Banner',
         var2 != 'Banner')
data_6to7$var1 <- factor(data_6to7$var1 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))
data_6to7$var2 <- factor(data_6to7$var2 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))

heat_6to7 <-
  ggplot(data = data_6to7 %>% filter(row_number() >= which(var1 == var2)) , aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient(
    low = 'white',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(0, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=30", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=35", size=6) +
  expand_limits(y=5)+
  ggtitle('Ladder fuels 6-7m')+
  theme(legend.position = 'none')
heat_6to7


data_7to8 <- combined_correlation %>% 
  filter(metric == '7-8m',
         var1 != 'Banner',
         var2 != 'Banner')
data_7to8$var1 <- factor(data_7to8$var1 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))
data_7to8$var2 <- factor(data_7to8$var2 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))

heat_7to8 <-
  ggplot(data = data_7to8 %>% filter(row_number() >= which(var1 == var2)) , aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient(
    low = 'white',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(0, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=30", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=35", size=6) +
  expand_limits(y=5)+
  ggtitle('Ladder fuels 7-8m')
heat_7to8

legend <- as_ggplot(ggpubr::get_legend(heat_7to8))

heat_7to8 <- heat_7to8+ theme(legend.position = 'none')


# ======================== arrange and output 7-8 figure ===========================


heat <-
  ggarrange(
    heat_1to2,
    heat_2to3,
    heat_3to4,
    heat_4to5,
    heat_5to6,
    heat_6to7,
    heat_7to8,
    legend,
    ncol = 2,
    nrow = 4,
    widths = c(6, 6),
    heights = c(6, 6, 6, 6)
  )



heat <- heat + theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))

ggsave(
  outfile,
  plot=heat,
  width = 15,
  height = 26,
  units = 'in',
  dpi = 300 )

# ======================== tukman figure ===========================
data_1to4 <- combined_correlation %>% 
  filter(metric == '1-4m') 
data_1to4$var1 <- factor(data_1to4$var1 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))
data_1to4$var2 <- factor(data_1to4$var2 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))

heat_1to4 <-
  ggplot(data = data_1to4 %>% filter(row_number() >= which(var1 == var2)) , aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient(
    low = 'white',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(0, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=30", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=35", size=6) +
  annotate("text", x = 5, y = 5.75, label = "n=34", size=6) +
  expand_limits(y=6)+
  ggtitle('Ladder fuels 1-4m')+
  theme(legend.position = 'none')
heat_1to4

data_1to8 <- combined_correlation %>% 
  filter(metric == '1-8m',
         var1 != 'Banner',
         var2 != 'Banner')
data_1to8$var1 <- factor(data_1to8$var1 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))
data_1to8$var2 <- factor(data_1to8$var2 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))

heat_1to8 <-
  ggplot(data = data_1to8 %>% filter(row_number() >= which(var1 == var2)) , aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient(
    low = 'white',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(0, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=30", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=35", size=6) +
  expand_limits(y=5)+
  ggtitle('Ladder fuels 1-8m')+
  theme(legend.key.size = unit(2.25, 'cm'),)
heat_1to8

legend <- as_ggplot(ggpubr::get_legend(heat_1to8))

heat_1to8 <- heat_1to8+ theme(legend.position = 'none')

# ======================== arrange and output 1-4 figure ===========================

heat_tuk <-
  ggarrange(
    heat_1to4,
    heat_1to8,
    legend, 
    ncol = 3,
    nrow = 1, 
    widths = c(7, 6, 3),
    heights = c(6)
  )


heat_tuk <- heat_tuk + theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))

ggsave(
  tuk_outfile,
  plot=heat_tuk,
  width = 18,
  height = 8,
  units = 'in',
  dpi = 300 )


# ======================== write method files to csv ==========================
# 
# rbr <- read_csv('D:/Analyses/kincade_glass_fire_tls_plot_centers_sentinel2a_20m_rbr.csv') 
# rbr <- rbr %>%
#   select(Plot, RBR_3x3avg) %>%
#   rename(plot=Plot) 
# 
# rbr$plot <- rbr$plot %>%
#   str_replace( 'p', '') %>%
#   str_replace('P', '')
# 
# rbr <- rbr %>%
#   mutate_at('plot', as.numeric)
# 
# tidy_data_als_filter <- tidy_data_als %>%
#   select(plot, ladder_fuel_1to2, ladder_fuel_1to3, ladder_fuel_1to4, ladder_fuel_7to8) %>% 
#   left_join(rbr, by='plot') %>%
#   filter(!is.na(RBR_3x3avg))
# 
# tidy_data_tls_filter <- tidy_data_tls %>%
#   select(plot, ladder_fuel_1to2, ladder_fuel_1to3, ladder_fuel_1to4, ladder_fuel_7to8)%>% 
#   left_join(rbr, by='plot') %>%
#   filter(!is.na(RBR_3x3avg))
# 
# tidy_data_banner_filter <- tidy_data_banner %>%
#   select(plot, ladder_fuel_1to2, ladder_fuel_1to3, ladder_fuel_1to4)%>% 
#   left_join(rbr, by='plot') %>%
#   filter(!is.na(RBR_3x3avg))
# 
# tidy_data_zeb_filter <- tidy_data_zeb %>%
#   select(plot, ladder_fuel_1to2, ladder_fuel_1to3, ladder_fuel_1to4, ladder_fuel_7to8)%>% 
#   left_join(rbr, by='plot') %>%
#   filter(!is.na(RBR_3x3avg))
# 
# tidy_data_uas_filter <- tidy_data_uas %>%
#   select(plot, ladder_fuel_1to2, ladder_fuel_1to3, ladder_fuel_1to4, ladder_fuel_7to8)%>% 
#   left_join(rbr, by='plot') %>%
#   filter(!is.na(RBR_3x3avg))
# 
# write_csv(tidy_data_als_filter, tidy_als_outfile)
# write_csv(tidy_data_tls_filter, tidy_tls_outfile)
# write_csv(tidy_data_banner_filter, tidy_banner_outfile)
# write_csv(tidy_data_zeb_filter, tidy_zeb_outfile)
# write_csv(tidy_data_uas_filter, tidy_uas_outfile)



