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
tuk_data <- read_csv('D:/Analyses/ladder_fuel_jmp/ladder-fuels_metrics_tls_zeb_als_uas_filtered_210529_tukman.csv')

control_plots = c('1301','1303','1304','1306','1307','1309','1310','1312','1313',
                  '1315','1319','1325','1329','1332','1335','1336','1337','1340',
                  '1342','1349','1851','1852','1853','1854','2','3','5','8','11',
                  '13','15','17','20','24','26')

methods <- c('TLS', 'HMLS', 'UAS','ALS',  'Banner')

cbh <- c('low', 'medium', 'high','very_high')

outfile_1to2 <- 'D:/Analyses/ladder_fuel_jmp/ladder-fuel_heatmap_1to2_210604.png'
outfile_2to3 <- 'D:/Analyses/ladder_fuel_jmp/ladder-fuel_heatmap_2to3_210604.png'
outfile_3to4 <- 'D:/Analyses/ladder_fuel_jmp/ladder-fuel_heatmap_3to4_210604.png'
outfile_4to5 <- 'D:/Analyses/ladder_fuel_jmp/ladder-fuel_heatmap_4to5_210604.png'
outfile_5to6 <- 'D:/Analyses/ladder_fuel_jmp/ladder-fuel_heatmap_5to6_210604.png'
outfile_6to7 <- 'D:/Analyses/ladder_fuel_jmp/ladder-fuel_heatmap_6to7_210604.png'
outfile_7to8 <- 'D:/Analyses/ladder_fuel_jmp/ladder-fuel_heatmap_7to8_210604.png'
tuk_outfile_1to4 <- 'D:/Analyses/ladder_fuel_jmp/ladder-fuel_heatmap_tukman_1to4_210604.png'
tuk_outfile_1to8 <- 'D:/Analyses/ladder_fuel_jmp/ladder-fuel_heatmap_tukman_1to8_210604.png'

# =============================== tidy the data ================================
data <- data %>%
  filter(plot %in% control_plots)

tuk_data <- tuk_data %>%
  filter(plot %in% control_plots)

data <- data %>%
  left_join(tuk_data)

data$cbh_class <-
  cut(
    data$cbh,
    breaks = c( 0,  3, 6, 9, Inf),
    label = c('low', 'medium', 'high', 'very_high')
  )

tidy_data_tls <- data %>%
  dplyr::select(plot, tls_ladder_fuel_1to2,tls_ladder_fuel_2to3, 
                tls_ladder_fuel_3to4,tls_ladder_fuel_4to5,
                tls_ladder_fuel_5to6,tls_ladder_fuel_6to7,
                tls_ladder_fuel_7to8, tls_ladder_fuel_1to4, 
                tls_ladder_fuel_1to8, cbh_class) %>% 
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
                als_ladder_fuel_1to8, cbh_class) %>% 
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
                zeb_ladder_fuel_1to8, cbh_class) %>% 
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
                uas_ladder_fuel_1to8, cbh_class) %>% 
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
                trad_ladder_fuel_3to4, trad_ladder_fuel_1to4, cbh_class) %>%
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
                               metric=as.character(), cbh_class=as.character())



for (c in cbh) {
  
  cbh_data <- tidy_data %>%
    filter(cbh_class == c)
  
  for (i in methods) {
    for (j in methods) {
      
      var1_data <- cbh_data %>%
        filter(method == i) 
      
      var2_data <- cbh_data %>%
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
                     '1-8m'),
          cbh_class = c
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
          ),
          cbh_class = c
        )
      }
      
      combined_correlation <- combined_correlation %>%
        add_row(correlation)
    }
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


data_1to2_low <- combined_correlation %>% 
  filter(metric == '1-2m') %>%
  filter(cbh_class == 'low')
data_1to2_low$var1 <- factor(data_1to2_low$var1 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))
data_1to2_low$var2 <- factor(data_1to2_low$var2 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))

heat_1to2_low <-
  ggplot(data = data_1to2_low %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=8", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=8", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=8", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=8", size=6) +
  annotate("text", x = 5, y = 5.75, label = "n=8", size=6) +
  expand_limits(y=6)+
  ggtitle('Ladder fuels 1-2m (low CBH)')+
  theme(legend.position = 'none')
heat_1to2_low



data_1to2_medium <- combined_correlation %>% 
  filter(metric == '1-2m') %>%
  filter(cbh_class == 'medium') 
data_1to2_medium$var1 <- factor(data_1to2_medium$var1 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))
data_1to2_medium$var2 <- factor(data_1to2_medium$var2 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))

heat_1to2_medium <-
  ggplot(data = data_1to2_medium %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=12", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=12", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=8", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=12", size=6) +
  annotate("text", x = 5, y = 5.75, label = "n=11", size=6) +
  expand_limits(y=6)+
  ggtitle('Ladder fuels 1-2m (medium CBH)')+
  theme(legend.position = 'none')
heat_1to2_medium

data_1to2_high <- combined_correlation %>% 
  filter(metric == '1-2m') %>%
  filter(cbh_class == 'high')
data_1to2_high$var1 <- factor(data_1to2_high$var1 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))
data_1to2_high$var2 <- factor(data_1to2_high$var2 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))

heat_1to2_high <-
  ggplot(data = data_1to2_high %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=7", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=7", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=6", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=7", size=6) +
  annotate("text", x = 5, y = 5.75, label = "n=7", size=6) +
  expand_limits(y=6)+
  ggtitle('Ladder fuels 1-2m (high CBH)')+
  theme(legend.position = 'none')
heat_1to2_high

data_1to2_very_high <- combined_correlation %>% 
  filter(metric == '1-2m') %>%
  filter(cbh_class == 'very_high')%>%
  filter(var1!='UAS') %>%
  filter(var2!='UAS')
data_1to2_very_high$var1 <- factor(data_1to2_very_high$var1 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))
data_1to2_very_high$var2 <- factor(data_1to2_very_high$var2 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))

heat_1to2_very_high <-
  ggplot(data = data_1to2_very_high %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=5", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=4", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=5", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=5", size=6) +
  expand_limits(y=5)+
  ggtitle('Ladder fuels 1-2m (very high CBH)')
heat_1to2_very_high

legend <- as_ggplot(ggpubr::get_legend(heat_1to2_very_high))

heat_1to2_very_high <- heat_1to2_very_high+ theme(legend.position = 'none')

# ======================== arrange and output 1-2 figure ===========================


heat_1to2 <-
  ggarrange(
    heat_1to2_low,
    heat_1to2_medium,
    heat_1to2_high,
    heat_1to2_very_high, 
    ncol = 2,
    nrow = 2,
    widths = c(6, 6),
    heights = c(6, 6)
  )

heat_1to2_legend <-
  ggarrange(
    heat_1to2,
    legend,
    ncol = 2,
    nrow = 1,
    widths = c(12, 2),
    heights = c(12)
  )


heat_1to2_legend <- heat_1to2_legend + theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))

ggsave(
  outfile_1to2,
  plot=heat_1to2_legend,
  width = 20,
  height = 15,
  units = 'in',
  dpi = 300 )

# ======================== 2-3 figure ===========================
data_2to3_low <- combined_correlation %>% 
  filter(metric == '2-3m') %>%
  filter(cbh_class == 'low')
data_2to3_low$var1 <- factor(data_2to3_low$var1 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))
data_2to3_low$var2 <- factor(data_2to3_low$var2 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))

heat_2to3_low <-
  ggplot(data = data_2to3_low %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=30", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=35", size=6) +
  annotate("text", x = 5, y = 5.75, label = "n=34", size=6) +
  expand_limits(y=6)+
  ggtitle('Ladder fuels 2-3m (low CBH)')+
  theme(legend.position = 'none')
heat_2to3_low

data_2to3_medium <- combined_correlation %>% 
  filter(metric == '2-3m') %>%
  filter(cbh_class == 'medium') 
data_2to3_medium$var1 <- factor(data_2to3_medium$var1 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))
data_2to3_medium$var2 <- factor(data_2to3_medium$var2 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))

heat_2to3_medium <-
  ggplot(data = data_2to3_medium %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=30", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=35", size=6) +
  annotate("text", x = 5, y = 5.75, label = "n=34", size=6) +
  expand_limits(y=6)+
  ggtitle('Ladder fuels 2-3m (medium CBH)')+
  theme(legend.position = 'none')
heat_2to3_medium

data_2to3_high <- combined_correlation %>% 
  filter(metric == '2-3m') %>%
  filter(cbh_class == 'high')
data_2to3_high$var1 <- factor(data_2to3_high$var1 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))
data_2to3_high$var2 <- factor(data_2to3_high$var2 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))

heat_2to3_high <-
  ggplot(data = data_2to3_high %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=30", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=35", size=6) +
  annotate("text", x = 5, y = 5.75, label = "n=34", size=6) +
  expand_limits(y=6)+
  ggtitle('Ladder fuels 2-3m (high CBH)')+
  theme(legend.position = 'none')
heat_2to3_high

data_2to3_very_high <- combined_correlation %>% 
  filter(metric == '2-3m') %>%
  filter(cbh_class == 'very_high')%>%
  filter(var1!='UAS') %>%
  filter(var2!='UAS')
data_2to3_very_high$var1 <- factor(data_2to3_very_high$var1 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))
data_2to3_very_high$var2 <- factor(data_2to3_very_high$var2 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))

heat_2to3_very_high <-
  ggplot(data = data_2to3_very_high %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=35", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=34", size=6) +
  expand_limits(y=5)+
  ggtitle('Ladder fuels 2-3m (very high CBH)')
heat_2to3_very_high

legend <- as_ggplot(ggpubr::get_legend(heat_2to3_very_high))

heat_2to3_very_high <- heat_2to3_very_high+ theme(legend.position = 'none')

# ======================== arrange and output 2-3 figure ===========================


heat_2to3 <-
  ggarrange(
    heat_2to3_low,
    heat_2to3_medium,
    heat_2to3_high,
    heat_2to3_very_high, 
    ncol = 2,
    nrow = 2,
    widths = c(6, 6),
    heights = c(6, 6)
  )

heat_2to3_legend <-
  ggarrange(
    heat_2to3,
    legend,
    ncol = 2,
    nrow = 1,
    widths = c(12, 2),
    heights = c(12)
  )


heat_2to3_legend <- heat_2to3_legend + theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))

ggsave(
  outfile_2to3,
  plot=heat_2to3_legend,
  width = 20,
  height = 15,
  units = 'in',
  dpi = 300 )

# ======================== 3-4 figure ===========================
data_3to4_low <- combined_correlation %>% 
  filter(metric == '3-4m') %>%
  filter(cbh_class == 'low')
data_3to4_low$var1 <- factor(data_3to4_low$var1 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))
data_3to4_low$var2 <- factor(data_3to4_low$var2 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))

heat_3to4_low <-
  ggplot(data = data_3to4_low %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=30", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=35", size=6) +
  annotate("text", x = 5, y = 5.75, label = "n=34", size=6) +
  expand_limits(y=6)+
  ggtitle('Ladder fuels 3-4m (low CBH)')+
  theme(legend.position = 'none')
heat_3to4_low

data_3to4_medium <- combined_correlation %>% 
  filter(metric == '3-4m') %>%
  filter(cbh_class == 'medium') 
data_3to4_medium$var1 <- factor(data_3to4_medium$var1 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))
data_3to4_medium$var2 <- factor(data_3to4_medium$var2 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))

heat_3to4_medium <-
  ggplot(data = data_3to4_medium %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=30", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=35", size=6) +
  annotate("text", x = 5, y = 5.75, label = "n=34", size=6) +
  expand_limits(y=6)+
  ggtitle('Ladder fuels 3-4m (medium CBH)')+
  theme(legend.position = 'none')
heat_3to4_medium

data_3to4_high <- combined_correlation %>% 
  filter(metric == '3-4m') %>%
  filter(cbh_class == 'high')
data_3to4_high$var1 <- factor(data_3to4_high$var1 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))
data_3to4_high$var2 <- factor(data_3to4_high$var2 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))

heat_3to4_high <-
  ggplot(data = data_3to4_high %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=30", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=35", size=6) +
  annotate("text", x = 5, y = 5.75, label = "n=34", size=6) +
  expand_limits(y=6)+
  ggtitle('Ladder fuels 3-4m (high CBH)')+
  theme(legend.position = 'none')
heat_3to4_high

data_3to4_very_high <- combined_correlation %>% 
  filter(metric == '3-4m') %>%
  filter(cbh_class == 'very_high')%>%
  filter(var1!='UAS') %>%
  filter(var2!='UAS')
data_3to4_very_high$var1 <- factor(data_3to4_very_high$var1 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))
data_3to4_very_high$var2 <- factor(data_3to4_very_high$var2 , levels = c('TLS', "HMLS", "UAS","ALS", 'Banner'))

heat_3to4_very_high <-
  ggplot(data = data_3to4_very_high %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=35", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=34", size=6) +
  expand_limits(y=5)+
  ggtitle('Ladder fuels 3-4m (very high CBH)')
heat_3to4_very_high

legend <- as_ggplot(ggpubr::get_legend(heat_3to4_very_high))

heat_3to4_very_high <- heat_3to4_very_high+ theme(legend.position = 'none')

# ======================== arrange and output 3-4 figure ===========================


heat_3to4 <-
  ggarrange(
    heat_3to4_low,
    heat_3to4_medium,
    heat_3to4_high,
    heat_3to4_very_high, 
    ncol = 2,
    nrow = 2,
    widths = c(6, 6),
    heights = c(6, 6)
  )

heat_3to4_legend <-
  ggarrange(
    heat_3to4,
    legend,
    ncol = 2,
    nrow = 1,
    widths = c(12, 2),
    heights = c(12)
  )


heat_3to4_legend <- heat_3to4_legend + theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))

ggsave(
  outfile_3to4,
  plot=heat_3to4_legend,
  width = 20,
  height = 15,
  units = 'in',
  dpi = 300 )
# ======================== 4-5 figure ===========================
data_4to5_low <- combined_correlation %>% 
  filter(metric == '4-5m') %>%
  filter(cbh_class == 'low') %>%
  filter(var1!='Banner')%>%
  filter(var2!='Banner')
data_4to5_low$var1 <- factor(data_4to5_low$var1 , levels = c('TLS', "HMLS", "UAS","ALS"))
data_4to5_low$var2 <- factor(data_4to5_low$var2 , levels = c('TLS', "HMLS", "UAS","ALS"))

heat_4to5_low <-
  ggplot(data = data_4to5_low %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=30", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=35", size=6) +
  expand_limits(y=5)+
  ggtitle('Ladder fuels 4-5m (low CBH)')+
  theme(legend.position = 'none')
heat_4to5_low

data_4to5_medium <- combined_correlation %>% 
  filter(var1!='Banner')%>%
  filter(var2!='Banner')%>%
  filter(metric == '4-5m') %>%
  filter(cbh_class == 'medium')
data_4to5_medium$var1 <- factor(data_4to5_medium$var1 , levels = c('TLS', "HMLS", "UAS","ALS"))
data_4to5_medium$var2 <- factor(data_4to5_medium$var2 , levels = c('TLS', "HMLS", "UAS","ALS"))

heat_4to5_medium <-
  ggplot(data = data_4to5_medium %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=30", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=35", size=6) +
  expand_limits(y=5)+
  ggtitle('Ladder fuels 4-5m (medium CBH)')+
  theme(legend.position = 'none')
heat_4to5_medium

data_4to5_high <- combined_correlation %>% 
  filter(metric == '4-5m') %>%
  filter(cbh_class == 'high') %>%
  filter(var1!='Banner')%>%
  filter(var2!='Banner')
data_4to5_high$var1 <- factor(data_4to5_high$var1 , levels = c('TLS', "HMLS", "UAS","ALS"))
data_4to5_high$var2 <- factor(data_4to5_high$var2 , levels = c('TLS', "HMLS", "UAS","ALS"))

heat_4to5_high <-
  ggplot(data = data_4to5_high %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=30", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=35", size=6) +
  expand_limits(y=5)+
  ggtitle('Ladder fuels 4-5m (high CBH)')+
  theme(legend.position = 'none')
heat_4to5_high

data_4to5_very_high <- combined_correlation %>% 
  filter(metric == '4-5m') %>%
  filter(cbh_class == 'very_high') %>%
  filter(var1!='Banner')%>%
  filter(var2!='Banner')%>%
  filter(var1!='UAS')%>%
  filter(var2!='UAS')
data_4to5_very_high$var1 <- factor(data_4to5_very_high$var1 , levels = c('TLS', "HMLS","ALS"))
data_4to5_very_high$var2 <- factor(data_4to5_very_high$var2 , levels = c('TLS', "HMLS","ALS"))

heat_4to5_very_high <-
  ggplot(data = data_4to5_very_high %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=35", size=6) +
  expand_limits(y=4)+
  ggtitle('Ladder fuels 4-5m (very high CBH)')
heat_4to5_very_high

legend <- as_ggplot(ggpubr::get_legend(heat_4to5_very_high))

heat_4to5_very_high <- heat_4to5_very_high+ theme(legend.position = 'none')

# ======================== arrange and output 4-5 figure ===========================


heat_4to5 <-
  ggarrange(
    heat_4to5_low,
    heat_4to5_medium,
    heat_4to5_high,
    heat_4to5_very_high, 
    ncol = 2,
    nrow = 2,
    widths = c(6, 6),
    heights = c(6, 6)
  )

heat_4to5_legend <-
  ggarrange(
    heat_4to5,
    legend,
    ncol = 2,
    nrow = 1,
    widths = c(12, 2),
    heights = c(12)
  )


heat_4to5_legend <- heat_4to5_legend + theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))

ggsave(
  outfile_4to5,
  plot=heat_4to5_legend,
  width = 20,
  height = 15,
  units = 'in',
  dpi = 300 )
# ======================== 5-6 figure ===========================
data_5to6_low <- combined_correlation %>% 
  filter(metric == '5-6m') %>%
  filter(cbh_class == 'low') %>%
  filter(var1!='Banner')%>%
  filter(var2!='Banner')
data_5to6_low$var1 <- factor(data_5to6_low$var1 , levels = c('TLS', "HMLS", "UAS","ALS"))
data_5to6_low$var2 <- factor(data_5to6_low$var2 , levels = c('TLS', "HMLS", "UAS","ALS"))

heat_5to6_low <-
  ggplot(data = data_5to6_low %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=30", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=35", size=6) +
  expand_limits(y=5)+
  ggtitle('Ladder fuels 5-6m (low CBH)')+
  theme(legend.position = 'none')
heat_5to6_low

data_5to6_medium <- combined_correlation %>% 
  filter(var1!='Banner')%>%
  filter(var2!='Banner')%>%
  filter(metric == '5-6m') %>%
  filter(cbh_class == 'medium')
data_5to6_medium$var1 <- factor(data_5to6_medium$var1 , levels = c('TLS', "HMLS", "UAS","ALS"))
data_5to6_medium$var2 <- factor(data_5to6_medium$var2 , levels = c('TLS', "HMLS", "UAS","ALS"))

heat_5to6_medium <-
  ggplot(data = data_5to6_medium %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=30", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=35", size=6) +
  expand_limits(y=5)+
  ggtitle('Ladder fuels 5-6m (medium CBH)')+
  theme(legend.position = 'none')
heat_5to6_medium

data_5to6_high <- combined_correlation %>% 
  filter(metric == '5-6m') %>%
  filter(cbh_class == 'high') %>%
  filter(var1!='Banner')%>%
  filter(var2!='Banner')
data_5to6_high$var1 <- factor(data_5to6_high$var1 , levels = c('TLS', "HMLS", "UAS","ALS"))
data_5to6_high$var2 <- factor(data_5to6_high$var2 , levels = c('TLS', "HMLS", "UAS","ALS"))

heat_5to6_high <-
  ggplot(data = data_5to6_high %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=30", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=35", size=6) +
  expand_limits(y=5)+
  ggtitle('Ladder fuels 5-6m (high CBH)')+
  theme(legend.position = 'none')
heat_5to6_high

data_5to6_very_high <- combined_correlation %>% 
  filter(metric == '5-6m') %>%
  filter(cbh_class == 'very_high') %>%
  filter(var1!='Banner')%>%
  filter(var2!='Banner') 
data_5to6_very_high$var1 <- factor(data_5to6_very_high$var1 , levels = c('TLS', "HMLS",'UAS',"ALS"))
data_5to6_very_high$var2 <- factor(data_5to6_very_high$var2 , levels = c('TLS', "HMLS",'UAS',"ALS"))


heat_5to6_very_high <-
  ggplot(data = data_5to6_very_high %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=35", size=6) +
  expand_limits(y=4)+
  ggtitle('Ladder fuels 5-6m (very high CBH)')
heat_5to6_very_high

legend <- as_ggplot(ggpubr::get_legend(heat_5to6_very_high))

heat_5to6_very_high <- heat_5to6_very_high+ theme(legend.position = 'none')

# ======================== arrange and output 5-6 figure ===========================


heat_5to6 <-
  ggarrange(
    heat_5to6_low,
    heat_5to6_medium,
    heat_5to6_high,
    heat_5to6_very_high, 
    ncol = 2,
    nrow = 2,
    widths = c(6, 6),
    heights = c(6, 6)
  )

heat_5to6_legend <-
  ggarrange(
    heat_5to6,
    legend,
    ncol = 2,
    nrow = 1,
    widths = c(12, 2),
    heights = c(12)
  )


heat_5to6_legend <- heat_5to6_legend + theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))

ggsave(
  outfile_5to6,
  plot=heat_5to6_legend,
  width = 20,
  height = 15,
  units = 'in',
  dpi = 300 )
# ======================== 6-7 figure ===========================
data_6to7_low <- combined_correlation %>% 
  filter(metric == '6-7m') %>%
  filter(cbh_class == 'low') %>%
  filter(var1!='Banner')%>%
  filter(var2!='Banner')
data_6to7_low$var1 <- factor(data_6to7_low$var1 , levels = c('TLS', "HMLS", "UAS","ALS"))
data_6to7_low$var2 <- factor(data_6to7_low$var2 , levels = c('TLS', "HMLS", "UAS","ALS"))

heat_6to7_low <-
  ggplot(data = data_6to7_low %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=30", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=35", size=6) +
  expand_limits(y=5)+
  ggtitle('Ladder fuels 6-7m (low CBH)')+
  theme(legend.position = 'none')
heat_6to7_low

data_6to7_medium <- combined_correlation %>% 
  filter(var1!='Banner')%>%
  filter(var2!='Banner')%>%
  filter(metric == '6-7m') %>%
  filter(cbh_class == 'medium')
data_6to7_medium$var1 <- factor(data_6to7_medium$var1 , levels = c('TLS', "HMLS", "UAS","ALS"))
data_6to7_medium$var2 <- factor(data_6to7_medium$var2 , levels = c('TLS', "HMLS", "UAS","ALS"))

heat_6to7_medium <-
  ggplot(data = data_6to7_medium %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=30", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=35", size=6) +
  expand_limits(y=5)+
  ggtitle('Ladder fuels 6-7m (medium CBH)')+
  theme(legend.position = 'none')
heat_6to7_medium

data_6to7_high <- combined_correlation %>% 
  filter(metric == '6-7m') %>%
  filter(cbh_class == 'high') %>%
  filter(var1!='Banner')%>%
  filter(var2!='Banner')
data_6to7_high$var1 <- factor(data_6to7_high$var1 , levels = c('TLS', "HMLS", "UAS","ALS"))
data_6to7_high$var2 <- factor(data_6to7_high$var2 , levels = c('TLS', "HMLS", "UAS","ALS"))

heat_6to7_high <-
  ggplot(data = data_6to7_high %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=30", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=35", size=6) +
  expand_limits(y=5)+
  ggtitle('Ladder fuels 6-7m (high CBH)')+
  theme(legend.position = 'none')
heat_6to7_high

data_6to7_very_high <- combined_correlation %>% 
  filter(metric == '6-7m') %>%
  filter(cbh_class == 'very_high') %>%
  filter(var1!='Banner')%>%
  filter(var2!='Banner') 
data_6to7_very_high$var1 <- factor(data_6to7_very_high$var1 , levels = c('TLS', "HMLS",'UAS',"ALS"))
data_6to7_very_high$var2 <- factor(data_6to7_very_high$var2 , levels = c('TLS', "HMLS",'UAS',"ALS"))


heat_6to7_very_high <-
  ggplot(data = data_6to7_very_high %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=35", size=6) +
  expand_limits(y=4)+
  ggtitle('Ladder fuels 6-7m (very high CBH)')
heat_6to7_very_high

legend <- as_ggplot(ggpubr::get_legend(heat_6to7_very_high))

heat_6to7_very_high <- heat_6to7_very_high+ theme(legend.position = 'none')

# ======================== arrange and output 6-7 figure ===========================


heat_6to7 <-
  ggarrange(
    heat_6to7_low,
    heat_6to7_medium,
    heat_6to7_high,
    heat_6to7_very_high, 
    ncol = 2,
    nrow = 2,
    widths = c(6, 6),
    heights = c(6, 6)
  )

heat_6to7_legend <-
  ggarrange(
    heat_6to7,
    legend,
    ncol = 2,
    nrow = 1,
    widths = c(12, 2),
    heights = c(12)
  )


heat_6to7_legend <- heat_6to7_legend + theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))

ggsave(
  outfile_6to7,
  plot=heat_6to7_legend,
  width = 20,
  height = 15,
  units = 'in',
  dpi = 300 )
# ======================== 6-7 figure ===========================
data_7to8_low <- combined_correlation %>% 
  filter(metric == '7-8m') %>%
  filter(cbh_class == 'low') %>%
  filter(var1!='Banner')%>%
  filter(var2!='Banner')
data_7to8_low$var1 <- factor(data_7to8_low$var1 , levels = c('TLS', "HMLS", "UAS","ALS"))
data_7to8_low$var2 <- factor(data_7to8_low$var2 , levels = c('TLS', "HMLS", "UAS","ALS"))

heat_7to8_low <-
  ggplot(data = data_7to8_low %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=30", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=35", size=6) +
  expand_limits(y=5)+
  ggtitle('Ladder fuels 7-8m (low CBH)')+
  theme(legend.position = 'none')
heat_7to8_low

data_7to8_medium <- combined_correlation %>% 
  filter(var1!='Banner')%>%
  filter(var2!='Banner')%>%
  filter(metric == '7-8m') %>%
  filter(cbh_class == 'medium')
data_7to8_medium$var1 <- factor(data_7to8_medium$var1 , levels = c('TLS', "HMLS", "UAS","ALS"))
data_7to8_medium$var2 <- factor(data_7to8_medium$var2 , levels = c('TLS', "HMLS", "UAS","ALS"))

heat_7to8_medium <-
  ggplot(data = data_7to8_medium %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=30", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=35", size=6) +
  expand_limits(y=5)+
  ggtitle('Ladder fuels 7-8m (medium CBH)')+
  theme(legend.position = 'none')
heat_7to8_medium

data_7to8_high <- combined_correlation %>% 
  filter(metric == '7-8m') %>%
  filter(cbh_class == 'high') %>%
  filter(var1!='Banner')%>%
  filter(var2!='Banner')
data_7to8_high$var1 <- factor(data_7to8_high$var1 , levels = c('TLS', "HMLS", "UAS","ALS"))
data_7to8_high$var2 <- factor(data_7to8_high$var2 , levels = c('TLS', "HMLS", "UAS","ALS"))

heat_7to8_high <-
  ggplot(data = data_7to8_high %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=30", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=35", size=6) +
  expand_limits(y=5)+
  ggtitle('Ladder fuels 7-8m (high CBH)')+
  theme(legend.position = 'none')
heat_7to8_high

data_7to8_very_high <- combined_correlation %>% 
  filter(metric == '7-8m') %>%
  filter(cbh_class == 'very_high') %>%
  filter(var1!='Banner')%>%
  filter(var2!='Banner') 
data_7to8_very_high$var1 <- factor(data_7to8_very_high$var1 , levels = c('TLS', "HMLS",'UAS',"ALS"))
data_7to8_very_high$var2 <- factor(data_7to8_very_high$var2 , levels = c('TLS', "HMLS",'UAS',"ALS"))


heat_7to8_very_high <-
  ggplot(data = data_7to8_very_high %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=35", size=6) +
  expand_limits(y=4)+
  ggtitle('Ladder fuels 7-8m (very high CBH)')
heat_7to8_very_high

legend <- as_ggplot(ggpubr::get_legend(heat_7to8_very_high))

heat_7to8_very_high <- heat_7to8_very_high+ theme(legend.position = 'none')

# ======================== arrange and output 7-8 figure ===========================


heat_7to8 <-
  ggarrange(
    heat_7to8_low,
    heat_7to8_medium,
    heat_7to8_high,
    heat_7to8_very_high, 
    ncol = 2,
    nrow = 2,
    widths = c(6, 6),
    heights = c(6, 6)
  )

heat_7to8_legend <-
  ggarrange(
    heat_7to8,
    legend,
    ncol = 2,
    nrow = 1,
    widths = c(12, 2),
    heights = c(12)
  )


heat_7to8_legend <- heat_7to8_legend + theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))

ggsave(
  outfile_7to8,
  plot=heat_7to8_legend,
  width = 20,
  height = 15,
  units = 'in',
  dpi = 300 )

# ======================== tukman figure 1-4 ===========================
data_1to4_low <- combined_correlation %>% 
  filter(metric == '1-4m') %>%
  filter(cbh_class == 'low') 
data_1to4_low$var1 <- factor(data_1to4_low$var1 , levels = c('TLS', "HMLS", "UAS","ALS", "Banner"))
data_1to4_low$var2 <- factor(data_1to4_low$var2 , levels = c('TLS', "HMLS", "UAS","ALS", "Banner"))

heat_1to4_low <-
  ggplot(data = data_1to4_low %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=8", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=8", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=8", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=8", size=6) +
  annotate("text", x = 5, y = 5.75, label = "n=8", size=6) +
  expand_limits(y=6)+
  ggtitle('Ladder fuels 1-4m (low CBH)')+
  theme(legend.position = 'none')
heat_1to4_low

data_1to4_medium <- combined_correlation %>% 
  filter(metric == '1-4m') %>%
  filter(cbh_class == 'medium')
data_1to4_medium$var1 <- factor(data_1to4_medium$var1 , levels = c('TLS', "HMLS", "UAS","ALS", "Banner"))
data_1to4_medium$var2 <- factor(data_1to4_medium$var2 , levels = c('TLS', "HMLS", "UAS","ALS", "Banner"))

heat_1to4_medium <-
  ggplot(data = data_1to4_medium %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=12", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=12", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=8", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=12", size=6) +
  annotate("text", x = 5, y = 5.75, label = "n=11", size=6) +
  expand_limits(y=6)+
  ggtitle('Ladder fuels 1-4m (medium CBH)')+
  theme(legend.position = 'none')
heat_1to4_medium

data_1to4_high <- combined_correlation %>% 
  filter(metric == '1-4m') %>%
  filter(cbh_class == 'high')
data_1to4_high$var1 <- factor(data_1to4_high$var1 , levels = c('TLS', "HMLS", "UAS","ALS", "Banner"))
data_1to4_high$var2 <- factor(data_1to4_high$var2 , levels = c('TLS', "HMLS", "UAS","ALS", "Banner"))

heat_1to4_high <-
  ggplot(data = data_1to4_high %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=7", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=7", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=6", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=7", size=6) +
  annotate("text", x = 5, y = 5.75, label = "n=7", size=6) +
  expand_limits(y=6)+
  ggtitle('Ladder fuels 1-4m (high CBH)')+
  theme(legend.position = 'none')
heat_1to4_high

data_1to4_very_high <- combined_correlation %>% 
  filter(metric == '1-4m') %>%
  filter(cbh_class == 'very_high') %>%
  filter(!var1 == 'UAS')%>%
  filter(!var2 == 'UAS')
data_1to4_very_high$var1 <- factor(data_1to4_very_high$var1 , levels = c('TLS', "HMLS","ALS", "Banner"))
data_1to4_very_high$var2 <- factor(data_1to4_very_high$var2 , levels = c('TLS', "HMLS","ALS", "Banner"))


heat_1to4_very_high <-
  ggplot(data = data_1to4_very_high %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=5", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=4", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=5", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=5", size=6) +
  expand_limits(y=5)+
  ggtitle('Ladder fuels 1-4m (very high CBH)')
heat_1to4_very_high

legend <- as_ggplot(ggpubr::get_legend(heat_1to4_very_high))

heat_1to4_very_high <- heat_1to4_very_high+ theme(legend.position = 'none')

# ======================== arrange and output 1-4 figure ===========================


heat_1to4 <-
  ggarrange(
    heat_1to4_low,
    heat_1to4_medium,
    heat_1to4_high,
    heat_1to4_very_high, 
    ncol = 2,
    nrow = 2,
    widths = c(6, 6),
    heights = c(6, 6)
  )

heat_1to4_legend <-
  ggarrange(
    heat_1to4,
    legend,
    ncol = 2,
    nrow = 1,
    widths = c(12, 2),
    heights = c(12)
  )


heat_1to4_legend <- heat_1to4_legend + theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))

ggsave(
  tuk_outfile_1to4,
  plot=heat_1to4_legend,
  width = 20,
  height = 15,
  units = 'in',
  dpi = 300 )

# ======================== tukman figure 1-8 ===========================
# ======================== 6-7 figure ===========================
data_1to8_low <- combined_correlation %>% 
  filter(metric == '1-8m') %>%
  filter(cbh_class == 'low') %>%
  filter(var1!='Banner')%>%
  filter(var2!='Banner')
data_1to8_low$var1 <- factor(data_1to8_low$var1 , levels = c('TLS', "HMLS", "UAS","ALS"))
data_1to8_low$var2 <- factor(data_1to8_low$var2 , levels = c('TLS', "HMLS", "UAS","ALS"))

heat_1to8_low <-
  ggplot(data = data_1to8_low %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=30", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=35", size=6) +
  expand_limits(y=5)+
  ggtitle('Ladder fuels 1-8m (low CBH)')+
  theme(legend.position = 'none')
heat_1to8_low

data_1to8_medium <- combined_correlation %>% 
  filter(var1!='Banner')%>%
  filter(var2!='Banner')%>%
  filter(metric == '1-8m') %>%
  filter(cbh_class == 'medium')
data_1to8_medium$var1 <- factor(data_1to8_medium$var1 , levels = c('TLS', "HMLS", "UAS","ALS"))
data_1to8_medium$var2 <- factor(data_1to8_medium$var2 , levels = c('TLS', "HMLS", "UAS","ALS"))

heat_1to8_medium <-
  ggplot(data = data_1to8_medium %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=30", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=35", size=6) +
  expand_limits(y=5)+
  ggtitle('Ladder fuels 1-8m (medium CBH)')+
  theme(legend.position = 'none')
heat_1to8_medium

data_1to8_high <- combined_correlation %>% 
  filter(metric == '1-8m') %>%
  filter(cbh_class == 'high') %>%
  filter(var1!='Banner')%>%
  filter(var2!='Banner')
data_1to8_high$var1 <- factor(data_1to8_high$var1 , levels = c('TLS', "HMLS", "UAS","ALS"))
data_1to8_high$var2 <- factor(data_1to8_high$var2 , levels = c('TLS', "HMLS", "UAS","ALS"))

heat_1to8_high <-
  ggplot(data = data_1to8_high %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=30", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=35", size=6) +
  expand_limits(y=5)+
  ggtitle('Ladder fuels 1-8m (high CBH)')+
  theme(legend.position = 'none')
heat_1to8_high

data_1to8_very_high <- combined_correlation %>% 
  filter(metric == '1-8m') %>%
  filter(cbh_class == 'very_high') %>%
  filter(var1!='Banner')%>%
  filter(var2!='Banner') 
data_1to8_very_high$var1 <- factor(data_1to8_very_high$var1 , levels = c('TLS', "HMLS",'UAS',"ALS"))
data_1to8_very_high$var2 <- factor(data_1to8_very_high$var2 , levels = c('TLS', "HMLS",'UAS',"ALS"))


heat_1to8_very_high <-
  ggplot(data = data_1to8_very_high %>%filter(row_number() >= which(var1 == var2)), aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = corr_stars), size = 7) +
  scale_fill_gradient2(
    low = 'yellow2',
    mid = 'orange2',
    high = 'firebrick4',
    space = "Lab",
    na.value = 'grey80',
    limit = c(-1, 1),
    name = "Pearson\nCorrelation"
  ) +
  labs(x='', y='') +
  annotate("text", x = 1, y = 1.75, label = "n=35", size=6) +
  annotate("text", x = 2, y = 2.75, label = "n=34", size=6) +
  annotate("text", x = 3, y = 3.75, label = "n=30", size=6) +
  annotate("text", x = 4, y = 4.75, label = "n=35", size=6) +
  expand_limits(y=5)+
  ggtitle('Ladder fuels 1-8m (very high CBH)')
heat_1to8_very_high

legend <- as_ggplot(ggpubr::get_legend(heat_1to8_very_high))

heat_1to8_very_high <- heat_1to8_very_high+ theme(legend.position = 'none')

# ======================== arrange and output 1-8 figure ===========================


heat_1to8 <-
  ggarrange(
    heat_1to8_low,
    heat_1to8_medium,
    heat_1to8_high,
    heat_1to8_very_high, 
    ncol = 2,
    nrow = 2,
    widths = c(6, 6),
    heights = c(6, 6)
  )

heat_1to8_legend <-
  ggarrange(
    heat_1to8,
    legend,
    ncol = 2,
    nrow = 1,
    widths = c(12, 2),
    heights = c(12)
  )


heat_1to8_legend <- heat_1to8_legend + theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))

ggsave(
  tuk_outfile_1to8,
  plot=heat_1to8_legend,
  width = 20,
  height = 15,
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



