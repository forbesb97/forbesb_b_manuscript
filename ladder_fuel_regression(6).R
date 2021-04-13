# ==============================================================================
#
# Regression for ladder fuel metrics from trad, ALS, UAV, ZEB, and banner
#
# ==============================================================================
#
# Author: Brieanne Forbes
#
# Created: 21 Jan 2021 
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
library(FSA)
library(ggpubr)
library(RColorBrewer)
library(Metrics)
library(gridExtra)


# ================================= User inputs ================================

# rbr_files <- list.files('D:/Analyses/ladder_fuel_full_process_take4/not_filtered/',
#                         pattern = '210331_RBR_3x3avg_rf_predict.csv',
#                         full.names = T)

rbr_files <- list.files('D:/Analyses/ladder_fuel_full_process_take4/filtered_zero_and_above/',
                        pattern = '210331_RBR_3x3avg_rf_predict.csv',
                        full.names = T)
# 
# wrap_out <- 'D:/Analyses/ladder_fuel_full_process_take4/not_filtered/ladder-fuel_linear-regression_210405.png'
wrap_out <- 'D:/Analyses/ladder_fuel_full_process_take4/filtered_zero_and_above/ladder-fuel_linear-regression_filtered_210408.png'

#presentation_out <- '~/desktop/Analyses/output/presentation_figure_210122.png'


# =============================== Combine data ================================

als <- read_csv(rbr_files[1]) %>%
  add_column(method='ALS')
banner <- read_csv(rbr_files[2])%>%
  add_column(method='Banner')
zeb <- read_csv(rbr_files[3])%>%
  add_column(method='HMLS')
tls <- read_csv(rbr_files[4])%>%
  add_column(method='TLS')
uas <- read_csv(rbr_files[5])%>%
  add_column(method='UAS')


rbr_actual <- als %>%
  select(plot, rbr_actual) %>%
  add_column(method='Actual') %>%
  rename(rbr=rbr_actual)



tidy_data <- als %>%
  add_row(banner) %>%
  add_row(tls) %>%
  add_row(uas) %>%
  add_row(zeb) %>%
  select(plot, method, rbr_predict) %>%
  rename(rbr=rbr_predict) %>%
  add_row(rbr_actual)

tls_rbr <- tls %>%
  select(plot,rbr_actual, rbr_predict) %>%
  rename(rbr_predict_tls = rbr_predict)

uas_rbr <- uas %>%
  select(plot,rbr_actual, rbr_predict) %>%
  rename(rbr_predict_uas = rbr_predict)

zeb_rbr <- zeb %>%
  select(plot,rbr_actual, rbr_predict) %>%
  rename(rbr_predict_zeb = rbr_predict)

banner_rbr <- banner %>%
  select(plot,rbr_actual, rbr_predict) %>%
  rename(rbr_predict_banner = rbr_predict)

rbr <- als %>%
  select(plot,rbr_actual, rbr_predict) %>%
  rename(rbr_predict_als = rbr_predict) %>%
  full_join(tls_rbr, by=c('plot', 'rbr_actual')) %>%
  full_join(uas_rbr, by=c('plot', 'rbr_actual')) %>%
  full_join(zeb_rbr, by=c('plot', 'rbr_actual')) %>%
  full_join(banner_rbr, by=c('plot', 'rbr_actual')) 

rbr$rbr_class <-
  cut(
    rbr$rbr_actual,
    breaks = c( 0,  35, 130, 298, Inf),
    label = c('NC', 'Low', 'Medium', 'High')
  )

#===============================Set theme=======================================
theme_set(
  theme(
    text = element_text(family = 'serif', face = 'plain'),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 20),
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

rbr$rbr_class <- factor(rbr$rbr_class, levels = c("High", "Medium", "Low", "NC"))

#=========================3 min Competition Figure===============================

# presentation_figure <- ggplot(data = rbr, aes(y=rbr_actual)) +
#   geom_abline(col='grey')+
#   geom_hline(yintercept = c(35, 130, 298), linetype="dashed", col='grey')+
#   geom_vline(xintercept = c(35, 130, 298), linetype="dashed", col='grey')+
#   geom_smooth(method = "lm", formula = y~x, se=F, aes(x=rbr_predict_tls, color='#E15759'))+
#   geom_smooth(method = "lm", formula = y~x, se=F, aes(x=rbr_predict_als, color='#F28E2B'))+
#   geom_smooth(method = "lm", formula = y~x, se=F, aes(x=rbr_predict_zeb, color='#F1CE63'))+
#   geom_smooth(method = "lm", formula = y~x, se=F, aes(x=rbr_predict_uas, color='#59A14F'))+
#   geom_smooth(method = "lm", formula = y~x, se=F, aes(x=rbr_predict_banner, color='#499894')) +
#   xlab('Predicted burn severity \n(from airborne and ground methods)') +
#   ylab('Actual burn severity \n(from satellite)') +
#   geom_point(aes(x=rbr_predict_tls, color='#E15759'),size=1.5)+
#   geom_point(aes(x=rbr_predict_als, color='#F28E2B'),size=1.5)+
#   geom_point(aes(x=rbr_predict_zeb, color='#F1CE63'),size=1.5)+
#   geom_point(aes(x=rbr_predict_uas, color='#59A14F'),size=1.5)+
#   geom_point(aes(x=rbr_predict_banner, color='#499894'),size=1.5)+
#   scale_color_manual(values = c('#E15759','#F28E2B','#F1CE63','#59A14F','#499894'),
#                      labels=c('TLS\n(r2=0.834)\n', 'ALS\n(r2= 0.830)\n',
#                               'ZEB\n(r2= 0.828)\n', 'UAS\n(r2= 0.739)\n',
#                               'Banner\n(r2= 0.783)\n'))+
#   xlim(-5, 350) +
#   ylim(-5, 350) +
#   labs(colour="Method") +
#   annotate('text', x=330, y=7, label='very\nlow', angle=90, size=5)+
#   annotate('text', x=340, y=80, label='low', angle=90, size=5)+
#   annotate('text', x=340, y=215, label='medium', angle=90, size=5)+
#   annotate('text', x=335, y=335, label='high', angle=45, size=5)+
#   annotate('text', x=7, y=330, label='very\nlow', size=5)+
#   annotate('text', x=80, y=340, label='low', size=5)+
#   annotate('text', x=215, y=340, label='medium', size=5) +
#   theme(legend.position = 'none', axis.title = element_text(size = 20),
#         axis.line=element_blank(),axis.text.x=element_blank(),
#         axis.text.y=element_blank(),axis.ticks=element_blank(),)
# presentation_figure
# 
# ggsave(
#   presentation_out,
#   plot=presentation_figure,
#   width = 5,
#   height = 4.5,
#   units = 'in',
#   dpi = 300)

#==============================Wrapped Figure===================================
# 
tls_lm <- lm(rbr_actual~rbr_predict_tls, data=rbr) #P=1.031e-10 *** Rsq=0.8358
summary(tls_lm)

equation_tls <- glue('y = {round(tls_lm$coefficients[2],2)}x + {round(tls_lm$coefficients[1], 2)}')
equation_tls #y = 1.27x + -35.97

wrap_tls <- ggplot(data = rbr, aes(y=rbr_actual)) +
  geom_abline(col='grey')+
  geom_hline(yintercept = c(35, 130, 298), linetype="dashed",col = 'grey')+
  geom_vline(xintercept = c(35, 130, 298), linetype="dashed",col = 'grey')+
  geom_smooth(method = "lm", formula = y~x, se=F, aes(x=rbr_predict_tls), col='black',size=1.5)+
  geom_point(aes(x=rbr_predict_tls, color=rbr_class),size=3.5)+
  scale_color_manual(values = c('red', 'orange', 'yellow2', 'green3'))+
  xlim(-5, 350) +
  ylim(-5, 350) +
  labs(colour="Burn Severity") +
  annotate('text', x=340, y=7, label='NC', angle=90, size=7, color='grey50')+
  annotate('text', x=340, y=80, label='low', angle=90, size=7, color='grey50')+
  annotate('text', x=340, y=215, label='medium', angle=90, size=7, color='grey50')+
  annotate('text', x=335, y=335, label='high', angle=45, size=7, color='grey50')+
  annotate('text', x=7, y=340, label='NC', size=7, color='grey50')+
  annotate('text', x=80, y=340, label='low', size=7, color='grey50')+
  annotate('text', x=215, y=340, label='medium', size=7, color='grey50')+
  annotate('label', x=215, y=20, label='y = 1.24x + -33.29\nRsq = 0.82 \np<0.001 ', size=5)+
  ggtitle("TLS")+
  theme(legend.position = 'none', axis.title = element_blank())
wrap_tls

als_lm <- lm(rbr_actual~rbr_predict_als, data=rbr) #P=8.199e-11 *** Rsq=0.8391
summary(als_lm)

equation_als <- glue('y = {round(als_lm$coefficients[2],2)}x + {round(als_lm$coefficients[1], 2)}')
equation_als #y = 1.41x + -59.34

wrap_als <- ggplot(data = rbr, aes(y=rbr_actual)) +
  geom_abline(col='grey')+
  geom_hline(yintercept = c(35, 130, 298), linetype="dashed",col = 'grey')+
  geom_vline(xintercept = c(35, 130, 298), linetype="dashed",col = 'grey')+
  geom_smooth(method = "lm", formula = y~x, se=F, aes(x=rbr_predict_als), col='black',size=1.5)+
  geom_point(aes(x=rbr_predict_als, color=rbr_class),size=3.5)+
  scale_color_manual(values = c('red', 'orange', 'yellow2', 'green3'))+
  xlim(-5, 350) +
  ylim(-5, 350) +
  labs(colour="Burn Severity") +
  annotate('text', x=340, y=7, label='NC', angle=90, size=7, color='grey50')+
  annotate('text', x=340, y=80, label='low', angle=90, size=7, color='grey50')+
  annotate('text', x=340, y=215, label='medium', angle=90, size=7, color='grey50')+
  annotate('text', x=335, y=335, label='high', angle=45, size=7, color='grey50')+
  annotate('text', x=7, y=340, label='NC', size=7, color='grey50')+
  annotate('text', x=80, y=340, label='low', size=7, color='grey50')+
  annotate('text', x=215, y=340, label='medium', size=7, color='grey50')+
  annotate('label', x=215, y=20, label='y = 1.42x + -60.64\nRsq = 0.83\np<0.001', size=5)+
  ggtitle("ALS")+
  theme(legend.position = 'none', axis.title = element_blank())
wrap_als
# 
zeb_lm <- lm(rbr_actual~rbr_predict_zeb, data=rbr) #P=7.074e-10 *** Rsq=0.8203
summary(zeb_lm)

equation_zeb <- glue('y = {round(zeb_lm$coefficients[2],2)}x + {round(zeb_lm$coefficients[1], 2)}')
equation_zeb #y = 1.48x + -64.19

wrap_zeb <- ggplot(data = rbr, aes(y=rbr_actual)) +
  geom_abline(col='grey')+
  geom_hline(yintercept = c(35, 130, 298), linetype="dashed",col = 'grey')+
  geom_vline(xintercept = c(35, 130, 298), linetype="dashed",col = 'grey')+
  geom_smooth(method = "lm", formula = y~x, se=F, aes(x=rbr_predict_zeb), col='black',size=1.5)+
  geom_point(aes(x=rbr_predict_zeb, color=rbr_class),size=3.5)+
  scale_color_manual(values = c('red', 'orange', 'yellow2', 'green3'))+
  xlim(-5, 350) +
  ylim(-5, 350) +
  labs(colour="Burn Severity") +
  annotate('text', x=340, y=7, label='NC', angle=90, size=7, color='grey50')+
  annotate('text', x=340, y=80, label='low', angle=90, size=7, color='grey50')+
  annotate('text', x=340, y=215, label='medium', angle=90, size=7, color='grey50')+
  annotate('text', x=335, y=335, label='high', angle=45, size=7, color='grey50')+
  annotate('text', x=7, y=340, label='NC', size=7, color='grey50')+
  annotate('text', x=80, y=340, label='low', size=7, color='grey50')+
  annotate('text', x=215, y=340, label='medium', size=7, color='grey50')+
  annotate('label', x=215, y=20, label='y = 1.49x + -65.29\nRsq = 0.83\np<0.001', size=5)+
  ggtitle("HMLS")+
  theme(legend.position = 'none', axis.title = element_blank())
wrap_zeb

uas_lm <- lm(rbr_actual~rbr_predict_uas, data=rbr) #P= 7.608e-06 *** Rsq= 0.7304
summary(uas_lm)

equation_uas <- glue('y = {round(uas_lm$coefficients[2],2)}x + {round(uas_lm$coefficients[1], 2)}')
equation_uas #y = 1.26x + -32.58

wrap_uas <- ggplot(data = rbr, aes(y=rbr_actual)) +
  geom_abline(col='grey')+
  geom_hline(yintercept = c(35, 130, 298), linetype="dashed",col = 'grey')+
  geom_vline(xintercept = c(35, 130, 298), linetype="dashed",col = 'grey')+
  geom_smooth(method = "lm", formula = y~x, se=F, aes(x=rbr_predict_uas), col='black',size=1.5)+
  geom_point(aes(x=rbr_predict_uas, color=rbr_class),size=3.5)+
  scale_color_manual(values = c('red', 'orange', 'yellow2', 'green3'))+
  xlim(-5, 350) +
  ylim(-5, 350) +
  labs(colour="Burn Severity") +
  annotate('text', x=340, y=7, label='NC', angle=90, size=7, color='grey50')+
  annotate('text', x=340, y=80, label='low', angle=90, size=7, color='grey50')+
  annotate('text', x=340, y=215, label='medium', angle=90, size=7, color='grey50')+
  annotate('text', x=335, y=335, label='high', angle=45, size=7, color='grey50')+
  annotate('text', x=7, y=340, label='NC', size=7, color='grey50')+
  annotate('text', x=80, y=340, label='low', size=7, color='grey50')+
  annotate('text', x=215, y=340, label='medium', size=7, color='grey50')+
  annotate('label', x=215, y=20, label='y = 1.3x + -35.54\nRsq = 0.80\np<0.001', size=5)+
  ggtitle("UAS")+
  theme(legend.position = 'none', axis.title = element_blank())
wrap_uas
# 
banner_lm <- lm(rbr_actual~rbr_predict_banner, data=rbr) #P= 1.463e-09 *** Rsq=0.8082
summary(banner_lm)

equation_banner <- glue('y = {round(banner_lm$coefficients[2],2)}x + {round(banner_lm$coefficients[1], 2)}')
equation_banner #y = 1.35x + -50.5

wrap_banner <- ggplot(data = rbr, aes(y=rbr_actual)) +
  geom_abline(col='grey')+
  geom_hline(yintercept = c(35, 130, 298), linetype="dashed",col = 'grey')+
  geom_vline(xintercept = c(35, 130, 298), linetype="dashed",col = 'grey')+
  geom_smooth(method = "lm", formula = y~x, se=F, aes(x=rbr_predict_banner), col='black',size=1.5)+
  geom_point(aes(x=rbr_predict_banner, color=rbr_class),size=3.5)+
  scale_color_manual(values = c('red', 'orange', 'yellow2', 'green3'))+
  xlim(-5, 350) +
  ylim(-5, 350) +
  labs(colour="RBR Class") +
  annotate('text', x=340, y=7, label='NC', angle=90, size=7, color='grey50')+
  annotate('text', x=340, y=80, label='low', angle=90, size=7, color='grey50')+
  annotate('text', x=340, y=215, label='medium', angle=90, size=7, color='grey50')+
  annotate('text', x=335, y=335, label='high', angle=45, size=7, color='grey50')+
  annotate('text', x=7, y=340, label='NC', size=7, color='grey50')+
  annotate('text', x=80, y=340, label='low', size=7, color='grey50')+
  annotate('text', x=215, y=340, label='medium', size=7, color='grey50')+
  annotate('label', x=215, y=20, label='y = 1.32x + -45.62\nRsq = 0.77\np<0.001', size=5)+
  ggtitle("Banner")+
  theme(legend.position = 'bottom', axis.title = element_blank())+
  guides(color = guide_legend(nrow = 3))
wrap_banner


wrap_all <-  ggplot(data = rbr, aes(y=rbr_actual)) +
  geom_abline(col='grey')+
  geom_hline(yintercept = c(35, 130, 298), linetype="dashed",col = 'grey')+
  geom_vline(xintercept = c(35, 130, 298), linetype="dashed",col = 'grey')+
  geom_smooth(method = "lm", formula = y~x, se=F, aes(x=rbr_predict_tls, color='#E15759'),size=1)+
  geom_smooth(method = "lm", formula = y~x, se=F, aes(x=rbr_predict_als, color='#F28E2B'),size=1)+
  geom_smooth(method = "lm", formula = y~x, se=F, aes(x=rbr_predict_zeb, color='#F1CE63'),size=1)+
  geom_smooth(method = "lm", formula = y~x, se=F, aes(x=rbr_predict_uas, color='#59A14F'),size=1)+
  geom_smooth(method = "lm", formula = y~x, se=F, aes(x=rbr_predict_banner, color='#499894'),size=1) +
  geom_point(aes( x=rbr_predict_tls, color='#E15759'),size=2.5)+
  geom_point(aes( x=rbr_predict_zeb, color='#F1CE63'),size=2.5)+
  geom_point(aes( x=rbr_predict_als, color='#F28E2B'),size=2.5)+
  geom_point(aes( x=rbr_predict_uas, color='#59A14F'),size=2.5)+
  geom_point(aes( x=rbr_predict_banner, color='#499894'),size=2.5)+
  scale_color_manual(values = c('#0D0887FF','#900DA4FF','#E16462FF','#FCCE25FF','#21908CFF'),
                     labels=c('TLS', 'MLS',
                              'ALS', 'UAS',
                             'Banner'))+
  xlim(-5, 350) +
  ylim(-5, 350) +
  labs(colour="Method") +
  annotate('text', x=340, y=7, label='NC', angle=90, size=7, color='grey50')+
  annotate('text', x=340, y=80, label='low', angle=90, size=7, color='grey50')+
  annotate('text', x=340, y=215, label='medium', angle=90, size=7, color='grey50')+
  annotate('text', x=335, y=335, label='high', angle=45, size=7, color='grey50')+
  annotate('text', x=7, y=340, label='NC', size=7, color='grey50')+
  annotate('text', x=80, y=340, label='low', size=7, color='grey50')+
  annotate('text', x=215, y=340, label='medium', size=7, color='grey50')+
  ggtitle("All Methods") +
  theme(legend.position = 'bottom', axis.title = element_blank())+
  guides(color = guide_legend(nrow = 3))
wrap_all

wrap_figure <-
  ggarrange(
    wrap_tls,
    wrap_zeb,
    wrap_als,
    wrap_uas,
    wrap_banner,
    wrap_all,
    ncol = 2,
    nrow = 3,
    widths = c(4.5, 4.5),
    heights = c(4.5, 4.5, 5.25)
  )

wrap_figure <- annotate_figure(wrap_figure,
                           bottom = text_grob('Predicted RBR', 
                                              family = 'serif', 
                                              size = 32),
                           left = text_grob('Actual RBR', 
                                              family = 'serif', 
                                              size = 32,
                                            rot = 90))

ggsave(
  wrap_out,
  plot=wrap_figure,
  width = 13,
  height = 20,
  units = 'in',
  dpi = 300 )


