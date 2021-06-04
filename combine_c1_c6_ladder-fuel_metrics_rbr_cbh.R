library(tidyverse)

control_plots = c('1301','1303','1304','1306','1307','1309','1310','1312','1313',
                  '1315','1319','1325','1329','1332','1335','1336','1337','1340',
                  '1342','1349','1851','1852','1853','1854','2','3','5','8','11',
                  '13','15','17','20','24','26')


 #read files, remove NA values from other plot and banner data 
c1 <- read_csv('D:/Analyses/ladder_fuel_jmp/c1_ladder-fuels_metrics_tls_zeb_als_uas_notfiltered_210426.csv') %>%
  filter(plot %in% control_plots & campaign == '1') %>%
  select(campaign:uas_ladder_fuel_7to8)
c1_filter <- read_csv('D:/Analyses/ladder_fuel_jmp/c1_ladder-fuels_metrics_tls_zeb_als_uas_filtered_210426.csv')%>%
  filter(plot %in% control_plots & campaign == '1')%>%
  select(campaign:uas_ladder_fuel_7to8)

c6 <- read_csv('D:/Analyses/ladder_fuel_jmp/c6_ladder-fuels_metrics_tls_zeb_als_uas_notfiltered_210426.csv')%>%
  filter(plot %in% control_plots & campaign == '6')%>%
  select(campaign:uas_ladder_fuel_7to8)
c6_filter <- read_csv('D:/Analyses/ladder_fuel_jmp/c6_ladder-fuels_metrics_tls_zeb_als_uas_filtered_210426.csv')%>%
  filter(plot %in% control_plots & campaign == '6')%>%
  select(campaign:uas_ladder_fuel_7to8)

#pull just banner data out 
banner <-read_csv('D:/Analyses/ladder_fuel_jmp/c1_ladder-fuels_metrics_tls_zeb_als_uas_notfiltered_210426.csv') %>%
  filter(plot %in% control_plots) %>%
  select(plot, "trad_ladder_fuel_1to2","trad_ladder_fuel_2to3","trad_ladder_fuel_3to4")

c1_rbr <- read_csv('D:/Analyses/kincade_plot_rdnbr_rbr_with_offset.csv')%>%
  rename(plot = Plot)%>%
  select( "plot" ,"L8_RdNBR3x3","L8_RdNBR","L8_RBR3x3","L8_RBR")
c6_rbr <- read_csv('D:/Analyses/saddle_mountain_plot_glass_l8_rdnbr_rbr_with_offset.csv') %>%
  rename(plot = Plot)%>%
  select("plot","L8_RdNBR3x3","L8_RdNBR","L8_RBR3x3","L8_RBR")

#remove p or P before plot number
c6_rbr$plot <- stringr::str_remove(c6_rbr$plot, 'p')
c6_rbr$plot <- stringr::str_remove(c6_rbr$plot, 'P')

c1_rbr <- c1_rbr %>%
  mutate_at('plot', as.numeric)

c6_rbr <- c6_rbr %>%
  mutate_at('plot', as.numeric)

rbr <- c1_rbr %>%
  add_row(c6_rbr)

#read cbh info and rename columns 
cbh <- read_csv('D:/Analyses/c1_c6_LLC.csv') %>%
  rename(cbh="llc_mean")

#combine all data for filtered and not filtered

data <- c1 %>%
  add_row(c6)%>%
  left_join(banner)%>%
  left_join(rbr) %>%
  left_join(cbh) 

data_filter <- c1_filter %>%
  add_row(c6_filter)%>%
  left_join(banner)%>%
  left_join(rbr) %>%
  left_join(cbh)

#add in rbr and cbh classes 

data$L8_RdNBR3x3_class <-
  cut(
    data$L8_RdNBR3x3,
    breaks = c(-Inf,  69, 315, 640, Inf),
    label = c('NC', 'Low', 'Moderate', 'High')
  )

data_filter$L8_RdNBR3x3_class <-
  cut(
    data_filter$L8_RdNBR3x3,
    breaks = c( -Inf,  69, 315, 640, Inf),
    label = c('NC', 'Low', 'Moderate', 'High')
  )

data$L8_RdNBR_class <-
  cut(
    data$L8_RdNBR,
    breaks = c(-Inf,  69, 315, 640, Inf),
    label = c('NC', 'Low', 'Moderate', 'High')
  )

data_filter$L8_RdNBR_class <-
  cut(
    data_filter$L8_RdNBR,
    breaks = c( -Inf,  69, 315, 640, Inf),
    label = c('NC', 'Low', 'Moderate', 'High')
  )

data$L8_RdNBR3x3_jmp <-
  cut(
    data$L8_RdNBR3x3,
    breaks = c( -Inf,  69, 315, 640, Inf),
    label = c('A', 'B', 'C', 'D')
  )

data_filter$L8_RdNBR3x3_jmp <-
  cut(
    data_filter$L8_RdNBR3x3,
    breaks = c(-Inf,  69, 315, 640, Inf),
    label = c('A', 'B', 'C', 'D')
  )

data$L8_RdNBR_jmp <-
  cut(
    data$L8_RdNBR,
    breaks = c( -Inf,  69, 315, 640, Inf),
    label = c('A', 'B', 'C', 'D')
  )

data_filter$L8_RdNBR_jmp <-
  cut(
    data_filter$L8_RdNBR,
    breaks = c(-Inf,  69, 315, 640, Inf),
    label = c('A', 'B', 'C', 'D')
  )

data$L8_RBR3x3_class <-
  cut(
    data$L8_RBR3x3,
    breaks = c(-Inf,  35, 130, 298, Inf),
    label = c('NC', 'Low', 'Moderate', 'High')
  )

data_filter$L8_RBR3x3_class <-
  cut(
    data_filter$L8_RBR3x3,
    breaks = c( -Inf,  69, 315, 640, Inf),
    label = c('NC', 'Low', 'Moderate', 'High')
  )

data$L8_RBR_class <-
  cut(
    data$L8_RBR,
    breaks = c(-Inf,  35, 130, 298, Inf),
    label = c('NC', 'Low', 'Moderate', 'High')
  )

data_filter$L8_RBR_class <-
  cut(
    data_filter$L8_RBR,
    breaks = c( -Inf,  69, 315, 640, Inf),
    label = c('NC', 'Low', 'Moderate', 'High')
  )

data$L8_RBR3x3_jmp <-
  cut(
    data$L8_RBR3x3,
    breaks = c( -Inf,  69, 315, 640, Inf),
    label = c('E', 'F', 'G', 'H')
  )

data_filter$L8_RBR3x3_jmp <-
  cut(
    data_filter$L8_RBR3x3,
    breaks = c(-Inf,  35, 130, 298, Inf),
    label = c('E', 'F', 'G', 'H')
  )

data$L8_RBR_jmp <-
  cut(
    data$L8_RBR,
    breaks = c( -Inf,  69, 315, 640, Inf),
    label = c('E', 'F', 'G', 'H')
  )

data_filter$L8_RBR_jmp <-
  cut(
    data_filter$L8_RBR,
    breaks = c(-Inf,  35, 130, 298, Inf),
    label = c('E', 'F', 'G', 'H')
  )

data$cbh_jmp <-
  cut(
    data$cbh,
    breaks = c( 0,  3, 6, 9, Inf),
    label = c('I', 'J', 'K', 'L')
  )

data_filter$cbh_jmp <-
  cut(
    data_filter$cbh,
    breaks = c( 0,  3, 6, 9, Inf),
    label = c('I', 'J', 'K', 'L')
  )

write_csv(data, 'D:/Analyses/ladder_fuel_jmp/ladder-fuels_metrics_tls_zeb_als_uas_notfiltered_210601.csv')
write_csv(data_filter, 'D:/Analyses/ladder_fuel_jmp/ladder-fuels_metrics_tls_zeb_als_uas_filtered_210601.csv')

#filter for burned plots only
data_burned <- data %>%
  filter(!is.na(L8_RdNBR))

data_burned_filter <- data_filter %>%
  filter(!is.na(L8_RdNBR))

write_csv(data_burned, 'D:/Analyses/ladder_fuel_jmp/ladder-fuels_metrics_tls_zeb_als_uas_notfiltered_burned_210601.csv')
write_csv(data_burned_filter, 'D:/Analyses/ladder_fuel_jmp/ladder-fuels_metrics_tls_zeb_als_uas_filtered_burned_210601.csv')
