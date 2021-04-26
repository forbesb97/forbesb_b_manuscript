library(tidyverse)

control_plots = c('1301','1303','1304','1306','1307','1309','1310','1312','1313',
                  '1315','1319','1325','1329','1332','1335','1336','1337','1340',
                  '1342','1349','1851','1852','1853','1854','2','3','5','8','11',
                  '13','15','17','20','24','26')

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

banner <-read_csv('D:/Analyses/ladder_fuel_jmp/c1_ladder-fuels_metrics_tls_zeb_als_uas_notfiltered_210426.csv') %>%
  filter(plot %in% control_plots) %>%
  select(plot, "trad_ladder_fuel_1to2","trad_ladder_fuel_2to3","trad_ladder_fuel_3to4")

rbr <- read_csv('D:/Analyses/kincade_glass_fire_tls_plot_centers_sentinel2a_20m_rbr.csv') %>%
  select(Plot, RBR_3x3avg) %>%
  rename(plot=Plot,
         rbr=RBR_3x3avg)

rbr$plot <- stringr::str_remove(rbr$plot, 'p')
rbr$plot <- stringr::str_remove(rbr$plot, 'P')

rbr <- rbr %>%
  mutate_at('plot', as.numeric)

cbh <- read_csv('D:/Analyses/c1_c6_LLC_LLF_treatmentremoved.csv') %>%
  select(plot, "Lowest Living Canopy") %>%
  rename(cbh="Lowest Living Canopy")

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

data$rbr_class <-
  cut(
    data$rbr,
    breaks = c( 0,  35, 130, 298, Inf),
    label = c('NC', 'Low', 'Medium', 'High')
  )

data_filter$rbr_class <-
  cut(
    data_filter$rbr,
    breaks = c( 0,  35, 130, 298, Inf),
    label = c('NC', 'Low', 'Medium', 'High')
  )

data$rbr_jmp <-
  cut(
    data$rbr,
    breaks = c( 0,  35, 130, 298, Inf),
    label = c('A', 'B', 'C', 'D')
  )

data_filter$rbr_jmp <-
  cut(
    data_filter$rbr,
    breaks = c( 0,  35, 130, 298, Inf),
    label = c('A', 'B', 'C', 'D')
  )


data$cbh_jmp <-
  cut(
    data$cbh,
    breaks = c( 0,  3, 6, 9, Inf),
    label = c('E', 'F', 'G', 'H')
  )

data_filter$cbh_jmp <-
  cut(
    data_filter$cbh,
    breaks = c( 0,  3, 6, 9, Inf),
    label = c('E', 'F', 'G', 'H')
  )

write_csv(data, 'D:/Analyses/ladder_fuel_jmp/ladder-fuels_metrics_tls_zeb_als_uas_notfiltered_210426.csv')
write_csv(data_filter, 'D:/Analyses/ladder_fuel_jmp/ladder-fuels_metrics_tls_zeb_als_uas_filtered_210426.csv')


data_burned <- data %>%
  filter(!is.na(rbr))

data_burned_filter <- data_filter %>%
  filter(!is.na(rbr))

write_csv(data_burned, 'D:/Analyses/ladder_fuel_jmp/ladder-fuels_metrics_tls_zeb_als_uas_notfiltered_burned_210426.csv')
write_csv(data_burned_filter, 'D:/Analyses/ladder_fuel_jmp/ladder-fuels_metrics_tls_zeb_als_uas_filtered_burned_210426.csv')
