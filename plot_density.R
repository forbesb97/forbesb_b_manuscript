library(lidR)
library(dplyr)
library(readr)

tls_files <- list.files('D:/c1 - Pepperwood/c1_DEMnorm_las_plot', pattern = '.las', full.names = T)
mls_files <- list.files('D:/c1 - Pepperwood/c1_zeb_cut2plot', pattern = '.las', full.names = T)
als_files <- list.files('D:/c1 - Pepperwood/c1_ALS_normplot', pattern = '.las', full.names = T)
uas_files <- list.files('D:/c1 - Pepperwood/c1_uas', pattern = '.las', full.names = T)

combined_avg<- tibble(tls_avg_density=as.numeric(), mls_avg_density=as.numeric(), 
                           als_avg_density=as.numeric(), uas_avg_density=as.numeric())


for (tls_file in tls_files) {
  
  las <- readLAS(tls_file)
  tls_density <- grid_density(las, res = 1)
  
  df <- as.data.frame(tls_density)
  
  tls_avg_density <- mean(df$point_density, na.rm=T)
  
  combined_avg<- combined_avg %>%
    add_row(tls_avg_density )
}

for (mls_file in mls_files) {
  
  las <- readLAS(mls_file)
  mls_density <- grid_density(las, res = 1)
  
  df <- as.data.frame(mls_density)
  
  mls_avg_density <- mean(df$point_density, na.rm=T)
  
  combined_avg<- combined_avg %>%
    add_row(mls_avg_density )
}

for (als_file in als_files) {
  
  las <- readLAS(als_file)
  als_density <- grid_density(las, res = 1)
  
  df <- as.data.frame(als_density)
  
  als_avg_density <- mean(df$point_density, na.rm=T)
  
  combined_avg<- combined_avg %>%
    add_row(als_avg_density )
}

for (uas_file in uas_files) {
  
  las <- readLAS(uas_file)
  uas_density <- grid_density(las, res = 1)
  
  df <- as.data.frame(uas_density)
  
  uas_avg_density <- mean(df$point_density, na.rm=T)
  
  combined_avg<- combined_avg %>%
    add_row(uas_avg_density )
}

write.csv(combined_avg, 'D:/Analyses/point_density.csv')



all_plots <- read_csv('D:/Analyses/point_density.csv')


all_plots_avg <- all_plots %>%
  summarize(
    tls_avg_all = mean(tls_avg_density, na.rm=T),
    mls_avg_all = mean(mls_avg_density, na.rm=T),
    als_avg_all = mean(als_avg_density, na.rm=T),
    uas_avg_all = mean(uas_avg_density, na.rm=T)
  )


          