
library(doParallel) # used for parallel processing
library(caret) # used for SVM tuning (takes advantage of doParallel)
library(tools)
library(kernlab)
library(dplyr)
library(tidyverse)

#folder <- 'D:/Analyses/ladder_fuel_full_process_take4/not_filtered/'
folder <- 'D:/Analyses/ladder_fuel_full_process_take4/filtered_zero_and_above/'
files <- list.files(folder, pattern = '_210331.csv', full.names = T)

#stats_outfile <- 'D:/Analyses/ladder_fuel_full_process_take4/not_filtered/ladder_fuel_rbr_210405_stats.csv'

stats_outfile <- 'D:/Analyses/ladder_fuel_full_process_take4/filtered_zero_and_above/ladder_fuel_rbr_210405_stats.csv'

stats <- tibble(method=as.character(), mtry=as.numeric(), RMSE=as.numeric(),Rsquared=as.numeric(),MAE=as.numeric(),RMSESD=as.numeric(), RsquaredSD=as.numeric(),MAESD=as.numeric(),)

# response variable
response <- c('RBR_3x3avg')


for (file in files) {
  data <- read.csv(file, header = T) %>%
    filter(!is.na(ladder_fuel_1to2))
  
  method <- unlist(strsplit(file, '_'))[10]                                       #may need to be changed with new folder and file names
  
  if (method == 'banner'){
    predictors <-
    c('ladder_fuel_1to2',
      'ladder_fuel_1to3')
  } else if (method == 'uas'){
    
    numfolds <- 5
  } else {
    predictors <-
    c('ladder_fuel_1to2',
      'ladder_fuel_1to3',
      'ladder_fuel_7to8')
    
    numfolds <- 10
    }
  set.seed(54)
  
  ctrl <- trainControl(method = "cv",
                       number = numfolds,
                       allowParallel = F)
  set.seed(54)
  # Random Forests
  rfTrain <- train(
    data[, predictors],
    data[, response],
    method = "rf",
    #preProcess = c("center", "scale"),
    trControl = ctrl,
    ntree = 1000,
    tuneLength = 10,
    metric = "RMSE",
    allowParallel = TRUE,
    na.action = na.omit
  )
  
  
  
  mtry <- rfTrain$bestTune[1]
  rfStats <-
    rfTrain$results[which(rfTrain$results$mtry == as.numeric(mtry)),]
  
  rfStats <- rfStats %>%
    add_column(method, .before = 1)
  
  rfStats$method <- method
  
  stats <- stats %>%
    add_row(rfStats)
  
  if(method=='banner') {
    csv_select <- data %>%
      select(ladder_fuel_1to2,
             ladder_fuel_1to3)
    
  } else{
    csv_select <- data %>%
      select(ladder_fuel_1to2,
             ladder_fuel_7to8,
             ladder_fuel_1to3)
  }
  
  set.seed(54)
  predict <- predict(rfTrain, csv_select)
 
   rbr_data <- data %>%
    select(plot, RBR_3x3avg ) %>%
    rename('rbr_actual' = 'RBR_3x3avg')
  
  rbr_data$rbr_predict <- predict
  
  base <- paste(tools::file_path_sans_ext(file), "_", response, sep = '')
  predict_outfile <- paste(base, "_rf_predict.csv", sep = "")
  write.csv(rbr_data, predict_outfile)
  
  workspace_outfile <- paste(base, "_rf_predict.RData", sep = "")
  save.image(file = workspace_outfile)
}


write_csv(stats,stats_outfile)


