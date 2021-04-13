# PURPOSE:
# Performs a Random Forest, Support Vector Machine and ordinary least-squares regression analysis and a 
# 5-fold cross validation with 10 repeasts used to calculate RMSE and r-squared.

# Methods generally follow:
# de Almeida, C. T., Galv?o, L. S., Ometto, J. P. H. B., Jacon, A. D., de Souza Pereira, F. R., Sato, L. Y., ... & Longo, M. 
# (2019). Combining LiDAR and hyperspectral data for aboveground biomass modeling in the Brazilian Amazon using different 
# regression algorithms. Remote Sensing of Environment, 232, 111323.


# AUTHOR:
# Dr. Matthew Clark
# Dept of Geography and Global Studies
# Sonoma State University
# 1801 E. Cotati Ave
# Rohnert Park, CA 94928 USA
# matthew.clark@sonoma.edu
# +1 707-664-2558

# Version: September 17, 2019; R 3.5.3
# Update: November 26, 2019; R 3.5.3; Changed predictor inputs for lidR standard canopy metrics
# Update: December 14, 2020: change inputs/outputs for Brieanne Forbes thesis

# load necessary libraries
library(doParallel) # used for parallel processing
library(caret) # used for SVM tuning (takes advantage of doParallel)
library(tools)
library(kernlab)


# remove any existing objects
rm(list=ls(all=TRUE))


# *******************************************************************************************************
# set these parameters before running

# set working directory
workspace<-'D:/Analyses/ladder_fuel_full_process_take2'
setwd(workspace)
# files to process

files <-
  c('ladder-fuels_metrics_210206.csv'
  )


# response variable
responses <- c('rbr_3x3')


# lidR standard metrics predictors
predictors <-
  c('tls_density_ladder_fuel_1to2',
    'tls_density_ladder_fuel_1to3',
    'tls_density_ladder_fuel_2to3',
    'tls_density_ladder_fuel_3to4',
    'tls_density_ladder_fuel_2to4',
    'tls_density_ladder_fuel_1to4',
    'tls_density_ladder_fuel_1to5',
    'tls_density_ladder_fuel_2to5',
    'tls_density_ladder_fuel_3to5',
    'tls_density_ladder_fuel_4to5',
    'tls_density_ladder_fuel_1to6',
    'tls_density_ladder_fuel_2to6',
    'tls_density_ladder_fuel_3to6',
    'tls_density_ladder_fuel_4to6',
    'tls_density_ladder_fuel_5to6',
    'tls_density_ladder_fuel_1to7',
    'tls_density_ladder_fuel_2to7',
    'tls_density_ladder_fuel_3to7',
    'tls_density_ladder_fuel_4to7',
    'tls_density_ladder_fuel_5to7',
    'tls_density_ladder_fuel_6to7',
    'tls_density_ladder_fuel_1to8',
    'tls_density_ladder_fuel_2to8',
    'tls_density_ladder_fuel_3to8',
    'tls_density_ladder_fuel_4to8',
    'tls_density_ladder_fuel_5to8',
    'tls_density_ladder_fuel_6to8',
    'tls_density_ladder_fuel_7to8'
  )

# set seed
set.seed(111)

# number of folds
numfolds <- 10

# repeats
repeats <- 10
repeatsfinal <- 100

# ***********************************************

for (r in responses) {
  for (i in files) {
    
    # read in CSV file
    data <- read.csv(i, header = TRUE)
    
    # base file name
    response<-r
    base <- paste(tools::file_path_sans_ext(i), "_", response, sep = '')
    
    # output R workspace
    outRobj <- paste(base, "_pickbest.RData", sep = '')
    
    
    # Start log file
    logfile <- paste(base, "_pickbest_log.txt", sep = "")
    timestamp<-format(Sys.time(), "%Y%m%d_%H%M")	
    write(paste(
      "--- ",
      format(Sys.time(), "%Y-%m-%d %H:%M"),
      "  Begin",
      sep =
        ''
    ),
    file = logfile,
    append = FALSE)	# erase prior file, if any
    write(paste("----------", sep = ''),
          file = logfile,
          append = TRUE)
    write(
      paste("Working directory: ", workspace, sep = ''),
      file = logfile,
      append = TRUE
    )
    write(
      paste("Input training data: ", i, sep = ''),
      file = logfile,
      append = TRUE
    )
    write(
      paste("R training workspace output file: ", outRobj, sep = ''),
      file = logfile,
      append = TRUE
    )
    write(
      paste("Response: ", response, sep = ''),
      file = logfile,
      append = TRUE
    )
    write("", file = paste(logfile), append = TRUE)
    
    # Perform RFE
    
    prednum <- length(predictors)
    
    # Register do parallel (uses all detected cores except 2)
    cl <- makeCluster(detectCores() - 2, outfile = "c:/temp/Log.txt")
    registerDoParallel(cl)
    
    # training control for internal model parameters
    ctrl <- trainControl(method = "cv",
                         number = numfolds,
                         allowParallel = TRUE)
    
    # Random Forests RFE
    rfProfile <- rfe(
      data[, predictors],
      data[, response],
      sizes = c(2:prednum),
      rfeControl = rfeControl(
        functions = rfFuncs,
        method = "repeatedcv",
        number = numfolds,
        repeats = repeats,
        allowParallel = TRUE
      ),
      #preProcess = c("center", "scale"),
      ntree = 1000,
      tuneLength = 10,
      metric = "RMSE",
      trControl = ctrl
    )
    
    
    # Support Vector Regression RFE
    svmProfile <- rfe(
      data[, predictors],
      data[, response],
      sizes = c(2:prednum),
      rfeControl = rfeControl(
        functions = caretFuncs,
        method = "repeatedcv",
        number = numfolds,
        repeats = repeats,
        allowParallel = TRUE
      ),
      method = "svmRadial",
      preProcess = c("center", "scale"),
      tuneLength = 10,
      metric = "RMSE",
      trControl = ctrl
    )
    
    # Ordinary least-squares regression RFE
    lmProfile <- rfe(
      data[, predictors],
      data[, response],
      sizes = c(2:prednum),
      rfeControl = rfeControl(
        functions = lmFuncs,
        method = "repeatedcv",
        number = numfolds,
        repeats = repeats,
        allowParallel = TRUE
      ),
      metric = "RMSE",
      trControl = ctrl
    )
    
    # pick best variables
    rfSize <-
      pickSizeBest(
        rfProfile$results,
        metric = "RMSE",
        maximize = FALSE
      )
    svmSize <-
      pickSizeBest(
        svmProfile$results,
        metric = "RMSE",
        maximize = FALSE
      )
    lmSize <-
      pickSizeBest(
        lmProfile$results,
        metric = "RMSE",
        maximize = FALSE
      )
    
    rfVariables <- rfProfile$optVariables[1:rfSize]
    svmVariables <- svmProfile$optVariables[1:svmSize]
    lmVariables <- lmProfile$optVariables[1:lmSize]
    
    # train final models
    
    # Training control for model parameters
    ctrl <- trainControl(
      method = "repeatedcv",
      number = numfolds,
      repeats = repeatsfinal,
      allowParallel = TRUE
    )
    
    # Random Forests
    rfTrain <- train(
      data[, rfVariables],
      data[, response],
      method = "rf",
      #preProcess = c("center", "scale"),
      trControl = ctrl,
      ntree = 1000,
      tuneLength = 10,
      metric = "RMSE",
      allowParallel = TRUE
    )
    
    mtry <- rfTrain$bestTune[1]
    rfStats <- rfTrain$results[which(rfTrain$results$mtry == as.numeric(mtry)), ]
    rfRMSECV <- rfStats$RMSE
    rfRsquaredCV <- rfStats$Rsquared
    
    # SVR
    svmTrain <- train(
      data[, svmVariables],
      data[, response],
      method = "svmRadial",
      preProcess = c("center", "scale"),
      trControl = ctrl,
      tuneLength = 10,
      metric = "RMSE",
      allowParallel = TRUE,
      fitBest = FALSE, 
      returnData = TRUE
    )
    sigma <- svmTrain$bestTune[1]
    cost <- svmTrain$bestTune[2]
    svmStats <- svmTrain$results[which(svmTrain$results$C == as.numeric(cost)), ]
    svmRMSECV <- svmStats$RMSE
    svmRsquaredCV <- svmStats$Rsquared
    
    # OLS regression
    lmTrain <- train(data[, lmVariables], data[, response],
                     method = "lm",
                     #preProcess = c("center", "scale"),
                     trControl = ctrl)
    lmStats <- lmTrain$results
    lmRMSECV = lmStats$RMSE
    lmRsquaredCV = lmStats$Rsquared
    
    # stop parallel processing cluster
    stopCluster(cl)
    
    # Write out results to log file
    write(paste("---------------------"),
          file = logfile,
          append = TRUE)
    write(paste("Random Forests"),
          file = logfile,
          append = TRUE)
    write(paste("RMSE-CV: ", rfRMSECV, sep = ''),
          file = logfile,
          append = TRUE)
    write(paste("Rsquared-CV: ", rfRsquaredCV, sep = ''),
          file = logfile,
          append = TRUE)
    write(paste("mtry: ", mtry, sep = ''),
          file = logfile,
          append = TRUE
    )
    write("", file = paste(logfile), append = TRUE)
    write(paste("---------------------"),
          file = logfile,
          append = TRUE)
    write(paste("Support Vector Regression"),
          file = logfile,
          append = TRUE)
    write(paste("RMSE-CV: ", svmRMSECV, sep = ''),
          file = logfile,
          append = TRUE)
    write(paste("Rsquared-CV: ", svmRsquaredCV, sep = ''),
          file = logfile,
          append = TRUE)
    write(paste("Cost: ", cost, sep = ''),
          file = logfile,
          append = TRUE)
    write(paste("Sigma: ", sigma, sep = ''),
          file = logfile,
          append = TRUE
    )
    write("", file = paste(logfile), append = TRUE)
    write(paste("---------------------"),
          file = logfile,
          append = TRUE)
    write(paste("OLS Regression"),
          file = logfile,
          append = TRUE)
    write(paste("RMSE-CV: ", lmRMSECV, sep = ''),
          file = logfile,
          append = TRUE)
    write(
      paste("Rsquared-CV: ", lmRsquaredCV, sep = ''),
      file = logfile,
      append = TRUE
    )
    write("", file = paste(logfile), append = TRUE)
    
    # Write out statistics files
    statsfile <- paste(base, "_pickbest_rf_stats.csv", sep = "")
    write.csv(rfStats, file=statsfile,row.names=FALSE)
    statsfile <- paste(base, "_pickbest_svm_stats.csv", sep = "")
    write.csv(svmStats, file=statsfile,row.names=FALSE)
    statsfile <- paste(base, "_pickbest_lm_stats.csv", sep = "")
    write.csv(lmStats, file=statsfile,row.names=FALSE)
    
    # Save RF workspace and close
    write("", file = logfile, append = TRUE)
    write(
      paste(
        "--- ",
        format(Sys.time(), "%Y-%m-%d %H:%M"),
        "  Writing R workspace",
        sep = ''
      ),
      file = logfile,
      append = TRUE
    )
    save.image(file = outRobj) # Saves the entire R workspace, which includes all variables
    write(
      paste(
        "--- ",
        format(Sys.time(), "%Y-%m-%d %H:%M"),
        "  Finished",
        sep =
          ''
      ),
      file = logfile,
      append = TRUE
    )
    rm(logfile)
  } # end files loop
} # end responses loop

#
#