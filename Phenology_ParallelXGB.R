library(tidyverse)
library(neon4cast)
library(lubridate)
library(rMR)
library(glue)
library(xgboost)

source("R/ignore_sigpipe.R")
source("R/noaa_mean_forecast.R")
source("R/noaa_mean_historical.R")

forecast_date <- Sys.Date()
forecast_doy = as.numeric(format(forecast_date, '%j'))
noaa_date <- Sys.Date() - days(1)  #Need to use yesterday's NOAA forecast because today's is not available yet

# Step 0: Define a unique name which will identify your model in the leaderboard and connect it to team members info, etc
model_id <- "xgboost_parallel"

# Step 1: Download latest target data and site description data
target <- readr::read_csv("https://data.ecoforecast.org/neon4cast-targets/phenology/phenology-targets.csv.gz", guess_max = 1e6) |> 
  na.omit()
site_data <- readr::read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv") |> 
  dplyr::filter(phenology == 1)


target_names <- c("gcc_90", "rcc_90")

# Step 2: Get meteorological predictions as drivers
df_past <- neon4cast::noaa_stage3()

# Step 2.5: We'll skip any site that doesn't have both temperature and oxygen
# sites <- target |> na.omit() |> distinct(site_id, variable) |> 
#   filter(variable %in% c("oxygen", "temperature", "chla")) |>
#   count(site_id) |> filter(n==3) |> pull(site_id)
sites <- target |> na.omit() |> distinct(site_id, variable) |> 
  filter(variable %in% target_names) |> count(site_id) |> filter(n>=1) |> pull(site_id)


# Make sure assumptions are clarified 

# Define the forecasts model for a site
forecast_site <- function(site, target_names) {
  
  #target_names <- c("gcc_90", "rcc_90")
  
  message(paste0("Running site: ", site))
  weather_vars = c("TMP", "DSWRF", "RH")
  weather_vars2 = c("air_temperature", "surface_downwelling_shortwave_flux_in_air", "relative_humidity")
  model_vars = append(weather_vars, "DOY")
  
  #prediction_targets = c("temperature", "oxygen", "chla")
  prediction_targets <- target |> na.omit() |> distinct(site_id, variable) |> 
    filter(site_id == site)
  
  predicting_vars = prediction_targets[2]
  message(predicting_vars)
  
  
  # historical temperatures
  noaa_past_mean <- noaa_mean_historical(df_past, weather_vars2, weather_vars, site)
  
  # Merge in past NOAA data into the targets file, matching by date.
  site_target <- target |>
    dplyr::select(datetime, site_id, variable, observation) |>
    dplyr::filter(variable %in% target_names,
                  site_id == site) |>
    tidyr::pivot_wider(names_from = "variable", values_from = "observation") |>
    dplyr::left_join(noaa_past_mean, by = c("datetime"))
  # site_target <- target |>
  #   dplyr::select(datetime, site_id, variable, observation) |>
  #   dplyr::filter(variable %in% prediction_targets, 
  #                 site_id == site) |>
  #   tidyr::pivot_wider(names_from = "variable", values_from = "observation") |>
  #   dplyr::left_join(noaa_past_mean, by = c("datetime"))
  
  rm(noaa_past_mean) # save RAM 
  
  site_target[['DOY']] <- as.numeric( format(site_target[['datetime']], '%j'))
  site_target = na.omit(site_target)
  
  startCheck = (forecast_doy - 7) %% 365
  
  historical <- site_target %>% 
    filter(DOY <= forecast_doy) %>% 
    filter(DOY > startCheck)
  
  if(nrow(historical) < 1){
    
    # Generate Empty Forecast
    forecast <- data.frame(matrix(ncol = 8, nrow = 0))
    colnames(forecast) <- c("model_id","datetime","reference_datetime","site_id",
                            "family","parameter","variable","prediction")
    
    message("Dropped Site")
    
  } else {
    
    #  Get 30-day predicted temperature ensemble at the site
    noaa_future <- noaa_mean_forecast(site, weather_vars, noaa_date)
    noaa_future[['DOY']] <- as.numeric( format(noaa_future[['datetime']], '%j'))
    future_input <- as.matrix(noaa_future[,model_vars])
    forecast = NULL
    
    # for (prediction_target in prediction_targets){
    #   message(prediction_target)
    #   sample <- sample(c(TRUE, FALSE), nrow(site_target), replace=TRUE, prob=c(0.8,0.2))
    #   train  <- site_target[sample, ]
    #   test   <- site_target[!sample, ]
    #   
    #   # Generate our labels as the current water temperature
    #   train.label = train[[prediction_target]]
    #   test.label = test[[prediction_target]]
    #   
    #   # Convert the input data to a matrix for xgboost
    #   train.data = as.matrix(train[, model_vars])
    #   test.data = as.matrix(test[, model_vars])
    #   
    #   # Generate Training Input for XGBoost
    #   dtrain<-xgb.DMatrix(data = train.data, label = train.label)
    #   # Train our model
    #   bst <- xgboost(data = dtrain, max.depth = 10, eta = 0.3, nthread = 2, nrounds = 15, verbose = 0)
    #   # Product Predictions for our Testing Dataset
    #   test_pred <- predict(bst, test.data)
    #   # Calculate Root Mean Squared Error
    #   rmse <- sqrt(mean((test_pred-test.label)^2))
    #   message('RMSE: ', rmse)
    #   
    #   standardDev = sd(test_pred-test.label)
    #   newPredictions = predict(bst, future_input)+rnorm(nrow(future_input))*standardDev
    #   
    #   newVariableForecast <-
    #     noaa_future |>
    #     mutate(site_id = site,
    #            prediction = newPredictions,
    #            variable = prediction_target)
    #   
    #   if(is_null(forecast)){
    #     forecast <- newVariableForecast
    #   } else {
    #     forecast <- dplyr::bind_rows(forecast, newVariableForecast)
    #   }
    # }
    
    predicting_vars = unlist(predicting_vars)
    
    #for (i in 1:length(predicting_vars)){
    for(target_variable in predicting_vars){  
      #target_variable = (predicting_vars)[i]
      message(paste0(target_variable))
      sample <- sample(c(TRUE, FALSE), nrow(site_target), replace=TRUE, prob=c(0.8,0.2))
      train  <- site_target[sample, ]
      test   <- site_target[!sample, ]
      
      # Generate our labels as the current water temperature
      train.label = train[[target_variable]]
      test.label = test[[target_variable]]
      
      # Convert the input data to a matrix for xgboost
      train.data = as.matrix(train[, model_vars])
      test.data = as.matrix(test[, model_vars])
      
      # Generate Training Input for XGBoost
      dtrain<-xgb.DMatrix(data = train.data, label = train.label)
      # Train our model
      bst <- xgboost(data = dtrain, max.depth = 10, eta = 0.3, nthread = 2, nrounds = 15, verbose = 0)
      # Product Predictions for our Testing Dataset
      test_pred <- predict(bst, test.data)
      # Calculate Root Mean Squared Error
      rmse <- sqrt(mean((test_pred-test.label)^2))
      message('RMSE: ', rmse)
      
      standardDev = sd(test_pred-test.label)
      newPredictions = predict(bst, future_input)+rnorm(nrow(future_input))*standardDev
      
      newVariableForecast <-
        noaa_future |>
        mutate(site_id = site,
               prediction = newPredictions,
               variable = target_variable)
      
      if(is_null(forecast)){
        forecast <- newVariableForecast
      } else {
        forecast <- dplyr::bind_rows(forecast, newVariableForecast)
      }
    }
    
    forecast <- forecast |>
      mutate(reference_datetime = forecast_date,
             family = "ensemble",
             model_id = model_id) |>
      rename(parameter = ensemble) |>
      select(model_id, datetime, reference_datetime,
             site_id, family, parameter, variable, prediction)
    
  }
}

# Run all sites -- may be slow!
forecast <- map_dfr(sites, forecast_site, target_names)

#Forecast output file name in standards requires for Challenge.
# csv.gz means that it will be compressed
file_date <- Sys.Date() 
forecast_file <- paste0("phenology","-",file_date,"-",model_id,".csv.gz")

#Write csv to disk
write_csv(forecast, forecast_file)

neon4cast::forecast_output_validator(forecast_file)

# Submit forecast

#neon4cast::submit(forecast_file = forecast_file, metadata = NULL, ask = FALSE)


