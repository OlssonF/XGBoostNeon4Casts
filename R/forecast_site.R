source("R/noaa_mean_forecast.R")
source("R/noaa_mean_historical.R")


# Make sure assumptions are clarified 
# Model will Use Day of Year and Weather Variables as inputs 

# Define the forecasts model for a site

forecast_site <- function(site, target_variables, target, weather_vars, weather_vars2, df_past, forecast_date) {
  
  
  #target_variables = parameters[1]
  #target = parameters[2]
  #weather_vars = parameters[3]
  #weather_vars2 = parameters[4]
  #df_past = parameters[5]
  message(paste0("Running site: ", site))
  model_vars = append(weather_vars, "DOY")
  
  prediction_targets <- target |> na.omit() |> distinct(site_id, variable) |> 
    filter(site_id == site)
  
  predicting_vars = prediction_targets[2]
  message(predicting_vars)
  
  # Get historical weather variable values
  noaa_past_mean <- noaa_mean_historical(df_past, weather_vars2, weather_vars, site)
  
  # Merge in past NOAA data into the targets file, matching by date.
  site_target <- target |>
    dplyr::select(datetime, site_id, variable, observation) |>
    dplyr::filter(variable %in% target_variables,
                  site_id == site) |>
    tidyr::pivot_wider(names_from = "variable", values_from = "observation") |>
    dplyr::left_join(noaa_past_mean, by = c("datetime"))
  
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
    
    predicting_vars = unlist(predicting_vars)
    
    for(target_variable in predicting_vars){  
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
