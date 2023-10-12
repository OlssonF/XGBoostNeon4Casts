library(tidyverse)
library(neon4cast)
library(lubridate)
library(rMR)
library(glue)
library(xgboost)

source("R/ignore_sigpipe.R")
source("R/forecast_site.R")


# Get list of dates to rerun
# Issue fixed on Oct 9, 2023 so only need to rerun up to that date
# Check dates that are in the bucket already

# Dates of forecasts 
end <- as_date('2023-10-09')
start <- as_date('2023-01-01')
this_year <- data.frame(date = as.character(paste0(seq.Date(start, end, by = 'day'), ' 00:00:00')),
                        exists = NA,
                        already_rerun = F)

challenge_model_name <- "xgboost_parallel"

# what forecasts have already been submitted?
challenge_s3_region <- "data"
challenge_s3_endpoint <- "ecoforecast.org"

Sys.unsetenv("AWS_ACCESS_KEY_ID")
Sys.unsetenv("AWS_SECRET_ACCESS_KEY")
Sys.unsetenv("AWS_DEFAULT_REGION")
Sys.unsetenv("AWS_S3_ENDPOINT")

# is that file present in the bucket?
for (i in 1:nrow(this_year)) {
  forecast_file <- paste0('aquatics-', as_date(this_year$date[i]), '-', challenge_model_name, '.csv.gz')
  
  this_year$exists[i] <- suppressMessages(aws.s3::object_exists(object = file.path("raw", 'aquatics', forecast_file),
                                                                bucket = "neon4cast-forecasts",
                                                                region = challenge_s3_region,
                                                                base_url = challenge_s3_endpoint))
  
  # if it is present, has it been submitted recently?
  if (this_year$exists[i]) {
    modified <- attr(suppressMessages(aws.s3::head_object(object = file.path("raw", 'aquatics', forecast_file),
                                                                bucket = "neon4cast-forecasts",
                                                                region = challenge_s3_region,
                                                                base_url = challenge_s3_endpoint)), 
                     "last-modified")
    
    this_year$already_rerun[i] <- ifelse(parse_date_time(gsub('GMT', '', str_split_1(modified, ', ')[2]),
                                                      orders = "%d %b %Y %H:%M:%S") >= end, 
                                      T, F)
  }
}

# which dates do you need to generate forecasts for?
# those that are missing or haven't been submitted
missed_dates <- this_year |> 
  filter(!(exists == T &  already_rerun == T)) |> 
  pull(date) |> 
  as_date()



# Run forecasts ----------------------------
for (i in 1:length(missed_dates)) {
  forecast_date <- missed_dates[i]
  
  message('Running ', forecast_date)
  forecast_doy = as.numeric(format(forecast_date, '%j'))
  noaa_date <- forecast_date - days(1)  #Need to use yesterday's NOAA forecast because today's is not available yet
  
  ################# Things To Change ##########################
  
  theme = "aquatics"
  
  # Define a unique name which will identify your model in the leaderboard and connect it to team members info, etc
  model_id <- "xgboost_parallel"
  
  # Download latest target data and site description data
  target <- readr::read_csv(paste0("https://data.ecoforecast.org/neon4cast-targets/",
                                   "aquatics/aquatics-targets.csv.gz"), guess_max = 1e6) %>% 
    filter(datetime <= forecast_date)
  
  target_variables <- c("oxygen", "temperature", "chla")
  weather_vars = c("TMP", "DSWRF", "RH")
  weather_vars2 = c("air_temperature", "surface_downwelling_shortwave_flux_in_air", "relative_humidity")
  
  ###############################################################
  
  # Get meteorological predictions as drivers
  df_past <- neon4cast::noaa_stage3()
  
  # We'll skip any site that doesn't have one of our target variables
  sites <- target |>
    na.omit() |>
    distinct(site_id, variable) |> 
    filter(variable %in% target_variables) |> 
    count(site_id) |> 
    filter(n>=1) |> 
    pull(site_id)
  
  # Run all sites -- may be slow!
  forecast <- map_dfr(sites, forecast_site, target_variables, target, weather_vars, weather_vars2, df_past, forecast_date)
  
  # Forecast output file name in standards requires for Challenge.
  # csv.gz means that it will be compressed
  file_date <- unique(forecast$reference_datetime) 
  forecast_file <- paste0(theme,"-",file_date,"-",model_id,".csv.gz")
  
  # Write csv to disk
  write_csv(forecast, forecast_file)
  
  # Validate forecast
  neon4cast::forecast_output_validator(forecast_file)
  
  # Submit forecast
  neon4cast::submit(forecast_file = forecast_file, metadata = NULL, ask = FALSE)
  
  # -------------------------------------------------------------------------
  
  
  
}
