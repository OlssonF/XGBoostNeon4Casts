library(tidyverse)
library(neon4cast)
library(lubridate)
library(rMR)
library(glue)
library(xgboost)

source("R/ignore_sigpipe.R")
source("R/forecast_site.R")

forecast_date <- Sys.Date()
forecast_doy = as.numeric(format(forecast_date, '%j'))
noaa_date <- Sys.Date() - days(1)  #Need to use yesterday's NOAA forecast because today's is not available yet

################# Things To Change ##########################

theme = "phenology"

# Define a unique name which will identify your model in the leaderboard and connect it to team members info, etc
model_id <- "xgboost_parallel"

# Download latest target data and site description data
target <- readr::read_csv("https://data.ecoforecast.org/neon4cast-targets/phenology/phenology-targets.csv.gz", guess_max = 1e6) |> 
  na.omit()

target_variables <- c("gcc_90", "rcc_90")
weather_vars = c("TMP", "DSWRF", "RH")
weather_vars2 = c("air_temperature", "surface_downwelling_shortwave_flux_in_air", "relative_humidity")

###############################################################

# Get meteorological predictions as drivers
df_past <- neon4cast::noaa_stage3()

# We'll skip any site that doesn't have one of our target variables
sites <- target |> na.omit() |> distinct(site_id, variable) |> 
  filter(variable %in% target_variables) |> count(site_id) |> filter(n>=1) |> pull(site_id)

# Run all sites -- may be slow!
forecast <- map_dfr(sites, forecast_site, target_variables, target, weather_vars, weather_vars2, df_past, forecast_date)

# Forecast output file name in standards requires for Challenge.
# csv.gz means that it will be compressed
file_date <- Sys.Date() 
forecast_file <- paste0(theme,"-",file_date,"-",model_id,".csv.gz")

# Write csv to disk
write_csv(forecast, forecast_file)

# Validate forecast
neon4cast::forecast_output_validator(forecast_file)

# Submit forecast
#neon4cast::submit(forecast_file = forecast_file, metadata = NULL, ask = FALSE)


