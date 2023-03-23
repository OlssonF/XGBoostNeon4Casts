noaa_mean_forecast <- function(site, vars, reference_date) {
  #variables <- c("TMP", "DSWRF", "RH")
  endpoint = "data.ecoforecast.org"
  bucket <- glue::glue("neon4cast-drivers/noaa/gefs-v12/stage1/0/{reference_date}")
  s3 <- arrow::s3_bucket(bucket, endpoint_override = endpoint, anonymous = TRUE)
  
  # stage1 air temp is Celsius
  noaa_mean_forecast <- arrow::open_dataset(s3) |>
    dplyr::filter(site_id == site,
                  datetime >= lubridate::as_datetime(forecast_date),
                  variable %in% vars) |>
    dplyr::select(datetime, variable, prediction, parameter) |>
    dplyr::mutate(datetime = as_date(datetime)) |>
    dplyr::group_by(datetime, variable, parameter) |>
    dplyr::summarize(prediction = mean(prediction), .groups = "drop") |>
    dplyr::collect() |>
    pivot_wider(names_from = variable, values_from = prediction) |>
    dplyr::rename(ensemble = parameter) 
}