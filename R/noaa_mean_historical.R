## Helper function: for each site, average over predicted 0h horizon ensembles to get 'historic values'
noaa_mean_historical <- function(df_past, vars, rename, site) {
  #vars <- c("air_temperature", "surface_downwelling_shortwave_flux_in_air", "relative_humidity")
  #rename <- c("air_temperature", "DSWRFr", "RH")
  df_past |>
    dplyr::filter(site_id == site,
                  variable %in% vars) |>
    dplyr::rename(ensemble = parameter) |>
    dplyr::select(datetime, variable, prediction, ensemble) |>
    dplyr::mutate(date = as_date(datetime)) |>
    dplyr::group_by(date, variable) |>
    dplyr::summarize(prediction = mean(prediction, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::collect() |>
    pivot_wider(names_from = variable, values_from = prediction) |>
    dplyr::rename(datetime = date) |>
    dplyr::mutate(air_temperature = air_temperature - 273.15) |>
    dplyr::rename_with(~ rename, all_of(vars))
}
