Gregory Harrison, Spring 2023 Research 

This Repo contians the code that generates xgboost forecasts for the EFI NEON Challenge. 

Files: 
Aquatics_ParallelXGB.R: Script that produces forecasts for all variables of the aquatics theme
Phenology_ParallelXGB.R: Script that produces forecasts for all variables of the phenology theme
Terrestrial_ParallelXGB.R: Script that produces forecasts for all variables of the terrestrial theme
PresentationSP23.qmd: Final Presentation generating markdown

Generic_ParallelXGB copy.R: Script that should be used as a template for producing new forecast models. Changes to theme, input weather varaibles, target variables, and model id can all be made within lines 17 and 28. 

R helper fuctions: 
R/forecast_site.R: Function that actually trains the xgboost model on data, target variables, etc.
R/noaa_mean_forecast.R: Get daily average temperature from each ensemble in future
R/noaa_mean_historical.R: For each site, average over predicted 0h horizon ensembles to get 'historic values'

Special thanks to Dr. R. Quinn Thomas, Dr. Freya Olsson, and the rest of the Ecological Forecasting Project at Virginia Tech for their assistance on this project. 
