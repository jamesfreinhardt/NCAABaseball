# Got it ✅ — you want to download climate data in R and then aggregate it by U.S. ZIP code to get average temperature and precipitation.
# This requires two main steps:
#   
#   Get climate data for each ZIP code’s coordinates.
# Aggregate by ZIP code.
# 
# Since ZIP codes are not weather stations, we’ll:
#   
#   Use a ZIP code → latitude/longitude lookup table.
# Pull climate data for each coordinate (e.g., from NASA POWER via nasapower — no station IDs needed).
# Compute averages.
# 

#Complete R Example
# Install required packages
packages <- c("tidyverse", "zipcodeR", "nasapower")
install_if_missing <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
lapply(packages, install_if_missing)

library(tidyverse)
library(zipcodeR)
library(nasapower)

library(tidyverse)
library(zipcodeR)
library(nasapower)

Sch_Location <- read.csv("input.csv") %>% select(unitid, latitude, longitude)


# Function to get NASA POWER daily climate data for a ZIP code
get_zip_climate <- function(unitid, lat, lon) {
  tryCatch({
    data <- get_power(
      community = "AG",
      lonlat = c(lon, lat),
      pars = c("T2M"),   #, "CLOUD_AMT","PRECTOTCORR"),
      temporal_api = "climatology")
      
    
    data %>% cbind(unitid)
      
  }, error = function(e) {
    message(sprintf("Failed for ZIP %s: %s", zip, e$message))
    # Return empty tibble with correct columns
    tibble(zipcode = zip,  T2M = numeric(), CLOUD_AMT = numeric(), PRECTOTCORR = numeric())
  })
}



# Save results
write.csv(zip_summary, "zip_avg_climate.csv", row.names = FALSE)

head(zip_summary)


# How It Works
# 
# zipcodeR: Gets ZIP code coordinates.
# nasapower: Downloads daily temperature (T2M) and precipitation (PRECTOT) for given coordinates.
# Loop + pmap: Iterates over ZIP codes to fetch data.
# Aggregation: Computes mean temperature and precipitation per ZIP.
# 
# 
# Performance Notes
# 
# The U.S. has ~42,000 ZIP codes — fetching all will take hours and may hit API limits.
# For nationwide data, it’s better to:
#   
#   Download gridded climate data (e.g., TerraClimate via climateR).
# Spatially join ZIP code polygons to the grid for faster processing.
