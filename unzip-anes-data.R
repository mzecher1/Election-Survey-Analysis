library(zip)
library(fs)

# If the .data file doesn't already exist, then unzip to create it

if (!file_exists(path("data-raw", "anes_timeseries_cdf_stata13.dta"))) {
  unzip(
    zipfile = path("data-raw", "anes_timeseries_cdf_dta.zip"), 
    exdir = path("data-raw"),
    junkpaths = TRUE
  )
}
