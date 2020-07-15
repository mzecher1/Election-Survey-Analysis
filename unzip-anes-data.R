library(zip)
library(fs)

dir_create(path("data-raw", "anes-codebook"))

# If the .dta file doesn't already exist, then unzip to create it

if (!file_exists(path("data-raw", "anes_timeseries_cdf_stata13.dta"))) {
  unzip(
    zipfile = path("data-raw", "anes_timeseries_cdf_dta.zip"), 
    exdir = path("data-raw"),
    junkpaths = TRUE
  )
}

if (!file_exists(path("data-raw", "anes_timeseries_cdf_stata13.dta"))) {
  unzip(
    zipfile = path("data-raw", "anes_timeseries_cdf_codebook.zip"), 
    exdir = path("data-raw", "anes-codebook"),
    junkpaths = TRUE
  )
}

codebook_zip_file <- path("data-raw", "anes_timeseries_cdf_codebook.zip")
if (file_exists(codebook_zip_file)) {
  file_delete(codebook_zip_file)
}