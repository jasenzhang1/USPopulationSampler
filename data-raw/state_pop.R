library(here)
library(sf)
## code to prepare `state_pop` dataset goes here

url <- 'https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_all_500k.zip'
url <- 'https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_state_500k.zip'

zipfile <- 'cb_2020_us_state_500k.zip'

download.file(url, zipfile, method = "curl")

unzip_dir <- "data-raw"
unzip(zipfile, exdir = unzip_dir)


# Assuming the dataset is a CSV file, read it into R
# Replace 'dataset.csv' with the actual file name if different
state_pop <- st_read('data-raw/cb_2020_us_state_500k.shp')



url2 <- 'https://api.census.gov/data/2020/dec/cd118?get=group(P1)&ucgid=pseudo(0100000US$0400000)'
dest_dir <- 'raw-data'
destfile <- file.path(dest_dir, "state_pop.csv")

download.file(url2, destfile)

usethis::use_data(state_pop, overwrite = TRUE)
