## code to prepare `DATASET` dataset goes here

library(tidyverse)
library(sf) #spatial package
library(here)

source('C:/Users/jasen/OneDrive - UCLA IT Services/School/Research/NYT_Covid/Code/helper_read.R')
dir <- 'C:/Users/jasen/OneDrive - UCLA IT Services/School/Research/NYT_Covid/Code'

state_shape_2020 <- read_shape(dir, 1, 2020)
county_shape_2020 <- read_shape(dir, 2, 2020)

state_pop <- read_subcounty_pop_V2(dir, 1)
county_pop <- read_subcounty_pop_V2(dir, 2)

state_convert <- state_pop %>% inner_join(state_shape_2020, by = c('STATEFP', 'NAME')) %>%
  select('NAME', 'STATEFP', 'STUSPS')

county_convert <- county_pop %>% inner_join(county_shape_2020, by = c('STATEFP', 'COUNTYFP')) %>%
  select('STATEFP', 'COUNTYFP', 'NAME.y', 'NAME.x', 'STATE_NAME', 'STUSPS') %>%
  mutate(FIPS = paste0(STATEFP, COUNTYFP))

# use_data(state_convert, internal = T, overwrite = TRUE)
# use_data(county_convert, internal = T, overwrite = TRUE)

usethis::use_data(state_convert, overwrite = TRUE)
usethis::use_data(county_convert, overwrite = TRUE)
