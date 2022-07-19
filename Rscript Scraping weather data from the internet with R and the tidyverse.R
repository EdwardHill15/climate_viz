# Scraping weather data from the internet with R and the tidyverse (CC231)
# https://riffomonas.org/code_club/2022-07-18-local-weather


# Finding a Weather Station
# https://ncei.noaa.gov/cdo-web/
library(tidyverse)
library(glue)


inventory_url <- "https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt"

inventory <- read_table(inventory_url, 
           col_names = c("station", "lat", "lon", "variable", "start", "end"))
inventory

# Now finding the closest weather station

# Latitude and longitude for Zwaag (Klaproos)

my_lat <- 52.665949365611574 * 2 * pi / 360
my_lon <- 5.041965353431656 * 2 * pi / 360

# go to google search and type; 'calculate distance between two latitude longitude points'

# Distance, d = 3963.0 * arccos[(sin(lat1) * sin(lat2)) + cos(lat1) * cos(lat2) * cos(long2 – long1)]
# 
# The obtained distance, d, is in miles. If you want your value to be in units of kilometers, multiple d by 1.609344.
# d in kilometers = 1.609344 * d in miles

# first calculate lat and lon in radiants with mutate:

inventory %>% 
  mutate(lat_r = lat*2*pi/360,
         lon_r = lon*2*pi/360,
         d = 3963 * acos((sin(lat_r) * sin(my_lat)) + 
                           cos(lat_r) * cos(my_lat) *
                           cos(my_lon - lon_r)))





