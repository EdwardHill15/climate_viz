# Scraping weather data from the internet with R and the tidyverse (CC231)
# https://riffomonas.org/code_club/2022-07-18-local-weather


# Finding a Weather Station
# https://ncei.noaa.gov/cdo-web/
library(tidyverse)
library(glue)
library(lubridate)


inventory_url <- "https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt"

inventory <- read_table(inventory_url, 
           col_names = c("station", "lat", "lon", "variable", "start", "end"))
inventory

# Now finding the closest weather station

# Latitude and longitude for Zwaag (Klaproos)



my_lat <- 52.13 * 2 * pi / 360
my_lon <- 5.29 * 2 * pi / 360

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

# mutate miles into km with * 1.609344
my_station <- inventory %>% 
  mutate(lat_r = lat*2*pi/360,
         lon_r = lon*2*pi/360,
         d = 1.609344 * 3963 * acos((sin(lat_r) * sin(my_lat)) + 
                           cos(lat_r) * cos(my_lat) *
                           cos(my_lon - lon_r))) %>% 
  filter(start < 1960) %>% 
  top_n(n = -1, d) %>% 
  distinct(station) %>% 
  pull(station)

# Get and tidy local weather station data (from: "NLE00100501")

station_daily <- glue("https://www.ncei.noaa.gov/pub/data/ghcn/daily/by_station/{my_station}.csv.gz")

df <- read_csv(station_daily,
         col_names = c("station", "date", "variable", "value", "a", "b", "c", "d")) %>% 
  select(date, variable, value) %>% 
  pivot_wider(names_from = "variable", values_from = "value",
              values_fill = 0) %>% 
  select(date, TMAX, PRCP)

# ELEMENT    is the element type.   There are five core elements as well as a number
# of addition elements.  
# 
# The five core elements are:
#   
#   PRCP = Precipitation (tenths of mm)
# SNOW = Snowfall (mm)
# SNWD = Snow depth (mm)
# TMAX = Maximum temperature (tenths of degrees C)
# TMIN = Minimum temperature (tenths of degrees C)


df <- df %>% 
  mutate(date = ymd(date))
  
local_weather <- df  
tail(local_weather)
local_weather %>% 
  mutate(t_max = TMAX / 10) %>% 
  ggplot(aes(x=date, y=t_max)) +
  geom_point(size = 4, color = "red") +
  geom_smooth(method = "lm", formula = y ~x)

t_diff <- local_weather %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarize(mean = mean(TMAX / 10)) %>% 
  ggplot(aes(x=year, y=mean, color = mean)) +
  geom_point(size = 9, alpha = .7) +
  geom_smooth(method = "lm", formula = y ~x, se = FALSE, color = "gray") +
  scale_color_viridis_c(option = ("magma")) +
  # scale_color_gradient2(low = "blue", mid = "white", high = "red",
  #                       midpoint = 13.5) + 
#coord_cartesian(xlim=c(10,16)) +
#coord_cartesian(expand=TRUE) +
scale_y_continuous(breaks = seq(10, 16, 2)) +
labs(y = "Temperature in (\u00B0 C)", 
     x = NULL,
     title = "Rise of temperature in The Netherlands between 1951 - 2008",
     subtitle = "Temperature is colored by the size of the mean temperature value",
     caption = "Source: https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt") +
  theme(
    plot.background = element_rect(fill = "black", color = "black"),
    panel.background = element_rect(fill = "black"),
    plot.title = element_text(color = "white", face = "bold", size = 25),
    plot.subtitle = element_text(color = "gray", size =15),
    plot.title.position = "plot",
    plot.caption = element_text(color = "white", size = 12),
    axis.text = element_text(color = "white", size = 15),
    axis.title = element_text(color = "white", size = 15),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray", size = 0.25),
    panel.grid.minor.y = element_blank(),
    axis.ticks = element_blank(),
    legend.background = element_rect(fill = "black"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    legend.position = "bottom")
library(ggplot2)
ggsave("rise_temp_netherlands.png", height = 6, width = 11)

  