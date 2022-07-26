# Scraping weather data from the internet with R and the tidyverse (CC231)
# https://riffomonas.org/code_club/2022-07-18-local-weather


# Finding a Weather Station
# https://ncei.noaa.gov/cdo-web/
library(tidyverse)
library(glue)
library(lubridate)
library(patchwork)

inventory_url <- "https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt"

inventory <- read_table(inventory_url, 
           col_names = c("station", "lat", "lon", "variable", "start", "end"))
inventory

# Now finding the closest weather station
# Zwaag, klaproos 23

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
  filter(start < 1960 & end > 2020) %>% 
  top_n(n = -1, d) %>% 
  distinct(station) %>% 
  pull(station)

# Get and tidy local weather station data (from: "NLM00006260")
# KNMI De Bilt: https://www.knmi.nl/over-het-knmi/over

station_daily <- glue("https://www.ncei.noaa.gov/pub/data/ghcn/daily/by_station/{my_station}.csv.gz")

df <- read_csv(station_daily,
         col_names = c("station", "date", "variable", "value", "a", "b", "c", "d")) %>% 
  select(date, variable, value) %>% 
  pivot_wider(names_from = "variable", values_from = "value",
              values_fill = 0) %>% 
  select(date, TMAX, TMIN, PRCP)

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
  
#local_weather <- df  
# tail(local_weather)
# local_weather %>% 
#   mutate(t_max = TMAX / 10) %>% 
#   ggplot(aes(x=date, y=t_max)) +
#   geom_point(size = 4, color = "red") +
#   geom_smooth(method = "lm", formula = y ~x)

df
NE_max <- aggregate(TMAX~year(date), df, max)
NE_max$TMAX <- NE_max$TMAX / 10
colnames(NE_max) <- c("year", "temp")




# NE_max %>% 
#   ggplot(aes(x=year, y=temp, col=temp)) +
#   geom_point(size = 9, alpha = 0.7)
# 
# # t_diff <- local_weather %>% 
# #   mutate(year = year(date)) %>% 
# #   group_by(year, TMAX) %>% 
# #   summarize(max = max(TMAX / 10)) %>% 
# p1 <- NE_max %>% 
#   ggplot(aes(x=year, y=temp, color = temp)) +
#   geom_point(size = 9, alpha = .7) +
#   geom_smooth(method = "lm", formula = y ~x, se = FALSE, color = "gray") +
#   scale_color_viridis_c(option = ("magma")) +
#   # scale_color_gradient2(low = "blue", mid = "white", high = "red",
#   #                       midpoint = 13.5) + 
# #coord_cartesian(xlim=c(10,16)) +
# #coord_cartesian(expand=TRUE) +
#   scale_y_continuous(breaks = seq(27.5, 37.5, 2.5),
#                    limits = c(26, 37.5)) +
#   scale_x_continuous(breaks = seq(1901, 2022, 11),
#                      limits = c(1901, 2023)) +
#   labs(y = "Temperature in (\u00B0 C)", 
#      x = NULL,
#      title = "Maximum temperature") + 
#      # title = "Maximum Temperature in The Netherlands between 1901 and 2022"
#      # subtitle = "Temperature is colored by the size of the maximum temperature value",
#      # caption = "Source: KNMI weather station De Bilt in The Netherlands (NLM00006260)") +
#   theme(
#     plot.background = element_rect(fill = "black", color = "black"),
#     panel.background = element_rect(fill = "black"),
#     plot.title = element_text(color = "white", face = "bold", size = 25, hjust = 0.5),
#     plot.subtitle = element_text(color = "gray", size =15),
#     plot.title.position = "plot",
#     plot.caption = element_text(color = "white", size = 12),
#     axis.text = element_text(color = "white", size = 15),
#     axis.title = element_text(color = "white", size = 15),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.y = element_line(color = "gray", size = 0.25),
#     panel.grid.minor.y = element_blank(),
#     axis.ticks = element_blank(),
#     legend.background = element_rect(fill = "black"),
#     legend.text = element_text(color = "white", size = 6),
#     legend.title = element_text(color = "white"),
#     legend.position = "bottom")
# library(ggplot2)
# 
# NE_mean <- aggregate(TMAX~year(date), df, mean)
# NE_mean$TMAX <- NE_mean$TMAX / 10
# colnames(NE_mean) <- c("year", "temp")
# NE_mean$temp <- round(NE_mean$temp, 1)
# library(tidyverse)
# summarize(NE_mean$temp)
# glimpse(NE_mean)
# min(NE_mean$temp)
# max(NE_mean$temp)
# p2 <- NE_mean %>% 
#   ggplot(aes(x=year, y=temp, color = temp)) +
#   geom_point(size = 9, alpha = .7) +
#   geom_smooth(method = "lm", formula = y ~x, se = FALSE, color = "gray") +
#   scale_color_viridis_c(option = ("viridis")) +
#   # scale_color_gradient2(low = "blue", mid = "white", high = "red",
#   #                       midpoint = 13.5) + 
#   #coord_cartesian(xlim=c(10,16)) +
#   #coord_cartesian(expand=TRUE) +
#   scale_y_continuous(breaks = seq(11.5, 16, 1.5),
#                      limits = c(11.5, 16)) +
#   scale_x_continuous(breaks = seq(1901, 2022, 11),
#                      limits = c(1901, 2023)) +
#   labs(y = "Temperature in (\u00B0 C)", 
#        x = NULL,
#        title = "Mean temperature") +
#        # title = "Mean Temperature in The Netherlands between 1901 and 2022"
#        # subtitle = "Temperature is colored by the size of the mean temperature value",
#        # caption = "Source: KNMI weather station De Bilt in The Netherlands (NLM00006260)") +
#   theme(
#     plot.background = element_rect(fill = "black", color = "black"),
#     panel.background = element_rect(fill = "black"),
#     plot.title = element_text(color = "white", face = "bold", size = 25, hjust = 0.5),
#     plot.subtitle = element_text(color = "gray", size =15),
#     plot.title.position = "plot",
#     plot.caption = element_text(color = "white", size = 12),
#     axis.text = element_text(color = "white", size = 15),
#     axis.title = element_text(color = "white", size = 15),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.y = element_line(color = "gray", size = 0.25),
#     panel.grid.minor.y = element_blank(),
#     axis.ticks = element_blank(),
#     legend.background = element_rect(fill = "black"),
#     legend.text = element_text(color = "white", size = 6),
#     legend.title = element_text(color = "white"),
#     legend.position = "bottom")
# 
# NE_prcp <- aggregate(PRCP~year(date), df, max)
# NE_prcp$PRCP <- NE_prcp$PRCP / 10
# colnames(NE_prcp) <- c("year", "prcp")
# NE_prcp$prcp <- round(NE_prcp$prcp, 1)
# 
# min(NE_prcp$prcp)
# max(NE_prcp$prcp)
# summarize(NE_prcp)
# 
# NE_prcp <- NE_prcp[-c(1:5),]
# p3 <- NE_prcp %>% 
#   ggplot(aes(x=year, y=prcp, color = prcp)) +
#   geom_point(size = 9, alpha = .7) +
#   geom_smooth(method = "lm", formula = y ~x, se = FALSE, color = "gray") +
#   scale_color_viridis_c(option = ("mako")) +
#   # scale_color_gradient2(low = "blue", mid = "white", high = "red",
#   #                       midpoint = 13.5) + 
#   #coord_cartesian(xlim=c(10,16)) +
#   #coord_cartesian(expand=TRUE) +
#   scale_y_continuous(breaks = seq(15, 65, 10),
#                      limits = c(15, 65)) +
#   scale_x_continuous(breaks = seq(1901, 2022, 11),
#                      limits = c(1901, 2023)) +
#   labs(y = "Precipitation in mm.", 
#        x = NULL,
#        title = "Precipitation") +
#        # title = Precipitation in The Netherlands between 1901 and 2022"
#        # subtitle = "Precipitation is colored by the size of the maximum precipitation in mm. value",
#        # caption = "Source: KNMI weather station De Bilt in The Netherlands (NLM00006260)") +
#   theme(
#     plot.background = element_rect(fill = "black", color = "black"),
#     panel.background = element_rect(fill = "black"),
#     plot.title = element_text(color = "white", face = "bold", size = 25, hjust = 0.5),
#     plot.subtitle = element_text(color = "gray", size =15),
#     plot.title.position = "plot",
#     plot.caption = element_text(color = "white", size = 12),
#     axis.text = element_text(color = "white", size = 15),
#     axis.title = element_text(color = "white", size = 15),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.y = element_line(color = "gray", size = 0.25),
#     panel.grid.minor.y = element_blank(),
#     axis.ticks = element_blank(),
#     legend.background = element_rect(fill = "black"),
#     legend.text = element_text(color = "white", size = 6),
#     legend.title = element_text(color = "white"),
#     legend.position = "bottom")
# 
# library(patchwork)
# col1 <- (plot_spacer() / p3 /plot_spacer()) +
#   plot_layout(heights = c(0.05, 0.9, 0.05))
# col2 <- (p1 / p2) + plot_layout(heights=c(1,1))
# 
# (col1 | col2) + plot_annotation(
#   title = "Temperature and Precipitation in The Netherlands between 1901 and 2022",
#   subtitle = "Temperature and Precipitation is colored by the size of the value",
#   caption = "Source: KNMI weather station De Bilt in The Netherlands (NLM00006260)",
#   tag_levels = c('A', '1'), tag_prefix = 'Fig. ',
#   tag_sep = '.', tag_suffix = ':') &
#   theme(plot.tag = element_text(color = "white"),
#         plot.title = element_text(size = 35),
#         plot.subtitle = element_text(size = 25),
#         plot.caption = element_text(size = 15))
# 
# ((plot_spacer() / p3 / plot_spacer()) | (p1 / p2)) + plot_annotation(
#   title = "Temperature and Precipitation in The Netherlands between 1901 and 2022",
#   subtitle = "Temperature and Precipitation is colored by the size of the value",
#   caption = "Source: KNMI weather station De Bilt in The Netherlands (NLM00006260)",
#   tag_levels = c('A', '1'), tag_prefix = 'Fig. ',
#                                  tag_sep = '.', tag_suffix = ':') &
#   theme(plot.tag = element_text(color = "white"),
#         plot.title = element_text(size = 35),
#         plot.subtitle = element_text(size = 25),
#         plot.caption = element_text(size = 15)) +
#   plot_layout(heights = unit(c(3,2))
# 
# ggsave("rise_temp_max_netherlands.png", height = 6, width = 11)
# ggsave("rise_temp_mean_netherlands.png", height = 6, width = 11)
# ggsave("precipitation_netherlands.png", height = 6, width = 11)
# ggsave("knmi_all.png", height = 10, width = 18)
#   