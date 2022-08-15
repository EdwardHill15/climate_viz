library(tidyverse)
library(lubridate)
library(readr)
library(glue)
library(scales)
library(ggtext)
library(psych)

# Dagwaarden neerslagstations
# Download tijdreeksen van 670 KNMI-neerslagstations
# https://www.knmi.nl/nederland-nu/klimatologie/monv/reeksen#Z

Sys.setlocale("LC_TIME", "English")

url <- ("https://cdn.knmi.nl/knmi/map/page/klimatologie/gegevens/monv_reeksen/neerslaggeg_DE-BILT_550.zip")

library(readr)
neerslaggeg_De_Bilt <- read_delim("C:/Git projects/climate_viz/data/neerslaggeg_De-Bilt.csv", 
                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)
df <- neerslaggeg_De_Bilt
df <- df[-1]
colnames(df) <- c("date", "prcp", "snow")

# Vervang NA in data met 0
df[is.na(df)] <- 0
df
df <- df %>% 
  mutate(date = ymd(date))
local_weather <- df

prcp_snow_annual <- local_weather %>% 
filter(snow > 0) %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarize(prcp = sum(prcp),
            snow = sum(snow))
prcp_snow_annual %>% 
  pivot_longer(-year) %>% 
  ggplot(aes(x=year, y=value)) +
  geom_line() +
  facet_wrap(~name, ncol = 1, scales = "free_y")


prcp_snow %>% 
  ggplot(aes(x=prcp, y=snow, color = year)) +
  geom_point(size = 2)

cor.test(prcp_snow_annual$prcp, prcp_snow_annual$snow)
