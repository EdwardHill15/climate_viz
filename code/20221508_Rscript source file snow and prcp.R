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
local_weather <- df
