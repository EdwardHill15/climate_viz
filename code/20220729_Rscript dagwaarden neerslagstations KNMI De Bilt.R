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


date1 <- 20220729
str(date1)
date1 <- as.character(date1)
date1

date1 <- as.Date(date1)
date2 <- ymd(date1)
str(date2)
year(date2)
Sys.setlocale("LC_TIME", "English")
month(date2, label = TRUE, abbr = FALSE)
?month

url <- ("https://cdn.knmi.nl/knmi/map/page/klimatologie/gegevens/monv_reeksen/neerslaggeg_DE-BILT_550.zip")



library(readr)
neerslaggeg_De_Bilt <- read_delim("neerslaggeg_De-Bilt.csv", 
                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)
df <- neerslaggeg_De_Bilt

df <- df[-1]
colnames(df) <- c("date", "prcp", "snow")
df
# Vervang NA in data met 0
df[is.na(df)] <- 0




df_snow$date <- as.character(df_snow$date)
df_snow$date <- ymd(df_snow$date)

this_year <- year(today())
this_month <- month(today(), label = TRUE, abbr = FALSE)
this_day <- ordinal(day(today()))

tail(df)
library(psych)
describe(df_snow)

# Sneeuwval in Nederland van 1897 tot 20 juni 2022

df_snow

df_snow <- df[-2]

df_snow$snow[df_snow$snow %in% c(996:999)] <- 1

describe(df_snow)

df_snow %>% 
  mutate(year = year(date),
         month = month(date),
         day = day(date)) %>% 
  filter(!(month == 2 & day == 29)) %>% 
  group_by(year) %>% 
  mutate(cum_snow = cumsum(snow)) %>%
  ungroup() %>% 
  ggplot(aes(x=year, y=cum_snow, group = year, col = cum_snow)) +
  geom_line(size = 4) +
  scale_x_continuous(breaks = seq(1955, 2022, 5),
                     labels = seq(1955, 2022, 5),
                     limits = c(1948, 2022)) +
  scale_color_gradient2(low = "darkred", mid = "White", high = "red",
                       midpoint = 50) +
  scale_y_continuous(breaks = seq(0, 500, 100),
                     labels = seq(0, 500, 100),
                     limits = c(0,500),
                     expand = c(0,0)) +
  labs(x = NULL,
       y = "Cumulative snow (cm)",
       title = glue("Through June 20th, the <span style = 'color: red'>cumulative</span> <span style = 'color: lightblue'>snow</span> in The Netherlands near De Bilt to {this_year}")) +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(margin=margin(b=10), size = 16, color = "white"),
    axis.title.y = element_text(color = "white"),
    panel.background = element_rect(fill = "black"),
    axis.line = element_line(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "black", color = "black"),
    axis.text = element_text(color = "white", size = 8),
    legend.background = element_rect(fill = "black"),
    legend.text = element_text(color = "white", size = 6),
    legend.title = element_text(color = "white"),
    legend.position = "bottom")
  


ggsave("figures/cumulative_prcp_ne_black2.png", width = 6, height = 5)

ggsave("figures/cumulative_prcp_ne_black_snow.png", width = 6, height = 5)


