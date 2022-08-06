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


df_prcp <- df[-3]
df_snow <- df[-2]
df_snow
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


df_prcp$date <- as.character(df_prcp$date)
df_prcp$date <- ymd(df_prcp$date)
today_date <- ymd(today())

this_year <- year(local_weather$date)
today_month <- month(today())
today_day <- day(today())
today_date <- ymd(glue("2020-{today_month}-{today_day}"))





df_prcp %>% 
  mutate(day = day(date),
         month = month(date),
         year = year(date)) %>% 
  drop_na(prcp) %>% 
  group_by(month, day) %>% 
  summarize(prob_prcp = mean(prcp > 0),
            mean_prcp = mean(prcp),
            mean_event = mean(prcp[prcp > 0]),
            .groups = "drop") %>% 
  mutate(date = ymd(glue("2020-{month}-{day}"))) %>% 
  select(-month, -day) %>% 
  pivot_longer(cols = c(prob_prcp, mean_prcp, mean_event)) %>% 
  mutate(name = factor(name, levels = c("prob_prcp", "mean_prcp", "mean_event"))) %>% 
ggplot(aes(x=date, y=value, color = name)) +
  geom_vline(xintercept = today_date, color = "red", size = 1) +
  geom_line() +
  geom_hline(yintercept = 0, color = "white") +
  geom_smooth(se = FALSE) +
  facet_wrap(~name, ncol = 1, scales = "free_y", 
             strip.position = "left",
             labeller = labeller(name = pretty_labels)) +
  scale_color_manual(values = c("red", "green", "dodgerblue")) +
  scale_y_continuous(limits = c(0,NA), expand = c(0,0)) +
  scale_x_date(date_breaks = "2 months", 
               date_labels = "%B") +
  coord_cartesian(clip = "off") +
  labs(x=NULL,
       y=NULL,
       title = glue("<span style = 'color: red'>Probability</span> and <span style = 'color: green'>Aver</span><span style = 'color: dodgerblue'>age</span> Precipitation in The Netherlands from 1901 - {this_year}"),
       #title = glue("Through {today_month} {today_day}, the precipitation in The Netherlands near De Bilt has a <span style = 'color: red'>probability</span> and <span style = 'color: red'>average</span> for {today_date}")) +
       subtitle = glue("The red vertical line is today {today_date}")) +
  theme(
    panel.background = element_rect(fill = "black"),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(color = "white", size = 12), 
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "black", color = "black"),
    axis.line = element_line(),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(margin=margin(b=10), size = 22, color = "white"),
    plot.subtitle = element_textbox_simple(margin=margin(b=10), size = 15, color = "white"),
    axis.title.y = element_text(color = "white"),
    axis.text = element_text(color = "white", size = 12),
    legend.background = element_rect(fill = "black"),
    # legend.text = element_text(color = "white", size = 10),
    # legend.title = element_text(color = "white"),
    #legend.position = "bottom",
    legend.position = "none",
    #legend.key = element_rect(fill = "black")
  )
  
  
  
  
  
  
  
  
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





