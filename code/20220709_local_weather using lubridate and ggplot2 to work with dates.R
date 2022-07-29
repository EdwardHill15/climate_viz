source("code/26072022_Rscript Weather Netherlands.R")
library(glue)
library(ggtext)
library(scales)
library(tidyverse)
library(lubridate)

local_weather <- df
colnames(local_weather) <- c("date", "tmax", "tmin", "prcp")
local_weather <- local_weather %>% 
  mutate(prcp_cm = prcp/10)
local_weather <- local_weather[-4]
colnames(local_weather) <- c("date", "tmax", "tmin", "prcp")
this_year <- year(today())
this_month <- month(today(), label = TRUE, abbr = FALSE)
this_day <- ordinal(day(today()))

Sys.setlocale("LC_TIME", "English")

local_weather %>% 
  select(date, prcp) %>% 
  drop_na(prcp) %>% 
  mutate(year = year(date),
         month = month(date),
         day = day(date),
         is_this_year = year == this_year) %>% 
  filter(!(month == 2 & day == 29)) %>% 
  group_by(year) %>% 
  mutate(cum_prcp = cumsum(prcp)) %>% 
  filter(cum_prcp > 0) %>% 
  ungroup() %>% 
  mutate(new_date = ymd(glue("2022-{month}-{day}"))) %>% 
  ggplot(aes(x=new_date, y=cum_prcp, group = year, color=is_this_year,
             size = is_this_year)) +
  geom_line(show.legend = FALSE) +
  geom_smooth(aes(group = 1), color = "darkred", size = 0.5) +
  scale_color_manual(breaks = c(F,T),
                     values = c("lightgray", "dodgerblue")) +
  scale_size_manual(breaks = c(F,T),
                    values = c(0.3,1)) +
  scale_x_date(date_labels = "%B", date_breaks = "2 months") +
  scale_y_continuous(breaks = seq(0, 1200, 300),
                     labels = seq(0, 120, 30),
                     limits = c(0,1200),
                     expand = c(0,0)) +
  labs(x = NULL,
       y = "Cumulative precipitation (cm)",
       title = glue("Through {this_month} {this_day}, the cumulative precipitation in The Netherlands near De Bilt is <span style = 'color: dodgerblue'>above</span> <span style = 'color: darkred'>average</span> for {this_year}")) +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(margin=margin(b=10), size = 16, color = "white"),
    axis.title.y = element_text(color = "white"),
    panel.background = element_rect(fill = "black"),
    axis.line = element_line(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "black", color = "black"),
    axis.text = element_text(color = "white", size = 8)
  )

ggsave("figures/cumulative_prcp_ne_black.png", width = 6, height = 5)
  

