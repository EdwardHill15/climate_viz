source("code/26072022_Rscript Weather Netherlands.R")
library(glue)
library(ggtext)
library(scales)
library(tidyverse)
library(lubridate)

local_weather <- df
colnames(local_weather) <- c("date", "tmax", "tmin", "prcp")
local_weather <- local_weather %>% 
  mutate(prcp_cm = prcp/10,
         tmax_c = tmax/10)
local_weather <- local_weather[-c(2:3)]
local_weather <- local_weather[-2]
colnames(local_weather) <- c("date", "prcp", "tmax")
this_year <- year(today())
this_month <- month(today(), label = TRUE, abbr = FALSE)
this_day <- ordinal(day(today()))

Sys.setlocale("LC_TIME", "English")


tmax_prcp <- local_weather %>% 
  mutate(year = year(date)) %>% 
  # drop_na(tmax, prcp) %>% 
  filter(year > 1905 & year != year(today())) %>% 
  group_by(year) %>% 
  summarise(tmax = mean(tmax, na.rm=TRUE),
            prcp = sum(prcp, na.rm=TRUE))
tmax_prcp %>% 
  pivot_longer(-year) %>% 
  ggplot(aes(x=year, y=value)) +
  geom_line() +
  facet_wrap(~name, ncol = 1, scales = "free_y") + 
  geom_smooth(se = FALSE)

# rescale tmax from (0,1)

scaled_tmax_prcp <- tmax_prcp %>% 
  mutate(tmax_tr = (tmax - min(tmax))/ (max(tmax) - min(tmax)),
         tmax_min = min(tmax),
         tmax_max = max(tmax),
         prcp_tr = (prcp - min(prcp))/ (max(prcp) - min(prcp)),
         prcp_min = min(prcp),
         prcp_max = max(prcp))
        
# %>% 
#   summarize(min = min(tmax_tr), max = max(tmax_tr))



tmax_plot <- scaled_tmax_prcp %>% 
  ggplot(aes(x=year, y=tmax_tr)) +
  geom_line(color="blue") 


tmax_plot + 
  geom_line(aes(y=prcp_tr), color = "red") +
  scale_y_continuous(labels = seq(10,30,5),
                     breaks = (seq(10,30,5) - 11.7)/15.8,
                     limits = (c(8, 32) - 11.7)/15.8,
                     name = "Average annual temperature",
                     sec.axis = sec_axis(trans = ~.,
                     labels = seq(300,1800,300),
                     breaks = (seq(300,1800,300) - 387.)/1240.,
                     name = "Total Precipitation (mm)" )) +
  theme(
    axis.title.y.left = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red")
  )
  
tmax_prcp %>% 
  ggplot(aes(x = tmax, y = prcp, color = year)) +
  geom_point() + 
  geom_smooth(method = "lm")

cor.test(tmax_prcp$tmax, tmax_prcp$prcp)
cor.test(tmax_prcp$tmax, tmax_prcp$prcp, method = "spearman", exact = FALSE)












