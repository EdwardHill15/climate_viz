source("code/26072022_Rscript Weather Netherlands.R")
library(ggtext)



df <- df[,-c(3,5)]
local_weather <- df
snow_data <- local_weather %>% 
  select(date, snow) %>% 
  drop_na(snow) %>% 
  mutate(cal_year = year(date),
         month = month(date),
         snow_year = if_else(date < ymd(glue("{cal_year}-07-01")),
                             cal_year - 1,
                             cal_year)) %>% 
  select(month, snow_year, snow)

snow_data %>% 
  group_by(snow_year) %>% 
  summarize(total_snow = sum(snow)) %>% 
  filter(total_snow > 0) %>% 
  ggplot(aes(x = snow_year, y = total_snow)) +
  geom_line()







dummy_df <- crossing(snow_year = 1901:2022,
                     month = 1:12) %>% 
  mutate(dummy = 0)


total_snow <- snow_data %>% 
  group_by(snow_year) %>% 
  summarize(total_snow = sum(snow)) %>% 
  filter(snow_year == 2020) %>% 
  mutate(total_snow = total_snow/10) %>% 
  pull(total_snow)


snow_data %>% 
  right_join(., dummy_df, by = c("snow_year", "month")) %>% 
  mutate(snow = if_else(is.na(snow), dummy, snow)) %>% 
  group_by(snow_year, month) %>% 
  summarize(snow = sum(snow), .groups = "drop") %>% 
  mutate(month = factor(month, levels = c(8:12, 1:7)),
         snowfall_year = 2020 == snow_year) %>% 
  ggplot(aes(x = month, y = snow, group = snow_year, color = snowfall_year)) +
  geom_line(show.legend = FALSE, size = 1) +
  scale_color_manual(name = NULL,
                     breaks = c(T, F),
                     values = c("dodgerblue", "gray")) +
  scale_x_discrete(breaks = c(9,11,1,3,5),
                   labels = month.abb[c(9,11,1,3,5)],
                   expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0,3500, 500),
                     labels = seq(0,350, 50)) +
  labs(x=NULL,
       y = "Total monthly snowfall (cm)",
       title = glue("The <span style = 'color:dodgerblue'>snow year 2020</span> had a total of {total_snow} cm of snow")) +
  
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(),
    plot.title.position = "plot",
    plot.title = element_markdown()
  )
  
df2 <- snow_data %>% 
  group_by(snow_year) %>% 
  filter(snow > 0) %>% 
  summarize(total = sum(snow)) %>% 
  arrange()
df2

ggsave("figures/snow_by_snowyear.png", width = 6, height = 4)

