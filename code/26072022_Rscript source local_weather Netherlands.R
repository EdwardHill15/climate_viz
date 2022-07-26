source("26072022_Rscript Weather Netherlands.R")
df
local_weather <- df


# normalize temperature data in the range of the years 1951 - 1980
# that means that the mean-temperature in this range = 0 and the 
# other years outside this range (1951 - 1980) show the difference (t_diff)
# of temperature from this range of temperature data. 

this_year <- year(today())

local_weather$tmax <- local_weather$tmax / 10
colnames(local_weather) <- c("date", "tmax", "tmin", "prcp")
local_weather %>% 
  select(date, tmax) %>% 
  mutate(year = year(date)) %>% 
  filter(year !=1901 & year != this_year) %>%
  group_by(year) %>% 
  summarize(tmax = mean(tmax)) %>% 
  mutate(normalize_range = (year >= 1951 & year <= 1980),
         normalize_mean = sum(tmax * normalize_range)/sum(normalize_range),
         t_diff = tmax - normalize_mean) %>% 
  ggplot(aes(x=year, y=t_diff)) + 
  geom_line() +
  geom_smooth()

# Normalize temperatures to a range of years
# in this case we want to set the average temperature between 1951 - 1980
# to zero degrees
# TRUE and FALSE values are respectively "1"  and "0"
  
  
#  create another plot using group_by and summarize method

local_weather %>% 
  select(date, tmax) %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  filter(year !=1901) %>% 
  group_by(year, month) %>% 
  summarize(tmax = mean(tmax), .groups = "drop") %>% 
  group_by(month) %>% 
  mutate(normalized_range = year >= 1951 & year <= 1980,
         normalized_temp = sum(tmax * normalized_range)/sum(normalized_range),
         t_diff = tmax - normalized_temp,
         is_this_year = year == this_year) %>%
  ungroup() %>% 
  #filter(month == 7) %>% 
  #ggplot(aes(x=month, y=normalized_temp)) + geom_line()
  ggplot(aes(x=month, y=t_diff, group=year, color=is_this_year)) + 
  geom_line() +
  scale_color_manual(breaks = c(F,T),
                     values = c("lightgray", "dodgerblue"),
                     guide = "none") +
  theme_classic()
  
  
  ggplot(aes(x=month, y=tmax, group = year, color=year)) +
  geom_line() +
  geom_smooth()







