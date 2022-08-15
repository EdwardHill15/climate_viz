source("20221508_Rscript snow and prcp.R")
local_weather
  filter(snow > 0) %>% 
   mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarize(prcp = sum(prcp))
