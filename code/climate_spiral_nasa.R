library(tidyverse)
library(gganimate)
library(ggplot2)

grid_labels <- tibble(
  x = c(-5, -4, 0, 1),
  y = 2030,
  labels = c("+1\u00B0 C", "0\u00B0 C", "0\u00B0 C", "+1\u00B0 C")
)


year_labels <- tibble(
  x = -2,
  y = c(seq(1880, 2000, by = 20), 2021)
)



t_data <- read_csv("data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>%
  select(year = Year, all_of(month.abb)) %>%
  pivot_longer(-year, names_to="month", values_to="t_diff") %>%
  drop_na()

t_data %>% 
  filter(month == "Apr" | month == "Oct") %>% 
  pivot_wider(names_from = "month", values_from = "t_diff") %>% 
  mutate(ave_t = (Oct + Apr) /2) %>% 
  ggplot(aes(x=-4 - Oct, xend = Apr, y = year, yend = year, color = ave_t)) +
  geom_vline(xintercept = c(-5, -4, 0, 1), color = "gold") +
  geom_label(data = grid_labels, aes(x=x, y=y, label = labels),
             inherit.aes = FALSE,
             fill = "black", color = "gold", label.size = 0, size = 3) + 
  geom_segment(size = .9, lineend = "round") +
  geom_text(data = year_labels, aes(x=x, y=y, label=y),
            inherit.aes = FALSE, color = "gold", size = 3, fontface = "bold") +
  scale_color_gradient2(low = "darkblue", high = "darkred", mid = "white",
                        midpoint = 0, guide = "none") +
  scale_y_continuous(limits = c(NA, 2030), expand = c(0,0)) +
  coord_cartesian(clip = "off") +
  labs(x = NULL,
       y = NULL, 
       title = NULL) +
  theme(
    plot.background = element_rect(fill="black"),
    panel.background = element_rect(fill = "black", color = "black"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
 
ggsave("figures/climate_tornado.png", width=4.5, height=3.5, units = "in")



t_diff <- read_csv("data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>%
  select(year = Year, month.abb) %>%
  pivot_longer(-year, names_to="month", values_to="t_diff") %>%
  drop_na()

next_jan <- t_diff %>%
  filter(month == "Jan") %>%
  mutate(year = year - 1,
         month = "next_Jan")

t_data <- bind_rows(t_diff, next_jan) %>%
  mutate(month = factor(month, levels = c(month.abb, "next_Jan")),
         month_number = as.numeric(month)) %>%
  arrange(year, month) %>%
  filter(year != 1879) %>%
  mutate(step_number = 1:nrow(.))

annotation <- t_data %>%
  slice_max(year) %>%
  slice_max(month_number)

temp_lines <- tibble(
  x = 1,
  y = c(1, 0, -1),
  labels = c("+1\u00B0 C", "0\u00B0 C", "-1\u00B0 C")
)

month_labels <- tibble(
  x = 1:12,
  labels = toupper(month.abb),
  y = 1.5
)

gridlines <- tibble(
  x = c(1.2, 1.3, 1.6),
  xend = c(12.8, 12.7, 12.4),
  y = c(1, 0, -1), 
  yend = y
  
)

a <- t_data %>% 
  ggplot(aes(x=month_number, y=t_diff, group=year, color=t_diff)) +
  # geom_rect(aes(xmin=1, xmax=13, ymin=-2, ymax=2.4),
  #           color="black", fill="black",
  #           inherit.aes = FALSE) +
  geom_label(aes(x = 1, y=-1.7, label = year),
             fill="black",
             label.size = 0,
             size=6) +
  geom_line() +
  geom_segment(data=gridlines, aes(x=x, y=y, xend=xend, yend=yend),
               color=c("yellow", "green", "yellow"),
               inherit.aes = FALSE) +
  geom_text(data = temp_lines, aes(x=x, y=y, label=labels),
            color=c("yellow", "green", "yellow"), size=2, fontface="bold",
            inherit.aes=FALSE) +
  geom_text(data = month_labels, aes(x=x, y=y, label = labels),
            inherit.aes = FALSE, color="yellow"#,
            #angle = seq(360 - 360/12, 0, length.out = 12)
  ) +
  # scale_x_continuous(breaks=1:12,
  #                    labels=month.abb, expand = c(0,0),
  #                    sec.axis = dup_axis(name = NULL, labels=NULL)) +
  scale_y_continuous(
                     limits = c(-2.0, 1.5), expand = c(0, -0.3), 
                     ) +
  scale_color_gradient2(low = "blue", high = "red", mid="white", midpoint = 0,
                        guide = "none") +
  coord_polar(start = 0) +
    labs(x = NULL,
       y = NULL,
       title = NULL) +
  theme(
    panel.background = element_rect(fill="black"),
    plot.background = element_rect(fill = "black", color="black"),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_blank()
  ) +
  transition_manual(frames = year, cumulative = TRUE)

#ggsave("figures/climate_spiral_nasa.png", width=4.155, height=4.5, unit="in")

animate(a, width=4.155, height=4.5, unit="in", res=300)
anim_save("figures/climate_spiral_nasa.gif")


animate(a, width=4.155, height=4.5, unit="in", res=300,
        renderer = av_renderer("figures/climate_spiral_nasa.mp4")
        )
