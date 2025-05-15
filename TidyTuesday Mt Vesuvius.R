vesuvius <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-13/vesuvius.csv')

library(tidyverse)

source("https://raw.githubusercontent.com/Zach-Nabavian/solar_theme/refs/heads/main/Solar%20Theme.R")

vesuvius %>% 
  select_all()


vesuvius %>% 
  distinct(review_level)

vesuvius %>%
  distinct(year)

vesuvius %>% 
  distinct(md_error)

vesuvius %>% 
  distinct(longitude)

vesuvius %>% 
  ggplot(aes(x = time, y = duration_magnitude_md)) +
  geom_line()

vesuvius_summarized <- vesuvius %>% 
    filter(is.na(duration_magnitude_md) == FALSE) %>% 
    group_by(year) %>% 
    summarize(events_recorded = n(), mean_md = mean(duration_magnitude_md))
    
coeff <- 1/(mean(vesuvius_summarized$events_recorded/vesuvius_summarized$mean_md))

colors = c(Column = solar_system_palette["Sun"],
           Line = solar_system_palette["Mars"])
  
vesuvius_summarized %>% 
  filter(year > 2012) %>% 
  ggplot(aes(x = year)) +
  geom_col(aes(y = events_recorded), fill = solar_system_palette["Sun"]) +
  geom_line(aes(y = mean_md/coeff), color = solar_system_palette["Mars"], linewidth = 1.5) +
  scale_y_continuous(name = "Number of Events Recorded", 
  sec.axis = sec_axis(~.*coeff, "Duration Magnitude (md)")) +
  scale_x_continuous(n.breaks = 12) +
  theme_solar() +
  labs(title = "Somewhat of an Upward Trend in Earthquakes Recorded", subtitle = "2023 Had an Unusually High Duration Magnitude") +
  theme(axis.title.y = element_text(colour = solar_system_palette["Sun"]),
        axis.title.y.right = element_text(colour = solar_system_palette["Mars"]))
  