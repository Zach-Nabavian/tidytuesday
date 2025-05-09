library(tidyverse)
library(readr)
library(tseries)
library(tsibble)
library(ggbeeswarm)
library(stringr)

source("https://raw.githubusercontent.com/Zach-Nabavian/solar_theme/refs/heads/main/Solar%20Theme.R")

daily_accidents <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-22/daily_accidents.csv')

daily_accidents <- as_tsibble(daily_accidents)

daily_accidents %>% 
  ggplot(aes(x = date, y = fatalities_count)) +
  geom_line()

daily_accidents %>% 
  mutate(month = month(date)) %>% 
  ggplot(aes(x = month, y = fatalities_count, group = month)) +
  scale_x_continuous(breaks = seq(0, 12, 1)) +
  geom_glow(geom_func = geom_boxplot, color_main = solar_system_palette["Sun"]) +
  theme_solar() +
  labs(title = "Distribution of Fatality Counts Each Month", x = "Month", y = "Fatalities")

trim_all_spaces <- function(x) {
  x <- gsub("^\\s+|\\s+$", "", x)       # trim leading/trailing spaces
  x <- gsub("\\s{2,}", " ", x)          # collapse internal multiple spaces
  return(x)
}

daily_accidents <- daily_accidents %>%
  mutate(
    weekday = weekdays(date),
    weekday = as.character(weekday),            # ensure it's character
    weekday = trim_all_spaces(weekday),         # now trim spaces
    weekday = factor(weekday, levels = c(
      "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
  )

daily_accidents %>% 
  ggplot(aes(x = weekday, y = fatalities_count, group = weekday)) +
  geom_glow(geom_func = geom_boxplot, color_main = solar_system_palette["Neptune"]) +
  theme_solar() +
  labs(title = "Distribution of Fatality Counts Each Weekday", x = "Weekday", y = "Fatalities")

weekday_colors <- c(
  "Sunday"    = solar_system_palette[["Jupiter"]],
  "Monday"    = solar_system_palette[["Mercury"]],
  "Tuesday"   = solar_system_palette[["Earth Blue"]],
  "Wednesday" = solar_system_palette[["Earth Green"]],
  "Thursday"  = solar_system_palette[["Uranus"]],
  "Friday"    = solar_system_palette[["Sun"]],
  "Saturday"  = solar_system_palette[["Mars"]]
)


daily_accidents %>%
  ggplot(aes(x = fatalities_count, y = "", fill = weekday)) +
  geom_glow(
    geom_func = function(...) geom_beeswarm(shape = 21, size = 4, stroke = 0.2, ...),
  ) +
  scale_fill_manual(values = weekday_colors) +
  theme_solar() +
  labs(
    title = "Fatality Beeswarm Plot",
    subtitle = "Weekends Tend to Have More Accidents",
    x = "Fatalities",
    y = ""
  )


unique(daily_accidents$weekday)
levels(daily_accidents$weekday)
names(solar_system_palette)
names(weekday_colors)
  
acf(daily_accidents$fatalities_count)

acf(daily_accidents$fatalities_count, type = "partial")

unique(daily_accidents$weekday)

daily_accidents$weekday <- factor(str_trim(as.character(daily_accidents$weekday)))
weekdays.Date(daily_accidents$date)
levels(daily_accidents$weekday)
setdiff(levels(daily_accidents$weekday), names(weekday_colors))
is.factor(daily_accidents$weekday)

levels(daily_accidents$weekday)
names(weekday_colors)