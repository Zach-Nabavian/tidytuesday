library(tidyverse)

source("https://raw.githubusercontent.com/Zach-Nabavian/solar_theme/refs/heads/main/Solar%20Theme.R")


nsf_terminations <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-06/nsf_terminations.csv')

nsf_terminations %>% 
  distinct(division) %>% 
  print(n = 35)

nsf_terminations %>% 
  group_by(division) %>% 
  summarize(grants_terminated = n(), dollars_revoked = sum(usaspending_obligated)) %>% 
  drop_na(division) %>% 
  ggplot(aes(x = dollars_revoked, y = fct_reorder(division, dollars_revoked))) +
  geom_glow(geom_func = geom_col, color_main = solar_system_palette["Mars"]) +
  theme_solar() +
  theme(legend.position="none") +
  scale_x_continuous(labels = scales::dollar_format()) +
  labs(title = "Grants in Education and Equity By Far The Largest Grant Terminations", 
        subtitle = "Hard Sciences Had Relatively Fewer Terminations",
        y = "Division", x = "Dollars Revoked")

