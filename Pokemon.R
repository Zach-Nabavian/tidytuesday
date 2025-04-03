library(tidyverse)
library(tidytuesdayR)
library(gt)
library(scales)

tuesdata <- tt_load('2025-04-01')
tuesdata

evolutions <- read_csv('evolutions.csv')
evolutions <- as_tibble(evolutions)

pokemon_df <- tuesdata$pokemon_df

pokemon_df <- pokemon_df %>% 
  mutate(bst = hp + attack + defense + special_attack + special_defense + speed)

pokemon_df <- left_join(pokemon_df, evolutions, by = c('pokemon' = 'name'))

pokemon_df %>% 
  select(pokemon, stage, family)

pokemon_df %>% 
  group_by(generation_id) %>% 
  summarise(mean = mean(bst))

pokemon_df %>% 
  group_by(family) %>% 
  filter(stage == max(stage)) %>% 
  select(pokemon, stage)

pokemon_df %>% 
  filter(is.na(generation_id) == FALSE) %>% 
  group_by(family) %>% 
  filter(stage == max(stage)) %>% 
  group_by(generation_id) %>% 
  summarise(across(c(attack, defense, special_attack, special_defense, speed, bst),mean)) %>% 
  pivot_longer(
    cols = -generation_id,
    names_to = "stat",
    values_to = "value"
  ) %>% 
  filter(stat %in% c("attack", "speed")) %>% 
  ggplot(aes(x = generation_id, y = value, color = stat)) +
  geom_line() +
  scale_x_continuous(breaks = breaks_pretty()) +
  labs(y = "Average Stats", x = "Generation", title = "Pokemon Gained More Attack Power Over Each Generation, But Became Slower") +
  theme_bw() +
  theme(axis.title = element_text(size = 15, face = "bold")) +
  theme(text = element_text(size = 14)) +
  theme(title = element_text(size = 15, face = "bold"))

