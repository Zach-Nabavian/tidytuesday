library(tidytuesdayR)
library(tidyverse)
library(gt)

tuesdata <- tt_load('2025-04-08')

care_state <- tuesdata$care_state

care_state %>% 
  distinct(measure_id, measure_name, start_date, end_date) %>% 
  select(measure_id, measure_name, start_date, end_date) %>% 
  print(n = 22)

care_state %>% 
  filter(measure_id == "HCP_COVID_19") %>% 
  arrange(score) %>% 
  top_n(10, score) %>% 
  ggplot(aes(x = reorder(state, desc(score)), y = score)) +
  labs(title = "Top 10 States With The Highest Percentage of Healthcare Workers With Up to Date Covid Vaccinations as of Q1 2024",
       y = "Percent", x = "State or Territory") +
  geom_col() +
  theme_bw() +
  theme(axis.title = element_text(size = 15, face = "bold")) +
  theme(text = element_text(size = 14)) +
  theme(title = element_text(size = 15, face = "bold"))



