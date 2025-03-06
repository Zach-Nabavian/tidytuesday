install.packages("tidytuesdayR")


library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2025, week = 9)

longbeach <- tuesdata$longbeach

head(longbeach)

longbeach <- longbeach %>% 
    mutate(
    was_outcome_alive = as.logical(was_outcome_alive),
    dplyr::across(
      c(
        "animal_type",
        "primary_color",
        "secondary_color",
        "sex",
        "intake_condition",
        "intake_type",
        "intake_subtype",
        "reason_for_intake",
        "jurisdiction",
        "outcome_type",
        "outcome_subtype"
      ),
      as.factor
    )
  ) %>%  
  select_all()

longbeach %>% 
  ggplot(aes(y = reorder(animal_type, animal_type, function(x) length(x)))) +
  geom_bar(stat = "count") +
  geom_line(stat = "fraction") +
  labs(y = "Animal Type", x = "Count", title = "Types of Animals") +
  theme_bw() +
  theme(axis.title = element_text(size = 15, face = "bold")) +
  theme(text = element_text(size = 14)) +
  theme(title = element_text(size = 15, face = "bold"))

  