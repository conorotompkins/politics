library("tidyverse")
library("maps")
library("sf")
library("rgeos")
library("janitor")
library("ggrepel")
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
head(states)

states <- cbind(states, st_coordinates(st_centroid(states)))








ggplot(states) +
  geom_sf() +
  geom_point(aes(X, Y))

state_abbreviations <- read_csv("data/state_abbreviations.csv") %>% 
  clean_names() %>% 
  mutate(state_district = tolower(state_district)) %>% 
  rename(abbr = postal_code) %>% 
  select(-abbreviation)

states <- states %>% 
  left_join(state_abbreviations, by = c("ID" = "state_district"))

ggplot(states) +
  geom_sf() +
  geom_label_repel(aes(X, Y, label = abbr))
