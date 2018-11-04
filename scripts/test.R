library(tidyverse)
library(maps)
library(sf)
library(rgeos)
library(janitor)
library(ggrepel)
library(tidygraph)
library(ggraph)

theme_set(theme_graph())

states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
head(states)

states <- cbind(states, st_coordinates(st_centroid(states)))

#state abbreviations from https://www.infoplease.com/state-abbreviations-and-state-postal-codes
state_abbreviations <- read_csv("data/state_abbreviations.csv") %>% 
  clean_names() %>% 
  mutate(state_district = tolower(state_district)) %>% 
  rename(abbr = postal_code) %>% 
  select(-abbreviation)

states <- states %>% 
  left_join(state_abbreviations, by = c("ID" = "state_district")) %>% 
  arrange(abbr)
  #select(-ID) %>% 
  #rename(ID = abbr) %>% 





df <- read_csv("https://raw.githubusercontent.com/PublicI/actblue-analysis/master/data/actblue_states.csv")
#df %>% 
#  right_join(states, by = c("contributor_state" = "abbr")) %>% 
#  right_join(states %>% select(-c(ID, X, Y)), by = c("recipient_state" = "abbr")) -> df
df %>% 
  semi_join(states, by = c("contributor_state" = "abbr")) %>% 
  semi_join(states, by = c("recipient_state" = "abbr")) -> df
df %>% 
  select(-c(X1, count, sum)) %>% 
  gather(state_type, state_name) %>% 
  distinct() %>% 
  group_by(state_type) %>% 
  summarize(n = n())

states %>% 
  semi_join(df, by = c("abbr" = "contributor_state")) %>% 
  semi_join(df, by = c("abbr" = "recipient_state"))  -> states

states %>% 
  ggplot() +
  geom_sf() +
  geom_point(aes(X, Y))



df %>%
  select(-`X1`) %>% 
  arrange(contributor_state, recipient_state) %>% 
  mutate(sum = sum / 10^6,
         dollar_per_contribution = sum/count) %>% 
  na.omit() %>% 
  filter(!(contributor_state == recipient_state)) %>% 
  #filter(sum > .25) %>% 
  #filter(contributor_state %in% c("PA", "WI")) %>% 
  as_tbl_graph(directed = TRUE) -> g
g

node_pos <- states %>%
  select(abbr, X, Y) %>%
  rename(x = X, y = Y) %>%  # node positions must be called x, y
  st_set_geometry(NULL)
lay <- create_layout(g, 'manual',
                     node.positions = node_pos)
#assert_that(nrow(lay) == nrow(nodes))

manual_layout <- create_layout(graph = g,
                               layout = "manual", node.positions = node_pos)


ggraph(manual_layout) +
  geom_sf(data = states) +
  #geom_label_repel(data = states, aes(X, Y, label = abbr)) +
  geom_node_label(aes(label = name),repel = FALSE) +
  geom_edge_fan(aes(edge_width = sum, edge_alpha = sum),
                arrow = arrow(length = unit(4, 'mm')), 
                start_cap = circle(3, 'mm'),
                end_cap = circle(3, 'mm'),
                color = "blue") +
  scale_edge_width_continuous(range = c(.3, 2)) +
  scale_edge_alpha_continuous(range = c(.1, 1))
