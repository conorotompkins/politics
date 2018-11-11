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

df <- read_csv("https://raw.githubusercontent.com/PublicI/actblue-analysis/master/data/actblue_states.csv")

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
         sum = round(sum, digits = 2)) %>% 
  na.omit() %>% 
  filter(!(contributor_state == recipient_state)) -> df_intermediate

df_intermediate %>% 
  as_tbl_graph(directed = TRUE) -> g

g %>% 
  activate(nodes) %>% 
  as_tibble() %>% 
  mutate(index = row_number()) -> state_nodes

to_state <- "TX"

to_state_index <- state_nodes %>% 
  filter(name == to_state) %>% 
  select(index) %>% 
  unlist()

g %>% 
  activate(edges) %>% 
  filter(to == to_state_index) -> g

node_pos <- states %>%
  select(abbr, X, Y) %>%
  rename(x = X, y = Y) %>%  # node positions must be called x, y
  st_set_geometry(NULL)
str(node_pos)

lay <- create_layout(g, 'manual',
                     node.positions = node_pos)

manual_layout <- create_layout(graph = g,
                               layout = "manual", node.positions = node_pos)


ggraph(manual_layout) +
  geom_sf(data = states) +
  geom_node_label(aes(label = name),repel = FALSE) +
  geom_edge_fan(aes(edge_width = sum, edge_alpha = sum),
                arrow = arrow(length = unit(4, 'mm')), 
                start_cap = circle(3, 'mm'),
                end_cap = circle(3, 'mm'),
                color = "blue") +
  scale_edge_width_continuous("Donations in millions USD", range = c(.3, 2)) +
  scale_edge_alpha_continuous("Donations in millions USD", range = c(.1, 1)) +
  labs(title = "ActBlue Political Donations",
       subtitle = str_c("Aggregate interstate donations to ", to_state),
       caption = "@conorotompkins, data from Center for Public Integrity and 538") +
  theme(panel.grid.major = element_line(colour = 'transparent')) -> p

p
