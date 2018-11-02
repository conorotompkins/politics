library(tidyverse)
library(tidygraph)
library(ggraph)

theme_set(theme_bw())

df <- read_csv("https://raw.githubusercontent.com/PublicI/actblue-analysis/master/data/actblue_states.csv")
df

df %>%
  select(-`X1`) %>% 
  mutate(sum = sum / 1000000) %>% 
  na.omit() %>% 
  filter(sum > 5) %>% 
  #filter(contributor_state %in% c("PA", "WI")) %>% 
  as_tbl_graph() %>% 
  ggraph(layout = "drl") +
  geom_edge_fan(aes(edge_width = sum, edge_alpha = sum),
                arrow = arrow(length = unit(4, 'mm')), 
                start_cap = circle(3, 'mm'),
                end_cap = circle(3, 'mm')) +
  geom_node_label(aes(label = name)) +
  scale_edge_width_continuous(range = c(.3, 3)) +
  scale_edge_alpha_continuous(range = c(.3, 1))
