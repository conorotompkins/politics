library(tidyverse)
library(tidygraph)
library(ggraph)

theme_set(theme_graph())

df <- read_csv("https://raw.githubusercontent.com/PublicI/actblue-analysis/master/data/actblue_states.csv")
df

df %>%
  select(-`X1`) %>% 
  mutate(sum = sum / 10^6,
         dollar_per_contribution = sum/count) %>% 
  na.omit() %>% 
  filter(!(contributor_state == recipient_state)) %>% 
  filter(sum > .25) %>% 
  #filter(contributor_state %in% c("PA", "WI")) %>% 
  as_tbl_graph() %>% 
  ggraph(layout = "kk") +
  geom_edge_fan(aes(edge_width = sum, edge_alpha = sum),
                arrow = arrow(length = unit(4, 'mm')), 
                start_cap = circle(3, 'mm'),
                end_cap = circle(3, 'mm')) +
  geom_node_label(aes(label = name)) +
  scale_edge_width_continuous(range = c(.3, 2)) +
  scale_edge_alpha_continuous(range = c(.1, 1)) -> plot

plot

#use this tutorial to overlay the network graph on a map
#https://datascience.blog.wzb.eu/2018/05/31/three-ways-of-visualizing-a-graph-on-a-map/