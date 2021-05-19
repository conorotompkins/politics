library(gganimate)
library(gifski)

state_graph

filter_threshhold_list <- c(0, .3)
filter_threshhold_list[2]

total <- filter_threshhold_list %>% 
  set_names() %>% 
  map_df(~filter(state_graph, coeff > .x), .id = "filter_threshhold") %>% 
  filter(!is.na(coeff),
         state != other_state) %>% 
  mutate(filter_threshhold = as.numeric(filter_threshhold))


original <- total %>% 
  filter(filter_threshhold == filter_threshhold_list[1])

additional <- total %>% 
  filter(filter_threshhold == filter_threshhold_list[2])

original_graph <- original %>% 
  select(-filter_threshhold) %>% 
  as_tbl_graph(directed = FALSE) %>% 
  activate(edges) %>% 
  mutate(filter_threshhold = filter_threshhold_list[1])

original_graph %>% 
  as_tibble()

additional_edges <- additional %>% 
  select(-filter_threshhold) %>% 
  as_tbl_graph(directed = FALSE) %>% 
  activate(edges) %>% 
  mutate(filter_threshhold = filter_threshhold_list[2]) %>% 
  as_tibble()

combined_graph <- original_graph %>% 
  bind_edges(additional_edges)
  
combined_graph %>% 
  ggraph("stress") +
  geom_edge_fan(aes(#edge_color = coeff, 
                    edge_alpha = coeff,
                    edge_width = coeff)) +
  geom_node_point() +
  geom_node_label(aes(label = name), repel =  T) +
  facet_edges(~filter_threshhold) +
  #scale_edge_color_viridis() +
  scale_edge_alpha_continuous(range = c(0, .6)) +
  scale_edge_width_continuous(range = c(.1, 4)) +
  guides(edge_color = guide_edge_colorbar(order = 2),
         edge_alpha = guide_legend(order = 1),
         edge_width = guide_legend(order = 1)) +
  labs(edge_color = "Similarity",
       edge_alpha = "Similarity",
       edge_width = "Similarity") +
  theme_void() +
  theme(panel.border = element_rect(color = "grey", fill= NA))

combined_graph_animated <- combined_graph %>% 
  ggraph("stress") +
  geom_edge_fan(aes(edge_color = coeff, 
                    edge_alpha = coeff,
                    edge_width = coeff)) +
  #geom_node_point() +
  geom_node_label(aes(label = name)) +
  scale_edge_color_viridis() +
  scale_edge_alpha_continuous(range = c(0, .6)) +
  scale_edge_width_continuous(range = c(.1, 4)) +
  guides(edge_color = guide_edge_colorbar(order = 2),
         edge_alpha = guide_legend(order = 1),
         edge_width = guide_legend(order = 1)) +
  labs(edge_color = "Similarity",
       edge_alpha = "Similarity",
       edge_width = "Similarity",
       title = "Filter threshhold: {previous_state}") +
  theme_void() +
  transition_states(filter_threshhold)

anim_save(combined_graph_animated, filename = "output/images/animated_network_graph.gif")
