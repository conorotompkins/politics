library(tidyverse)
library(janitor)
library(tidygraph)
library(ggraph)

options(scipen = 999, digits = 4)

states <- read_csv("https://raw.githubusercontent.com/TheEconomist/us-potus-model/master/data/state_correlation_matrix.csv")

glimpse(states)

states_matrix <- as.matrix(states)

states_matrix[upper.tri(states_matrix)] <- NA
#states_matrix[diag(states_matrix)] <- NA


state_lookup <- states_matrix %>% 
  as_tibble() %>% 
  names() %>% 
  enframe() %>% 
  rename(other_state = value,
         row_id = name)

state_lookup

state_graph <- states_matrix %>%
  # as.matrix(diag = FALSE, upper = FALSE) %>% 
  as_tibble() %>% 
  rownames_to_column(var = "row_id") %>% 
  mutate(row_id = as.integer(row_id)) %>% 
  left_join(state_lookup) %>% 
  select(other_state, everything())

state_graph %>% 
  filter(other_state == "WV")

# state_graph %>% 
#   filter(other_state == "WV")

state_graph <- state_graph %>% 
  pivot_longer(cols = -c(row_id, other_state), names_to = "state", values_to = "coeff") %>% 
  select(state, other_state, coeff)

state_graph %>% 
  filter(other_state == "WV")

state_graph <- state_graph %>% 
  filter(!is.na(coeff),
         state != other_state) %>% 
  arrange(other_state)

state_graph %>% 
  filter(other_state == "WV" | state == "WV") %>% 
  View()
  
state_graph %>% 
  filter(state == "WV" | other_state == "WV") %>% 
  arrange(desc(coeff))

state_similarity_graph <- state_graph %>% 
  filter(coeff > 0) %>% 
  as_tbl_graph(directed = FALSE)


"lgl"
"kk"
"mds"


state_similarity_graph %>% 
  activate(edges) %>% 
  #filter(coeff > .5) %>% 
  ggraph("kk") +
  geom_edge_fan(aes(edge_color = coeff, 
                    edge_alpha = coeff,
                    edge_width = coeff)) +
  geom_node_point() +
  geom_node_label(aes(label = name), repel = TRUE) +
  scale_edge_color_viridis() +
  scale_edge_alpha_continuous(range = c(0, .6)) +
  scale_edge_width_continuous(range = c(.1, 4)) +
  guides(edge_color = guide_edge_colorbar(order = 2),
         edge_alpha = guide_legend(order = 1),
         edge_width = guide_legend(order = 1)) +
  labs(edge_color = "Similarity",
       edge_alpha = "Similarity",
       edge_width = "Similarity") +
  theme_void()
  
state_similarity_graph

kk_layout <- create_layout(graph = state_similarity_graph, layout = "kk") %>%
  select(x, y)

filtered_graph <- state_similarity_graph %>% 
  activate(edges) %>% 
  filter(coeff > .3)

filtered_layout <- create_layout(graph = filtered_graph, layout = kk_layout)


final_graph <- ggraph(filtered_layout) +
  geom_edge_fan(aes(edge_color = coeff,
                    edge_alpha = coeff,
                    edge_width = coeff)) +
  geom_node_point() +
  geom_node_label(aes(label = name), repel = TRUE) +
  scale_edge_color_viridis() +
  scale_edge_alpha_continuous(range = c(0, .6)) +
  scale_edge_width_continuous(range = c(.1, 4)) +
  guides(edge_color = guide_edge_colorbar(order = 2),
         edge_alpha = guide_legend(order = 1),
         edge_width = guide_legend(order = 1)) +
  labs(title = "State Similarity Scores",
       subtitle = "Data from github.com/TheEconomist/us-potus-model",
       edge_color = "Similarity",
       edge_alpha = "Similarity",
       edge_width = "Similarity",
       caption = "@conor_tompkins") +
  theme_void()

ggsave(final_graph, filename = "output/images/state_correlation_network_graph.png",
       width = 12, height = 12, dpi = 300)
