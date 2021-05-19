library(tidyverse)
library(lubridate)
library(hrbrthemes)
library(tigris)
library(tidycensus)

options(tigris_use_cache = TRUE,
        scipen = 999,
        digits = 4)

theme_set(theme_ipsum())

state_polls <- read_csv("data/economist_model/site_data/state_averages_and_predictions_topline.csv") %>% 
  arrange(state, date) %>% 
  mutate(tightening = dem_average_mean - projected_vote_mean)

glimpse(state_polls)

state_polls %>% 
  select(state, dem_average_mean, projected_vote_mean)

state_polls %>% 
  ggplot(aes(dem_average_mean, projected_vote_mean)) +
  geom_abline(alpha = .3, linetype = 2) +
  geom_point(size = 2, alpha = .5)
  #geom_label(aes(label = state))

state_polls %>% 
  mutate(state = fct_reorder(state, tightening),
         tightening = tightening * 100) %>%
  ggplot(aes(tightening, state)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_point()

state_polls %>% 
  ggplot(aes(tightening)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_histogram()

#states <- states(cb = TRUE)

states <- get_acs(geography = "state", 
                       variable = "B25077_001", 
                       survey = "acs1", 
                       geometry = TRUE, 
                       shift_geo = TRUE)

state_info <- bind_cols(state.name, state.abb) %>% 
  set_names(c("state", "abb"))

state_poll_graph <- states %>% 
  left_join(state_info, by = c("NAME" = "state")) %>% 
  select(NAME, abb) %>% 
  left_join(state_polls, by = c("abb" = "state")) %>% 
  semi_join(state_polls, by = c("abb" = "state")) %>% 
  #filter(NAME %in% c("Pennsylvania", "New York")) %>% 
  ggplot(aes(fill = tightening)) +
  geom_sf() +
  scale_fill_viridis_c()

state_poll_graph

state_poll_graph %>% 
  ggsave(filename = "output/images/state_poll_reversion.png")

state_polls %>% 
  select(state, date, dem_average_mean, projected_vote_mean) %>% 
  pivot_longer(cols = c(dem_average_mean, projected_vote_mean)) %>% 
  ggplot(aes(name, value, group = state)) +
  geom_point() +
  geom_line()

state_polls %>% 
  select(state, date, dem_average_mean) %>% 
  bind_rows(state_polls %>% select(state, projected_vote_mean)) %>% 
  replace_na(list(date = "2020-11-03")) %>% 
  pivot_longer(cols = c(dem_average_mean, projected_vote_mean)) %>%
  filter(!is.na(value)) %>% 
  ggplot(aes(date, value, color = name)) +
  geom_point()
