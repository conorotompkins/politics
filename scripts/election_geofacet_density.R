library(tidyverse)
library(lubridate)
library(janitor)
library(sf)
library(tigris)
library(gganimate)
library(hrbrthemes)
library(geofacet)

#data from https://electionlab.mit.edu/data

options(tigris_use_cache = TRUE,
        scipen = 999,
        digits = 4)

county_geo <- counties(cb = TRUE)

#theme_set(theme_ipsum())

data <- read_csv("data/countypres_2000-2016.csv",
                 col_types = cols(
                   year = col_double(),
                   state = col_character(),
                   state_po = col_character(),
                   county = col_character(),
                   FIPS = col_character(),
                   office = col_character(),
                   candidate = col_character(),
                   party = col_character(),
                   candidatevotes = col_double(),
                   totalvotes = col_double(),
                   version = col_double()
                 )) %>% 
  clean_names() %>% 
  filter(office == "President",
         party == "democrat" | party == "republican",
         county != "Statewide writein") %>% 
  drop_na(fips, candidatevotes)

data %>% 
  semi_join(county_geo, by = c("fips" = "GEOID")) %>% 
  count(county) %>% 
  arrange(n)

data %>% 
  anti_join(county_geo, by = c("fips" = "GEOID")) %>% 
  count(state) %>% 
  arrange(desc(n))

presidential_votes <- data %>% 
  #semi_join(county_geo, by = c("fips" = "GEOID")) %>% 
  group_by(year, state, county, fips) %>% 
  mutate(pct_vote = candidatevotes / sum(candidatevotes)) %>% 
  ungroup() %>% 
  select(year, state, county, party, candidatevotes, pct_vote)

presidential_votes %>%
  anti_join(data, by = c("state", "county"))

presidential_votes %>% 
  filter(is.na(pct_vote))
  
geofacet_map_static <- presidential_votes %>% 
  mutate(id = str_c(state, county, sep = ", ")) %>% 
  ggplot(aes(x = pct_vote, fill = party)) +
  geom_density(alpha = .5) +
  scale_fill_manual(values = c("blue", "red")) +
  scale_x_percent(breaks = c(.2, .5, .8)) +
  facet_geo(~ state) +
  labs(title = "Distribution of two-party vote in Presidential elections",
       subtitle = "County-level",
       x = "Percent of two-party vote",
       y = NULL) +
  theme_ipsum() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 20),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 22),
        strip.text = element_text(size = 18, hjust = 0))

ggsave(filename = "output/images/geofacet_map_static.png",
       plot = geofacet_map_static,
       width = 20, height = 10)

presidential_votes %>% 
  mutate(year = as.integer(year)) %>% 
  filter(!(state %in% c("District of Columbia"))) %>% 
  filter(is.na(pct_vote))


geofacet_map_anim <- presidential_votes %>% 
  mutate(year = as.integer(year)) %>% 
  filter(!(state %in% c("District of Columbia"))) %>% 
  mutate(id = str_c(state, county, sep = ", ")) %>% 
  ggplot(aes(x = pct_vote, fill = party)) +
  geom_density(alpha = .5) +
  facet_geo(~state, scales = "free_y") +
  scale_fill_manual(values = c("blue", "red")) +
  scale_x_percent(breaks = c(.2, .5, .8)) +
  theme_ipsum() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 20),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 22),
        strip.text = element_text(size = 18, hjust = 0)) +
  transition_states(year) +
  labs(title = "Year: {closest_state}",
       subtitle = "County-level distribution of two-party vote in Presidential elections",
       x = "Percent of two-party vote",
       y = NULL)


anim_save(filename = "output/images/geofacet_map_anim.gif",
          animation = geofacet_map_anim,
          height = 1500, width = 2000
          )
