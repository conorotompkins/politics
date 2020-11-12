library(tidyverse)
library(lubridate)
library(janitor)
library(sf)
library(tigris)
library(tidycensus)
library(gganimate)
library(hrbrthemes)
library(geofacet)
library(scales)
library(NISTunits)
library(tictoc)

#data from https://electionlab.mit.edu/data

options(tigris_use_cache = TRUE,
        scipen = 999,
        digits = 4)

theme_set(theme_ipsum())

#county_geo <- counties(cb = TRUE)

presidential_votes_shift <- read_csv("data/presidential_votes_shift.csv")

presidential_votes_shift %>% 
  filter(is.na(dem_margin_pct)) %>% 
  distinct(state, county, fips)

presidential_votes_shift %>% 
  ggplot(aes(dem_margin_pct)) +
  geom_density() +
  facet_wrap(~year)

presidential_votes_shift %>% 
  ggplot(aes(shift_pct)) +
  geom_density() +
  facet_wrap(~year)



county_geo <- get_acs(variables = "B19013_001",
                      geography = "county", 
                      geometry = TRUE,
                      shift_geo = TRUE) %>% 
  mutate(center = map(geometry, st_centroid)) %>% 
  #mutate(center = map(center, st_coordinates)) %>% 
  mutate(center_lon_x = map_dbl(center, 1),
         center_lat_y = map_dbl(center, 2)) 

county_geo %>% 
  ggplot() +
  geom_sf() +
  geom_point(aes(center_lon_x, center_lat_y), color = "red", size = .1)
    

presidential_votes_shift <- presidential_votes_shift %>% 
  mutate(shift_pct_scaled = rescale(shift_pct, to = c(0, 180)), #republican 0, democrat 180
         shift_votes_scaled = rescale(abs(shift_votes), to = c(10^4, 10^6)))

presidential_votes_shift %>% 
  anti_join(county_geo, by = c("fips" = "GEOID")) %>% 
  distinct(state)


presidential_votes_shift %>% 
  left_join(county_geo, by = c("fips" = "GEOID")) %>% 
  st_sf() %>% 
  filter(year == 2016) %>% 
  ggplot() +
  geom_sf(aes(fill = shift_pct), size = .1) +
  facet_wrap(~year) +
  scale_fill_gradient2(low = "red", mid = "grey", high= "blue", midpoint = 0) +
  theme_void()


presidential_votes_shift %>% 
  ggplot(aes(shift_pct, shift_pct_scaled)) +
  geom_point()

presidential_votes_shift %>% 
  ggplot(aes(shift_pct, shift_votes)) +
  geom_point()

#create shift map object
shift_map <- presidential_votes_shift %>% 
  left_join(county_geo, by = c("fips" = "GEOID")) %>% 
  st_sf() %>% 
  rename(lng0 = center_lon_x,
         lat0 = center_lat_y) %>% 
  mutate(lng1 = lng0 + (shift_votes_scaled * cos(NISTdegTOradian(shift_pct_scaled))),
         lat1 = lat0 + (shift_votes_scaled * sin(NISTdegTOradian(shift_pct_scaled))))


#test arrow map
test_angle <- 180

shift_map %>% 
  filter(str_detect(NAME, "Pennsylvania")) %>% 
  ggplot() +
  geom_sf(size = .01) +
  geom_segment(aes(x = lng0, xend = lng1,
                   y = lat0, yend = lat1),
               arrow = arrow(length = unit(0.03, "inches")), alpha = .9) +
  scale_color_gradient2(low="red", mid = "grey", high="blue", midpoint = 0) +
  facet_wrap(~year) +
  theme_void()

shift_map %>% 
  ggplot() +
  geom_sf(size = .01) +
  geom_segment(aes(x = lng0, xend = lng1,
                   y = lat0, yend = lat1,
                   color = shift_pct),
               arrow = arrow(length = unit(0.03, "inches")), alpha = .9) +
  scale_color_gradient2(low="red", mid = "grey", high="blue", midpoint = 0) +
  facet_wrap(~year) +
  theme_void()
  
  
political_winds_static <- shift_map %>% 
  ggplot() +
  geom_sf(size = .01) +
  geom_segment(aes(x = lng0, xend = lng1,
                   y = lat0, yend = lat1,
                   color = shift_pct),
               arrow = arrow(length = unit(0.03, "inches")), alpha = .9) +
  scale_color_gradient2(low = "red", mid = "grey", high = "blue", midpoint = 0,
                        labels = percent,
                        guide = guide_colorbar(title.position = "top",
                                               barwidth = 30)) +
  facet_wrap(~year) +
  theme_void(base_size = 25) +
  theme(legend.direction = "horizontal",
        legend.position = "bottom")


ggsave(filename = "output/images/political_winds_static.png", plot = political_winds_static,
       width = 20, height = 10, dpi = 300)


#function to make animation per state
animate_state_shifts <- function(choose_state){
  
  state_anim <- shift_map %>% 
    ggplot(aes(group = id)) +
    geom_sf(size = .01) +
    geom_segment(aes(x = lng0, xend = lng1,
                     y = lat0, yend = lat1,
                     color = shift_pct),
                 arrow = arrow(length = unit(0.03, "inches")), alpha = .9) +
    scale_color_gradient2(low="red", mid = "grey", high="blue", midpoint = 0) +
    theme_void() +
    transition_states(year) +
    labs(title = "Year: {closest_state}")
  
  return(state_anim)
}

anim_df <- tibble(state_name = state.name) %>% 
  #slice(1:5) %>% 
  mutate(anim = map(state_name, ~animate_state_shifts(choose_state = .x)),
         anim_play = map(anim, possibly(animate, otherwise = "error")))

bad_states <- anim_df %>% 
  filter(anim_play == "error") %>% 
  pull(state_name)

presidential_votes_shift %>% 
  #filter(!(state %in% c(bad_states))) %>% 
  filter(state == "Virginia") %>% 
  count(state, county, fips) %>% 
  View()

#bedford county VA is causing failure. bedford city merged with the county in 2013, so there is not data for 2016

presidential_votes_shift %>% 
  left_join(county_geo, by = c("fips" = "GEOID")) %>% 
  filter(state == "Virginia") %>% 
  filter(county == "Bedford")

test_anim <- shift_map %>% 
  mutate(id = str_c(state, county, fips)) %>% 
  filter(state == "Pennsylvania") %>% 
  ggplot(aes(group = id)) +
  geom_sf(size = .3) +
  geom_segment(aes(x = lng0, xend = lng1,
                   y = lat0, yend = lat1,
                   color = shift_pct),
               size = 1,
               arrow = arrow(length = unit(0.03, "inches")), alpha = .9) +
  scale_color_gradient2(low = "red", mid = "grey", high = "blue", midpoint = 0,
                        labels = percent,
                        guide = guide_colorbar(title.position = "top",
                                               barwidth = 30)) +
  theme_void(base_size = 25) +
  theme(legend.direction = "horizontal",
        legend.position = "bottom") +
  transition_states(year) +
  labs(title = "Shift in Presidential election Democratic margin",
       subtitle = "Year: {closest_state}",
       color = "Shift in Democratic margin")

test_anim

tic()
political_winds_anim <- shift_map
  mutate(id = str_c(state, county, fips)) %>% 
  mutate(year = as.integer(year)) %>% 
  ggplot(aes(group = id)) +
  geom_sf(size = .3) +
  geom_segment(aes(x = lng0, xend = lng1,
                   y = lat0, yend = lat1,
                   color = shift_pct),
               size = 1,
               arrow = arrow(length = unit(0.03, "inches")), alpha = .9) +
  scale_color_gradient2(low = "red", mid = "grey", high = "blue", midpoint = 0,
                        labels = percent,
                        guide = guide_colorbar(title.position = "top",
                                               barwidth = 30)) +
  theme_void(base_size = 25) +
  theme(legend.direction = "horizontal",
        legend.position = "bottom") +
  transition_states(year) +
  labs(title = "Shift in Presidential election Democratic margin",
       subtitle = "Year: {closest_state}",
       color = "Shift in Democratic margin")

anim_save(filename = "output/images/political_winds_anim.gif",
          animation = political_winds_anim,
          height = 1500, width = 2000, fps = 30, duration = 12
          )
toc()