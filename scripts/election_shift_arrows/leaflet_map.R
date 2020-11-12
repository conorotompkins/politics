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
library(leaflet)
library(leaflet.minicharts)

#data from https://electionlab.mit.edu/data

options(tigris_use_cache = TRUE,
        scipen = 999,
        digits = 4)

theme_set(theme_ipsum())

#county_geo <- counties(cb = TRUE)

presidential_votes_shift <- read_csv("data/presidential_votes_shift.csv") %>% 
  filter(year == 2016)

presidential_votes_shift <- presidential_votes_shift %>% 
  mutate(shift_pct_scaled = rescale(shift_pct, to = c(0, 180)), #republican 0, democrat 180
         shift_votes_scaled = rescale(abs(shift_votes), to = c(0, 10)))

county_geo <- get_acs(variables = "B19013_001",
                      geography = "county", 
                      geometry = TRUE,
                      shift_geo = TRUE) %>% 
  st_transform(crs = "WGS84") %>% 
  mutate(center = map(geometry, st_centroid)) %>% 
  #mutate(center = map(center, st_coordinates)) %>% 
  mutate(center_lon_x = map_dbl(center, 1),
         center_lat_y = map_dbl(center, 2))  

#leaflet
leaflet_data <- presidential_votes_shift %>% 
  #filter(!(state %in% c(bad_states))) %>% 
  filter(state == "Pennsylvania") %>% 
  #filter(county != "Bedford") %>% 
  mutate(id = str_c(state, county, fips)) %>% 
  left_join(county_geo, by = c("fips" = "GEOID")) %>% 
  mutate(year = as.integer(year)) %>% 
  st_sf() %>% 
  rename(lng0 = center_lon_x,
         lat0 = center_lat_y) %>% 
  mutate(lng1 = lng0 + (shift_votes_scaled * cos(NISTdegTOradian(shift_pct_scaled))),
         lat1 = lat0 + (shift_votes_scaled * sin(NISTdegTOradian(shift_pct_scaled))))

leaflet_data_counties <- leaflet_data %>% 
  select(geometry)

leaflet_data_counties %>% 
  ggplot() +
  geom_sf()


leaflet() %>% 
  addPolygons(data = leaflet_data_counties,
              fillOpacity = 0) %>%
  # addCircles(data = st_point_on_surface(leaflet_data_counties),
  #            color = "red") %>% 
  addFlows(leaflet_data$lng0, leaflet_data$lat0,
           leaflet_data$lng1, leaflet_data$lat1,
           flow = leaflet_data$shift_votes_scaled,
           #color = leaflet_data$shift_pct,
           dir = 1,
           maxThickness = 4)
  
