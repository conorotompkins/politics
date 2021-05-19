#poc
library(tidyverse)
library(janitor)
library(sf)
library(tigris)
library(tidycensus)
library(hrbrthemes)
library(geofacet)
library(scales)
library(NISTunits)

#data from https://electionlab.mit.edu/data

options(tigris_use_cache = TRUE,
        scipen = 999,
        digits = 4)

theme_set(theme_ipsum())

county_geo <- get_acs(variables = "B19013_001",
                      geography = "county", 
                      geometry = TRUE,
                      shift_geo = TRUE) %>% 
  #calculate the center of each county and extract coordinates
  mutate(center = map(geometry, st_centroid)) %>% 
  mutate(center_lon_x = map_dbl(center, 1),
         center_lat_y = map_dbl(center, 2)) 


#read in pa election data
pa_shift <- read_csv("https://raw.githubusercontent.com/conorotompkins/politics/master/data/presidential_votes_shift.csv",
                     col_types = cols(
                       year = col_double(),
                       state = col_character(),
                       county = col_character(),
                       fips = col_character(),
                       candidatevotes_sum_democrat = col_double(),
                       candidatevotes_sum_republican = col_double(),
                       pct_vote_democrat = col_double(),
                       pct_vote_republican = col_double(),
                       dem_margin_pct = col_double(),
                       dem_margin_votes = col_double(),
                       shift_pct = col_double(),
                       shift_votes = col_double()
                     ))
#
pa_shift <- pa_shift %>% 
  #this calculates the angle to draw the arrow at
  mutate(shift_pct_scaled = rescale(shift_pct, to = c(0, 180)), #republican 0, democrat 180
         #this rescales the raw vote count into meters. this is used to determine the length of the arrow
         shift_votes_scaled = rescale(abs(shift_votes), to = c(10^4, 10^6)))

#create shift map object
shift_map <- pa_shift %>% 
  #join pa shift data with county geometry
  left_join(county_geo, by = c("fips" = "GEOID")) %>% 
  st_sf() %>% 
  rename(lng0 = center_lon_x,
         lat0 = center_lat_y) %>% 
  #this calculates where the arrow will end. it calculates how far to shift the point and then adds that distance to the origin point
  mutate(lng1 = lng0 + (shift_votes_scaled * cos(NISTdegTOradian(shift_pct_scaled))),
         lat1 = lat0 + (shift_votes_scaled * sin(NISTdegTOradian(shift_pct_scaled))))


shift_map %>% 
  filter(state == "Pennsylvania") %>% 
  ggplot() +
  geom_sf(size = .01) +
  geom_segment(aes(x = lng0, xend = lng1,
                   y = lat0, yend = lat1,
                   color = shift_pct),
               arrow = arrow(length = unit(0.03, "inches")), alpha = .9) +
  scale_color_gradient2(low="red", mid = "grey", high="blue", midpoint = 0) +
  facet_wrap(~year) +
  theme_void()
