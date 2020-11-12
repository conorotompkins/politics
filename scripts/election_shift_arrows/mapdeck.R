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
library(mapdeck)

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

counties <- county_geo <- get_acs(variables = "B19013_001",
                                  geography = "county", 
                                  geometry = TRUE,
                                  shift_geo = TRUE) %>% 
  st_transform(crs = "WGS84") 

county_centroids <- county_geo %>% 
  select(center_lon_x, center_lat_y) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  rename(lng0 = center_lon_x,
         lat0 = center_lat_y)



shift_map <- presidential_votes_shift %>% 
  #filter(state == "Pennsylvania") %>% 
  mutate(id = str_c(state, county, fips)) %>% 
  left_join(county_geo, by = c("fips" = "GEOID")) %>% 
  mutate(year = as.integer(year)) %>% 
  st_sf() %>% 
  rename(lng0 = center_lon_x,
         lat0 = center_lat_y) %>% 
  mutate(lng1 = lng0 + (shift_votes_scaled * cos(NISTdegTOradian(shift_pct_scaled))),
         lat1 = lat0 + (shift_votes_scaled * sin(NISTdegTOradian(shift_pct_scaled)))) %>% 
  select(lng0, lat0, lng1, lat1, shift_pct) %>% 
  st_drop_geometry() %>% 
  as_tibble()

n <- 100
m <- grDevices::colorRamp(c("red", "grey", "blue"))( (1:n)/n )

x <- seq(0, 1, length.out = 100)
custom_palette2 <- seq_gradient_pal("red", "blue")(x)
show_col(seq_gradient_pal("red", "blue")(x))


mapdeck(style = mapdeck_style('dark')) %>% 
  add_polygon(data = counties,
              stroke_colour = "#FFFFFFFF",
              stroke_width = 500,
              #fill_colour = "#aaaaaa",
              fill_opacity = 0) %>%
  add_scatterplot(data = county_centroids,
                  lon = "lng0",
                  lat = "lat0",
                  stroke_colour = "#FFFFFFFF") %>% 
  add_line(data = shift_map,
           origin = c("lng0", "lat0"),
           destination = c("lng1", "lat1"),
           stroke_colour = "shift_pct",
           stroke_width = 10,
           palette = m)
  
