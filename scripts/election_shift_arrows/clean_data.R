#election shift
#script to clean data

library(tidyverse)
library(lubridate)
library(janitor)
library(sf)
library(tigris)
library(tidycensus)

#data from https://electionlab.mit.edu/data

options(tigris_use_cache = TRUE,
        scipen = 999,
        digits = 4)


#fips info
#https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code#FIPS_state_codes
#https://en.wikipedia.org/wiki/List_of_United_States_FIPS_codes_by_county
#changes https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes.2010.html

#read in data
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
  rename(fips_raw = fips) %>% 
  filter(!(state == "Connecticut" & county == "Statewide writein")) %>% 
  filter(!(state == "Maine" & county == "Maine UOCAVA")) %>% 
  filter(!(state == "Rhode Island" & county == "Federal Precinct"))

data %>% 
  filter(is.na(fips_raw)) %>% 
  arrange(state, county, year) %>% 
  View()

data %>% 
  filter(is.na(candidatevotes)) %>% 
  arrange(state, county, year) %>% 
  View()


data <- data %>% 
  filter(office == "President",
         party == "democrat" | party == "republican") %>% 
  arrange(state, county, fips_raw, year) %>% 
  replace_na(list(candidatevotes = 0))

data %>% 
  filter(is.na(candidatevotes)) %>% 
  arrange(state, county, year) %>% 
  View()

glimpse(data)

data %>% 
  filter(state == "Alabama") %>% 
  distinct(fips_raw)

#clean fips data
states_with_bad_fips <- str_to_title(c("ALABAMA", "ALASKA", "ARIZONA", 
                                      "ARKANSAS", "CALIFORNIA",
                                      "COLORADO", "CONNECTICUT"))
data %>% 
  filter(state %in% states_with_bad_fips) %>% 
  mutate(fips = paste0("0", fips_raw)) %>% 
  distinct(fips_raw, fips)

data <- data %>% 
  mutate(fips = case_when(state %in% states_with_bad_fips ~ paste0("0", fips_raw),
                          !(state %in% states_with_bad_fips) ~ fips_raw)) %>%
  #update Oglala Lakota SD fips
  #changed in 2015 https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes.2010.html
  mutate(fips = case_when(state == "South Dakota" & county == "Oglala Lakota" ~ "46102",
                          TRUE ~ fips)) %>% 
  #merge Kansas City Missouri with Jackson County Missouri
  mutate(county = case_when(state == "Missouri" & county == "Kansas City" ~ "Jackson",
                            TRUE ~ county),
         fips = case_when(state == "Missouri" & county == "Jackson" ~ "29095",
                          TRUE ~ fips)) %>% 
  #merge Bedford (city) fips 51515 with Bedford county 51019
  mutate(fips = case_when(state == "Virginia" & county == "Bedford" & fips == "51515" ~ "51019",
                          TRUE ~ fips))

data %>% 
  filter(state %in% states_with_bad_fips) %>% 
  distinct(state, fips_raw, fips)

data %>% 
  filter(is.na(fips))

data %>% 
  filter(str_detect(county, "Lakota")) %>% 
  distinct(state, county, fips)


county_geo <- get_acs(variables = "B19013_001",
                      geography = "county", 
                      geometry = TRUE,
                      shift_geo = TRUE) %>% 
  select(NAME, GEOID) %>% 
  st_drop_geometry()

data %>% 
  filter(str_detect(county, "Lakota")) %>% 
  distinct(state, county, fips)

county_geo %>% 
  filter(str_detect(NAME, "Lakota"))

data %>% 
  filter(state == "Virginia" & county == "Bedford") %>% 
  distinct(year, state, county, fips)

data %>% 
  select(year, state, county, fips) %>% 
  anti_join(county_geo, by = c("fips" = "GEOID")) %>% 
  distinct(state)

#some counties have 4 records because of merging process
data %>%
  select(state, county, fips, year) %>% 
  add_count(state, county, fips, year) %>% 
  distinct(n)

data %>%
  #select(state, county, fips, year) %>% 
  add_count(state, county, fips, year) %>% 
  arrange(desc(n))

#summarize candidatevotes to account for merged counties
data %>% 
  select(state, county, fips, year, office, party, candidate, candidatevotes) %>% 
  group_by(state, county, fips, year, office, party, candidate) %>% 
  summarize(candidatevotes_sum = sum(candidatevotes)) %>% 
  ungroup() %>% 
  add_count(state, county, fips, year) %>% 
  #confirm that each county only has 2 records
  distinct(n)

data <- data %>% 
  select(state, county, fips, year, office, party, candidate, candidatevotes) %>% 
  group_by(state, county, fips, year, office, party, candidate) %>% 
  summarize(candidatevotes_sum = sum(candidatevotes)) %>% 
  ungroup()

data %>% 
  write_csv(., "data/cleaned_president_election_county.csv")

