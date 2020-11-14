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
  #filter out state-wide ballot collection
  filter(!(state == "Connecticut" & county == "Statewide writein")) %>% 
  filter(!(state == "Maine" & county == "Maine UOCAVA")) %>% 
  filter(!(state == "Rhode Island" & county == "Federal Precinct"))

data %>% 
  filter(is.na(fips_raw)) %>% 
  arrange(state, county, year)

data %>% 
  filter(is.na(candidatevotes)) %>% 
  distinct(state, county, year) %>% 
  arrange(state, county, year)

#filter for only 2-party vote in presidential elections
data <- data %>% 
  filter(office == "President",
         party == "democrat" | party == "republican") %>% 
  arrange(state, county, fips_raw, year) %>% 
  replace_na(list(candidatevotes = 0))

data %>% 
  filter(is.na(candidatevotes)) %>% 
  distinct(state, county, year) %>% 
  arrange(state, county, year)

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

#decisions to make with wonky geometry
#merge Shannnon and Oglala Lakota counties in SD
#merge Kansas City Missouri and Jackson County Missouri
#merge Bedford (city) fips 51515 with Bedford county 51019

data <- data %>% 
  #add "0" to front of states where leading "0" was dropped
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
#Broomfield county 08014 separated from Boulder County 08013 in 2001
# 
# data <- data %>% 
#   mutate(census_geo_year = case_when(year < 2010 ~ 2000,
#                                      year >= 2010 ~ 2010))

data %>% 
  filter(state %in% states_with_bad_fips) %>% 
  distinct(state, fips_raw, fips)

data %>% 
  filter(is.na(fips))

data %>% 
  filter(str_detect(county, "Lakota|Shannon"),
         state == "South Dakota") %>% 
  distinct(state, county, fips)

data %>% 
  filter(state == "Virginia", county == "Bedford") %>% 
  distinct(state, county, fips, year)

county_geo <- get_acs(variables = "B19013_001",
                      geography = "county",
                      geometry = FALSE) %>% 
  #mutate(census_geo_year = 2010) %>% 
  select(NAME, GEOID)

data %>% 
  filter(str_detect(county, "Lakota|Shannon"),
         state == "South Dakota") %>% 
  distinct(state, county, fips)

county_geo %>% 
  filter(str_detect(NAME, "Lakota|Shannon"))

data %>% 
  filter(state == "Virginia" & county == "Bedford") %>% 
  distinct(year, state, county, fips)

#alaska falls out: this is expected
#Broomfield County CO falls out for year 2000: was part of Boulder County in 2000
#Oglala Lakota County SD falls out for year 2000: was Shannon County in 2000
#
data %>% 
  select(year, state, county, fips) %>% 
  anti_join(county_geo, by = c("fips" = "GEOID")) %>% 
  View()

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

