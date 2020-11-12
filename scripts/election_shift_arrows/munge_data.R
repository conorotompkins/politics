#script to munge cleaned data

library(tidyverse)
library(lubridate)
library(janitor)
library(sf)
library(tigris)
library(tidycensus)

options(tigris_use_cache = TRUE,
        scipen = 999,
        digits = 4)

data <- read_csv("data/cleaned_president_election_county.csv",
                 col_types = cols(
                   state = col_character(),
                   county = col_character(),
                   fips = col_character(),
                   year = col_double(),
                   office = col_character(),
                   party = col_character(),
                   candidate = col_character(),
                   candidatevotes_sum = col_double()
                 ))


presidential_votes <- data %>% 
  group_by(year, state, county, fips) %>% 
  mutate(pct_vote = candidatevotes_sum / sum(candidatevotes_sum)) %>% 
  ungroup() %>% 
  select(year, state, county, fips, party, candidatevotes_sum, pct_vote)

presidential_votes %>% 
  filter(is.na(candidatevotes_sum))

presidential_votes %>% 
  filter(is.na(pct_vote)) %>% 
  distinct(state, county, fips)

presidential_votes_shift <- presidential_votes %>% 
  pivot_wider(names_from = party, values_from = c(candidatevotes_sum, pct_vote)) %>%
  mutate(dem_margin_pct = pct_vote_democrat - pct_vote_republican,
         dem_margin_votes = candidatevotes_sum_democrat - candidatevotes_sum_republican) %>% 
  arrange(state, county, fips, year) %>% 
  group_by(state, county, fips) %>% 
  mutate(shift_pct = dem_margin_pct - lag(dem_margin_pct),
         shift_votes = dem_margin_votes - lag(dem_margin_votes)) %>% 
  filter(row_number() > 1) %>% 
  ungroup()

presidential_votes_shift %>% 
  filter(is.na(dem_margin_pct))

presidential_votes_shift %>% 
  filter(is.na(shift_pct))

presidential_votes_shift %>% 
  filter(is.na(shift_votes))

presidential_votes_shift %>% 
  count(state, county, fips, year) %>% 
  distinct(n)

presidential_votes_shift %>% 
  write_csv("data/presidential_votes_shift.csv")
