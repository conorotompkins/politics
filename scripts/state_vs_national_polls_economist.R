library(tidyverse)
library(lubridate)
library(janitor)
library(hrbrthemes)

theme_set(theme_ipsum())


polls <- read_csv("data/2020 US presidential election polls - all_polls.csv") %>% 
  clean_names() %>% 
  mutate(flag_national_poll = str_detect(state, "--"),
         state = case_when(str_detect(state, "--") ~ NA_character_,
                           TRUE ~ state)) %>% 
  mutate(end_date = case_when(end_date == "7/14/20220" ~ "7/14/2020",
                              end_date == "5/19/0220" ~ "5/19/2020",
                              TRUE ~ end_date),
         start_date = case_when(start_date == "5/17/2002" ~ "5/17/2020",
                                TRUE ~ start_date)) %>% 
  mutate(end_date = mdy(end_date),
         start_date = mdy(start_date))
  
glimpse(polls)

polls_state <- polls %>% 
  filter(!is.na(state)) %>% 
  group_by(state) %>%
  filter(n() > 10) %>% 
  ungroup()

polls_national <- polls %>% 
  filter(flag_national_poll == TRUE) %>% 
  select(-state)

state_vs_national_polls <- polls_state %>% 
  add_count(state) %>% 
  mutate(state_label = str_c("State: ", state, " (n = ", n, ")", sep = ""),
         state_label = fct_reorder(state_label, desc(n))) %>% 
  arrange(state_label, end_date) %>% 
  select(state_label, end_date, biden_margin) %>% 
  ggplot(aes(end_date, biden_margin)) +
  #geom_vline(xintercept = ymd("2020-05-15"), linetype = 2) +
  geom_hline(yintercept = 0, alpha = .5) +
  geom_point(alpha = .2, color = "blue") +
  geom_smooth(data = polls_national,
              color = "black", alpha = .2) +
  geom_smooth(color = "blue", fill = "blue", alpha = .2) +
  #annotate("text", x = ymd("2020-04-01"), y = -5, label = "State", color = "blue") +
  #annotate("text", x = ymd("2020-04-01"), y = 12, 
  #         label = "National", color = "black") +
  facet_wrap(~state_label, scale = "free_y") +
  labs(title = "Biden's margin in state vs. national polls",
       subtitle = "Black line indicates national polls",
       x = NULL,
       y = "Biden margin in %",
       caption = "@conor_tompkins, data from The Economist") +
  theme(strip.text = element_text(color = "blue"))

ggsave("output/images/state_vs_national_polls.png", width = 12, height = 8, dpi = 300)

polls_state %>% 
  ggplot(aes(end_date, biden_margin, group = state)) +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE)


