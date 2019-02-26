library(readr)
library(tidyverse)
library(gt)

elections <- read_csv("ps_4_elections-poll-nc09-3.csv",
                      col_types = cols(
                        .default = col_character(),
                        turnout_scale = col_double(),
                        turnout_score = col_double(),
                        w_LV = col_double(),
                        w_RV = col_double(),
                        final_weight = col_double(),
                        timestamp = col_datetime(format = "")))
  
# support of democratic candidate
dems <- elections %>%
  filter(response == "Dem") %>%
  summarize(dems = n())

# support of republican candidate
reps <- elections %>%
  filter(response == "Rep") %>%
  summarize(reps = n())

# undecided
unds <- elections %>%
  filter(response == "Und") %>%
  summarize(unds = n())

# number of people with different values for gender and gender_combined
differing_gender <- elections %>%
  filter(gender != gender_combined) %>%
  summarize(differing_gender = n())

differing_ethnicity <- elections %>%
  filter(race_eth == "White", file_race_black != "White") %>%
  summarize(differing_ethnicity = n())

# first responses
first_responses <- elections %>%
  group_by(response) %>%
  summarize(first_response = min(timestamp))

# question 2

# questions for OH
# do we need to reinstall gt every time?
# how should we deal with the rows where file_race does not match race_eth?

elections %>%
  select(response, final_weight, file_race) %>%
  group_by(file_race, response) %>%
  summarize(total = sum(final_weight)) %>%
  #filter(file_race != race_eth) %>%
  spread(key = response, value = total) %>%
  mutate(all = Dem + Rep + Und) %>% 
  mutate(Dem = Dem / all) %>% 
  mutate(Rep = Rep / all) %>% 
  mutate(Und = Und / all) %>% 
  select(-all) %>% 
  ungroup() %>%
  gt() %>%
    tab_header(title = "insert title here") %>%
    cols_label(
      file_race = "Race",
      Dem = "DEM.",
      Rep = "REP.",
      Und = "UND."
    ) %>%
  fmt_percent(columns = vars(Dem, Rep, Und), decimals = 0) %>% 
  as_raw_html() %>% as.character() %>% cat()
  
  

