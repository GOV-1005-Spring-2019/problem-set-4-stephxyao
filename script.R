library(readr)
library(tidyverse)

elections <- read_csv("ps_4_elections-poll-nc09-3.csv")
  
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

