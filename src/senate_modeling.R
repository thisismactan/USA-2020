src("data/shape_house_data.R")

# Presidential leans by state and year
historical_presidential_results <- read_csv("data/presidential_election_results_by_state.csv")

historical_national_presidential_results <- historical_presidential_results %>%
  group_by(year, party) %>%
  summarise(votes = sum(votes)) %>%
  group_by(year) %>%
  mutate(votes = votes / sum(votes)) %>%
  spread(party, votes) %>%
  mutate(natl_margin = Democratic - Republican) %>%
  dplyr::select(year, natl_margin)

presidential_leans <- historical_presidential_results %>%
  group_by(year, state, party) %>%
  summarise(votes = sum(votes)) %>%
  group_by(year, state) %>%
  mutate(votes = votes / sum(votes)) %>%
  spread(party, votes) %>%
  mutate(state_margin = Democratic - Republican) %>%
  dplyr::select(year, state, state_margin) %>%
  left_join(historical_national_presidential_results, by = "year") %>%
  mutate(pres_lean = state_margin - natl_margin) %>%
  ungroup()

## Maybe two totally separate models, one for incumbents and one for non-incumbents
historical_senate_incumbents <- read_csv("data/historical_senate_incumbents.csv")

historical_senate_results <- read_csv("data/senate_results_1976-2018.csv") %>%
  filter(year >= 2000, stage == "gen", grepl("democrat|republican", party)) %>%
  dplyr::select(year, state, special, party, candidatevotes) %>%
  left_join(historical_senate_incumbents, by = c("state", "year", "special")) %>%
  mutate(party = case_when(grepl("republican", party) ~ "REP",
                           grepl("democrat", party) ~ "DEM")) %>%
  group_by(year, state, class, special, democrat_running, republican_running, incumbent_running, incumbent_first_elected, party) %>%
  summarise(party_votes = sum(candidatevotes)) %>%
  group_by(year, state, class) %>%
  mutate(party_votes = party_votes / sum(party_votes)) %>%
  spread(party, party_votes, fill = 0) %>%
  ungroup() %>%
  arrange(state, class, year) %>%
  mutate(margin = DEM - REP) %>%
  group_by(state, class) %>%
  mutate(last_margin = lag(margin),
         last_incumbent = lag(incumbent_running))

historical_senate_results_filtered <- historical_senate_results %>%
  # Remove appointees and elections from 2000-2004
  na.omit() %>%
  
  # Remove elections where either this election or the last one went uncontested or where the incumbent or the last incumbent was an independent
  # and filter down to presidential years
  filter(democrat_running, republican_running, lag(democrat_running), lag(republican_running), incumbent_running != "IND", 
         last_incumbent != "IND", year %% 4 == 0) %>%
  
  # Join presidential election results
  left_join(presidential_leans, by = c("year", "state"))

historical_senate_results_filtered %>%
  ggplot(aes(x = state_margin, y = margin)) +
  geom_point() +
  facet_wrap(~open_seat)

senate_lm <- lm(margin ~ state_margin + last_margin + incumbent_running, data = historical_senate_results_filtered)
summary(senate_lm)
