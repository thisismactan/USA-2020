source("src/library.R")

# Two-party Democratic margins at district, state, and national levels
historical_house_incumbents <- read_csv("data/historical_house_incumbents.csv")
house_results <- read_csv("data/house_results_2000-2018.csv") %>%
  left_join(historical_house_incumbents, by = c("year", "state", "seat_number"))

state_house_results <- house_results %>%
  filter(party %in% c("DEM", "REP")) %>%
  group_by(year, state, party) %>%
  summarise(votes = sum(candidatevotes)) %>%
  mutate(pct = votes / sum(votes),
         state_margin = pct - lead(pct)) %>%
  na.omit() %>%
  dplyr::select(year, state, state_margin) %>%
  arrange(state, year) %>%
  group_by(state) %>%
  mutate(state_margin_change = state_margin - lag(state_margin)) %>%
  ungroup()

national_house_results <- house_results %>%
  filter(party %in% c("DEM", "REP")) %>%
  group_by(year, party) %>%
  summarise(votes = sum(candidatevotes)) %>%
  mutate(pct = votes / sum(votes),
         natl_margin = pct - lead(pct)) %>%
  na.omit() %>%
  dplyr::select(year, natl_margin) %>%
  ungroup() %>%
  mutate(natl_margin_change = natl_margin - lag(natl_margin)) 

house_results_2party <- house_results %>%
  filter(!runoff, !special, !writein) %>%
  # Handle multiple candidate from a party (as in states with top-two primaries)
  group_by(year, state, seat_number, party, incumbent_running, incumbent_first_elected, democrat_running, republican_running, redistricted) %>%
  summarise(partyvotes = sum(candidatevotes)) %>%
  group_by(year, state, seat_number) %>%
  filter(party %in% c("REP", "DEM")) %>%
  mutate(pct = partyvotes / sum(partyvotes)) %>%
  ungroup() %>%
  left_join(regions %>% dplyr::select(state, region), by = "state") %>%
  dplyr::select(year, state, seat_number, region, incumbent_running, democrat_running, republican_running, redistricted, party, pct) %>%
  spread(party, pct, fill = 0) 

house_results_2party_filtered <- house_results_2party %>%
  mutate(margin = DEM - REP,
         pres_year = year %% 4 == 0) %>%
  group_by(state, seat_number) %>%
  arrange(state, seat_number, year) %>%
  mutate(last_margin = lag(margin), 
         incumbency_change = paste(lag(incumbent_running), incumbent_running, sep = " to ")) %>%
  filter(democrat_running, republican_running, !redistricted, !lag(redistricted), lag(democrat_running), lag(republican_running), 
         !grepl("IND", incumbency_change)) %>%
  left_join(national_house_results %>% mutate(last_natl_margin = lag(natl_margin)), by = "year") %>%
  left_join(state_house_results %>% mutate(last_state_margin = lag(state_margin)), by = c("year", "state")) %>%
  dplyr::select(year, pres_year, state, seat_number, region, incumbent_running, incumbency_change, margin, last_margin, natl_margin, 
                last_natl_margin, state_margin, last_state_margin) %>%
  na.omit()

house_results_2party_filtered$incumbency_change <- factor(house_results_2party_filtered$incumbency_change)
house_results_2party_filtered
