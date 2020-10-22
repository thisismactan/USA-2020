source("src/library.R")

# Download polls from FiveThirtyEight
download.file("https://projects.fivethirtyeight.com/polls-page/president_polls.csv", destfile = "data/president_polls.csv")
download.file("https://projects.fivethirtyeight.com/polls-page/senate_polls.csv", destfile = "data/senate_polls.csv")
download.file("https://projects.fivethirtyeight.com/polls-page/house_polls.csv", destfile = "data/house_district_polls.csv")
download.file("https://projects.fivethirtyeight.com/polls-page/generic_ballot_polls.csv", destfile = "data/generic_ballot_polls.csv")

# President
president_polls <- read_csv("data/president_polls.csv") %>%
  dplyr::select(poll_id, state, pollster, question_id, start_date, end_date, n = sample_size, pop = population, mode = methodology, 
                party = partisan, tracking, candidate = candidate_name, pct) %>%
  mutate(pct = pct / 100,
         start_date = as.Date(start_date, format = "%m/%d/%y"),
         end_date = as.Date(end_date, format = "%m/%d/%y"),
         spread = as.numeric(end_date - start_date) + 1,
         median_date = start_date + round(spread / 2),
         age = as.numeric(today() - median_date),
         state = case_when(is.na(state) ~ "National",
                           !is.na(state) ~ state),
         party = case_when(!is.na(party) ~ party,
                           is.na(party) ~ "None"),
         loess_weight = (n^0.25) * ifelse(spread == 1, 1, 5) * ifelse(grepl("IVR|Automated", mode)|is.na(mode), 1, 2) * ifelse(pop == "lv", 3, 1) *
           ifelse(mode == "Live Phone", 2, 1) * ifelse(party == "None", 4, 1) * ifelse(is.na(tracking), 1, 1 / spread) *
           ifelse(pollster == "USC Dornsife/Los Angeles Times", 2, 1) / sqrt(abs(spread - 4) + 2)) %>%
  group_by(poll_id, question_id) %>%
  mutate(biden_v_trump = any(grepl("Biden", candidate)) & any(grepl("Trump", candidate)),
         has_3p = any(!grepl("Biden|Trump", candidate))) %>%
  ungroup() %>%
  filter(biden_v_trump, pop %in% c("lv", "rv", "v"), grepl("Biden|Trump", candidate)) %>%
  mutate(candidate = case_when(!grepl("Biden|Trump", candidate) ~ candidate,
                               grepl("Biden", candidate) ~ "biden",
                               grepl("Trump", candidate) ~ "trump"))

## National vs. state polls
national_president_polls <- president_polls %>% 
  filter(state == "National")

state_president_polls <- president_polls %>%
  filter(state != "National")

# House generic ballot
generic_ballot_polls <- read_csv("data/generic_ballot_polls.csv") %>%
  filter(is.na(state), population %in% c("rv", "lv")) %>%
  dplyr::select(poll_id, pollster, question_id, start_date, end_date, n = sample_size, pop = population, mode = methodology, 
                party = partisan, tracking, dem, rep) %>%
  melt(measure.vars = c("dem", "rep"), variable.name = "candidate", value.name = "pct") %>%
  mutate(pct = pct / 100,
         start_date = as.Date(start_date, format = "%m/%d/%y"),
         end_date = as.Date(end_date, format = "%m/%d/%y"),
         spread = as.numeric(end_date - start_date) + 1,
         median_date = start_date + round(spread / 2),
         age = as.numeric(today() - median_date),
         party = case_when(!is.na(party) ~ party,
                           is.na(party) ~ "None"),
         party = case_when(grepl("McLaughlin", pollster) ~ "REP",
                           !grepl("McLaughlin", pollster) ~ party),
         loess_weight = (n^0.25) * ifelse(spread == 1, 1, 5) * ifelse(grepl("IVR|Automated", mode)|is.na(mode), 1, 2) * ifelse(pop == "lv", 3, 1) *
           ifelse(mode == "Live Phone", 2, 1) * ifelse(party == "None", 4, 1) * ifelse(is.na(tracking), 1, 1 / spread) / sqrt(abs(spread - 4) + 2)) %>%
  group_by(poll_id, question_id) %>%
  ungroup()

# Individual House district polls
house_district_polls <- read_csv("data/house_district_polls.csv") %>%
  filter(population %in% c("rv", "lv", "v"), cycle == 2020) %>%
  dplyr::select(poll_id, state, seat_number, pollster, question_id, start_date, end_date, n = sample_size, pop = population, mode = methodology, 
                party = partisan, tracking, candidate_party, candidate = candidate_name, pct) %>%
  mutate(pct = pct / 100,
         start_date = as.Date(start_date, format = "%m/%d/%y"),
         end_date = as.Date(end_date, format = "%m/%d/%y"),
         spread = as.numeric(end_date - start_date) + 1,
         median_date = start_date + round(spread / 2),
         age = as.numeric(today() - median_date),
         party = case_when(!is.na(party) ~ party,
                           is.na(party) ~ "None"),
         party = case_when(grepl("McLaughlin", pollster) ~ "REP",
                           !grepl("McLaughlin", pollster) ~ party),
         mode = case_when(is.na(mode) ~ "IVR",
                          !is.na(mode) ~ mode),
         n = case_when(is.na(n) ~ 300,
                       !is.na(n) ~ n),
         loess_weight = (n^0.25) * ifelse(spread == 1, 1, 5) * ifelse(grepl("IVR|Automated", mode), 1, 2) * ifelse(pop == "lv", 3, 1) *
           ifelse(mode == "Live Phone", 2, 1) * ifelse(party == "None", 6, 1) * ifelse(is.na(tracking), 1, 1 / spread) / sqrt(abs(spread - 4) + 2)) %>%
  
  # Pathological cases (primary polls, special elections)
  filter(!(question_id %in% c(100720, 100721, 100722, 100723, 103717, 103799, 103800, 114021, 115665, 116568, 117478, 118912, 118010, 118011,
                              118012, 120204)),
         start_date >= as.Date("2020-01-01"))

# Senate
senate_polls_all <- read_csv("data/senate_polls.csv") %>%
  filter(!is.na(state), population %in% c("rv", "lv", "v"), candidate_party %in% c("DEM", "REP"), cycle == 2020) %>%
  dplyr::select(poll_id, state, seat_name, pollster, question_id, start_date, end_date, n = sample_size, pop = population, mode = methodology, 
                party = partisan, tracking, candidate_party, candidate = candidate_name, pct) %>%
  mutate(pct = pct / 100,
         start_date = as.Date(start_date, format = "%m/%d/%y"),
         end_date = as.Date(end_date, format = "%m/%d/%y"),
         spread = as.numeric(end_date - start_date) + 1,
         median_date = start_date + round(spread / 2),
         age = as.numeric(today() - median_date),
         party = case_when(!is.na(party) ~ party,
                           is.na(party) ~ "None"),
         party = case_when(grepl("McLaughlin", pollster) ~ "REP",
                           !grepl("McLaughlin", pollster) ~ party),
         loess_weight = (n^0.25) * ifelse(spread == 1, 1, 5) * ifelse(grepl("IVR|Automated", mode), 1, 2) * ifelse(pop == "lv", 3, 1) *
           ifelse(mode == "Live Phone", 2, 1) * ifelse(party == "None", 4, 1) * ifelse(is.na(tracking), 1, 1 / spread) / sqrt(abs(spread - 4) + 2))

georgia_primary_candidates <- senate_polls_all %>%
  filter(state == "Georgia", seat_name == "Class III") %>%
  dplyr::select(candidate, candidate_party) %>%
  distinct()

georgia_primary_polls <- senate_polls_all %>%
  filter(state == "Georgia", seat_name == "Class III") %>%
  group_by(question_id) %>%
  mutate(n_cands = n()) %>%
  filter(n_cands > 2) %>%
  spread(candidate, pct, fill = 0.005) %>%
  melt(id.vars = c("poll_id", "state", "seat_name", "pollster", "question_id", "start_date", "end_date", "n", "pop", "mode", "party", "tracking",
                   "spread", "median_date", "age", "loess_weight", "n_cands", "candidate_party"), 
       variable.name = "candidate", value.name = "pct") %>%
  as.tbl() %>%
  inner_join(georgia_primary_candidates, by = c("candidate", "candidate_party")) %>%
  arrange(age) %>%
  filter(candidate_party %in% c("DEM", "REP"))

georgia_runoff_polls <- senate_polls_all %>%
  filter(state == "Georgia", seat_name == "Class III") %>%
  group_by(question_id) %>%
  mutate(n_cands = n()) %>%
  filter(n_cands == 2)

georgia_question_matchups <- georgia_runoff_polls %>%
  dplyr::select(question_id, candidate) %>%
  group_by(question_id) %>%
  mutate(candidate_num = 1:2) %>%
  ungroup() %>%
  spread(candidate_num, candidate) %>%
  mutate(matchup = paste(`1`, "vs.", `2`)) %>%
  dplyr::select(question_id, matchup)

georgia_runoff_polls <- georgia_runoff_polls %>%
  left_join(georgia_question_matchups, by = "question_id")

## Candidates
senate_candidates <- read_csv("data/senate_candidates.csv") %>%
  dplyr::select(state, seat_name, candidate_party, candidate_fullname) %>%
  spread(candidate_party, candidate_fullname)
  

## Filter polls to those with the appropriate candidate matchups
senate_poll_candidates <- senate_polls_all %>%
  dplyr::select(state, seat_name, question_id, candidate_party, candidate) %>%
  filter(seat_name == "Class II" | state == "Arizona", candidate_party %in% c("DEM", "REP"), state != "Louisiana") %>%
  spread(candidate_party, candidate)

senate_question_ids <- senate_candidates %>%
  left_join(senate_poll_candidates, by = c("state", "seat_name", "DEM", "REP")) %>%
  filter(!is.na(question_id)) %>%
  pull(question_id)

senate_polls <- senate_polls_all %>%
  filter(question_id %in% senate_question_ids)
