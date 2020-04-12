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
         loess_weight = (n^0.25) * ifelse(spread == 1, 1, 5) * ifelse(grepl("IVR|Automated", mode), 1, 2) * ifelse(pop == "lv", 3, 1) *
           ifelse(mode == "Live Phone", 2, 1) * ifelse(party == "nONE", 4, 1) * ifelse(is.na(tracking), 2, 1) / sqrt(abs(spread - 4) + 2)) %>%
  group_by(poll_id, question_id) %>%
  mutate(biden_v_trump = any(grepl("Biden", candidate)) & any(grepl("Trump", candidate)),
         has_3p = any(!grepl("Biden|Trump", candidate))) %>%
  ungroup() %>%
  filter(biden_v_trump, pop %in% c("lv", "rv"), !(candidate %in% c("Howard Schultz", "Justin Amash"))) %>%
  mutate(candidate = case_when(!grepl("Biden|Trump", candidate) ~ candidate,
                               grepl("Biden", candidate) ~ "biden",
                               grepl("Trump", candidate) ~ "trump"))

# National vs. state polls
national_president_polls <- president_polls %>% 
  filter(state == "National")

state_president_polls <- president_polls %>%
  filter(state != "National")
