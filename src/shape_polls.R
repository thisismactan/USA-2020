source("src/library.R")

# Download polls from FiveThirtyEight
download.file("https://projects.fivethirtyeight.com/polls-page/president_polls.csv", destfile = "data/president_polls.csv")
download.file("https://projects.fivethirtyeight.com/polls-page/senate_polls.csv", destfile = "data/senate_polls.csv")
download.file("https://projects.fivethirtyeight.com/polls-page/house_polls.csv", destfile = "data/house_district_polls.csv")
download.file("https://projects.fivethirtyeight.com/polls-page/generic_ballot_polls.csv", destfile = "data/generic_ballot_polls.csv")

# President
president_polls <- read_csv("data/president_polls.csv") %>%
  dplyr::select(poll_id, state, pollster, question_id, start_date, end_date, n = sample_size, pop = population, mode = methodology, 
                party = partisan, candidate = candidate_name, pct) %>%
  mutate(start_date = as.Date(start_date, format = "%m/%d/%y"),
         end_date = as.Date(end_date, format = "%m/%d/%y"),
         spread = as.numeric(end_date - start_date) + 1,
         median_date = start_date + round(spread / 2),
         age = as.numeric(today() - poll_date)) %>%
  group_by(poll_id, question_id) %>%
  mutate(biden_v_trump = any(grepl("Biden", candidate)) & any(grepl("Trump", candidate)),
         has_3p = any(!grepl("Biden|Trump", candidate))) %>%
  ungroup() %>%
  filter(biden_v_trump)
