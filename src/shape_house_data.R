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

# Fundraising
election_years <- seq(from = 2006, to = 2020, by = 2)
fundraising <- vector("list", length(election_years))
for(y in 1:length(election_years)) {
  fundraising_filename <- paste0("data/fec-data/fundraising_", election_years[y], ".csv")
  fundraising[[y]] <- read.csv(fundraising_filename, header = FALSE) %>%
    mutate(year = election_years[y],
           V1 = gsub("ï»¿", "", V1)) 
}

fundraising <- bind_rows(fundraising) %>% 
  as.tbl() %>%
  inner_join(regions %>% dplyr::select(state, abbrev), by = c("V19" = "abbrev")) %>%
  dplyr::select(year, state, seat_number = V20, candidate_id = V1, candidate_name = V2, candidate_status = V3, party_code = V4, party = V5, 
                total_receipts = V6, transfers_from_committees = V7, total_disbursements = V8, transfers_to_committees = V9, beginning_cash = V10,
                end_cash = V11, candidate_contributions = V12, candidate_loans = V13, other_loans = V14, candidate_loan_repayments = V15,
                other_loan_repayments = V16, debt = V17, individual_contributions = V18, special = V21, primary = V22, runoff = V23, general = V24, 
                vote_share = V25, committee_contributions = V26, party_contributions = V27, end_date = V28, individual_refunds = V29, 
                committee_refunds = V30) %>%
  mutate(chamber = substring(candidate_id, 1, 1),
         seat_number = pmax(seat_number, 1))

dem_house_fundraising_frac <- fundraising %>%
  filter(general %in% c("W", "L"), special == "" | is.na(special), (party %in% c("DEM", "REP") | state == "Alaska"), chamber == "H") %>%
  dplyr::select(year, state, seat_number, party, individual_contributions) %>%
  group_by(year, state, seat_number) %>%
  mutate(pct_fundraising = individual_contributions / sum(individual_contributions)) %>%
  ungroup() %>%
  filter(party == "DEM") %>%
  dplyr::select(-individual_contributions)

dem_senate_fundraising_frac <- fundraising %>%
  filter(general %in% c("W", "L"), special == "", (party %in% c("DEM", "REP") | state == "Alaska"), chamber == "S") %>%
  dplyr::select(year, state, seat_number, party, individual_contributions) %>%
  group_by(year, state, seat_number) %>%
  mutate(pct_fundraising = individual_contributions / sum(individual_contributions)) %>%
  ungroup() %>%
  filter(party == "DEM") %>%
  dplyr::select(-individual_contributions)
  
# This year
oct_fundraising <- read.csv("data/fec-data/fundraising_2020_oct.csv", header = FALSE) %>%
  mutate(V1 = gsub("ï»¿", "", V1)) %>%
  inner_join(regions %>% dplyr::select(state, abbrev), by = c("V19" = "abbrev")) %>%
  dplyr::select(state, seat_number = V20, candidate_id = V1, candidate_name = V2, candidate_status = V3, party_code = V4, party = V5, 
                total_receipts = V6, transfers_from_committees = V7, total_disbursements = V8, transfers_to_committees = V9, beginning_cash = V10,
                end_cash = V11, candidate_contributions = V12, candidate_loans = V13, other_loans = V14, candidate_loan_repayments = V15,
                other_loan_repayments = V16, debt = V17, individual_contributions = V18, special = V21, primary = V22, runoff = V23, general = V24, 
                vote_share = V25, committee_contributions = V26, party_contributions = V27, end_date = V28, individual_refunds = V29, 
                committee_refunds = V30) %>%
  as.tbl()

dem_house_fundraising_frac_2020 <- read_csv("data/house_candidates.csv") %>%
  left_join(oct_fundraising %>% dplyr::select(candidate_id, individual_contributions), 
            by = c("fec_candidate_id" = "candidate_id")) %>%
  mutate(individual_contributions = ifelse(is.na(individual_contributions), 0, individual_contributions)) %>%
  dplyr::select(-candidate_firstname, -candidate_lastname, -fec_candidate_id) %>%
  spread(candidate_party, individual_contributions) %>%
  mutate(dem_pct_fundraising = DEM / (DEM + REP)) %>%
  dplyr::select(state, seat_number, dem_pct_fundraising)

dem_senate_fundraising_frac_2020 <- read_csv("data/senate_candidates.csv") %>%
  left_join(oct_fundraising %>% dplyr::select(candidate_id, individual_contributions), 
            by = c("fec_candidate_id" = "candidate_id")) %>%
  mutate(individual_contributions = ifelse(is.na(individual_contributions), 0, individual_contributions)) %>%
  dplyr::select(state, seat_name, candidate_party, individual_contributions) %>%
  spread(candidate_party, individual_contributions) %>%
  mutate(dem_pct_fundraising = DEM / (DEM + REP)) %>%
  dplyr::select(state, seat_name, dem_pct_fundraising)

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
  dplyr::select(year, state, seat_number, region, incumbent_running, incumbent_first_elected, democrat_running, republican_running, redistricted, 
                party, pct) %>%
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
  left_join(dem_house_fundraising_frac, by = c("year", "state", "seat_number")) %>%
  mutate(dem_pct_fundraising = ifelse(is.na(pct_fundraising), 0, pct_fundraising),
         multiterm = (incumbent_running != "None") & (year - incumbent_first_elected > 2),
         multiterm_dem = multiterm & (incumbent_running == "DEM"),
         multiterm_rep = multiterm & (incumbent_running == "REP")) %>%
  dplyr::select(year, pres_year, state, seat_number, region, incumbent_running, incumbent_first_elected, multiterm, multiterm_dem, multiterm_rep, 
                incumbency_change, margin, last_margin, natl_margin, last_natl_margin, state_margin, last_state_margin, dem_pct_fundraising) %>%
  na.omit() 

house_results_2party_filtered$incumbency_change <- factor(house_results_2party_filtered$incumbency_change)
house_results_2party_filtered

# Filling in results in uncontested races from 2018
pres_results_by_2020_cd <- house_results_2party %>%
  filter(year == 2018, democrat_running, republican_running) %>%
  left_join(read_csv("data/presidential_results_by_2020_cd.csv"), by = c("state", "seat_number")) %>%
  mutate(margin = DEM - REP,
         pres_2party_2016 = (clinton_2016_pct - trump_2016_pct) / (clinton_2016_pct + trump_2016_pct),
         pres_2party_2012 = (obama_2012_pct - romney_2012_pct) / (obama_2012_pct + romney_2012_pct),
         pres_2party_2008 = (obama_2008_pct - mccain_2008_pct) / (obama_2008_pct + mccain_2008_pct))

contested_2018_lm <- lm(margin ~ pres_2party_2016 + pres_2party_2012, data = pres_results_by_2020_cd)
