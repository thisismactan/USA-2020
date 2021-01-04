source("src/poll_averages.R")
source("src/state_partisan_priors.R")
if(!exists("state_cov")) {
  source("src/state_demographic_similarity.R")
}

set.seed(2020)

n_sims <- 100000

# Simulated national popular vote based on national polling
poll_pcts <- rmvn(n_sims, mu = president_poll_covariance$center, president_poll_covariance$cov + diag(c(0.035^2, 0.035^2)))
colnames(poll_pcts) <- c("biden", "trump")
undecided_pct <- 1 - rowSums(poll_pcts)

# Undecided split: Biden's share of undecided voters
biden_undecided_frac <- rbeta(n_sims, 15, 15)
trump_undecided_frac <- 1 - biden_undecided_frac

biden_undecided_pct <- biden_undecided_frac * undecided_pct
trump_undecided_pct <- trump_undecided_frac * undecided_pct

national_popular_vote_sims <- (poll_pcts + cbind(biden_undecided_pct, trump_undecided_pct)) %>%
  as.data.frame() %>%
  as.tbl() %>%
  mutate(sim_id = 1:n()) %>%
  dplyr::select(sim_id, biden, trump) %>%
  mutate(national_two_party_margin = biden - trump)

national_popular_vote_sims %>%
  as.data.frame() %>%
  mutate(biden_win = biden > trump) %>%
  group_by(biden_win) %>%
  summarise(prob = n() / n_sims)

# Regional deviations
region_names <- unique(regions$region)
regional_deviations <- expand.grid(region = region_names,
                                   sim_id = 1:n_sims) %>%
  as.tbl() %>%
  mutate(region_deviation = rnorm(n(), 0, regional_sd))

# Residual_deviations
state_names <- regions %>%
  pull(state) %>%
  unique()

state_deviations <- expand.grid(state = state_names,
                                sim_id = 1:n_sims) %>%
  as.tbl() %>%
  mutate(state_deviation = rnorm(n(), 0, residual_sd))

# Joining onto national results
total_deviations <- regions %>%
  dplyr::select(state, region, electoral_votes) %>%
  left_join(regional_deviations, by = "region") %>%
  left_join(state_deviations, by = c("state", "sim_id")) %>%
  arrange(sim_id, state) %>%
  na.locf() %>%
  mutate(total_deviation = region_deviation + state_deviation) 

# Use fixed effects coefficients to simulate two-party margins by state
state_priors <- total_deviations %>%
  left_join(national_popular_vote_sims, by = "sim_id") %>%
  left_join(state_two_party_margins_2016, by = "state") %>%
  mutate(two_party_margin_natl_change = national_two_party_margin - two_party_margin_2016,
         prior_predicted_two_party_margin = state_two_party_margin * fixed_effect_coefficients["last_two_party_margin"] +
           two_party_margin_natl_change * fixed_effect_coefficients["two_party_margin_natl_change"] + total_deviation,
         prior_predicted_two_party_margin = pmax(prior_predicted_two_party_margin, -1),
         prior_predicted_two_party_margin = pmin(prior_predicted_two_party_margin, 1),
         biden = 0.5 + prior_predicted_two_party_margin / 2,
         trump = 0.5 - prior_predicted_two_party_margin / 2) %>%
  dplyr::select(sim_id, state, region, electoral_votes, total_deviation, biden, trump, prior_predicted_two_party_margin)

# State prior probabilities
state_prior_probabilities <- state_priors %>%
  mutate(winner = case_when(biden > trump ~ "Biden",
                            trump >= biden ~ "Trump")) %>%
  group_by(state, winner) %>%
  summarise(prob = n() / n_sims) %>%
  spread(winner, prob, fill = 0)

# State prior mean and variance
state_prior_summary_stats <- state_priors %>%
  group_by(state) %>%
  summarise(biden_prior_mean = mean(biden),
            trump_prior_mean = mean(trump),
            biden_prior_variance = var(biden),
            trump_prior_variance = var(trump)) %>%
  mutate(state = case_when(grepl("'s 1st congressional district", state) ~ gsub("'s 1st congressional district", " CD-1", state),
                           grepl("'s 2nd congressional district", state) ~ gsub("'s 2nd congressional district", " CD-2", state),
                           grepl("'s 3rd congressional district", state) ~ gsub("'s 3rd congressional district", " CD-3", state),
                           !grepl("congress", state) ~ state))

# Updating with state-level polling
state_poll_averages_today <- president_averages %>%
  filter(median_date == today(), state != "National")

polling_variation <- state_poll_averages_today %>%
  group_by(state) %>%
  summarise(margin_var = mean(var / eff_n)) %>%
  right_join(state_prior_summary_stats, by = "state") %>%
  mutate(polled = !is.na(margin_var),
         margin_var = replace_na_zero(margin_var)) %>%
  dplyr::select(state, polled, margin_var)

state_cov_adj <- state_cov + diag(polling_variation$margin_var)

while(!is.positive.definite(state_cov_adj)) {
  diag(state_cov_adj) <- diag(state_cov_adj) + 1e-6
}

# Weights for average
poll_weights <- polling_variation %>%
  mutate(poll_weight = polled / diag(state_cov_adj)) %>%
  dplyr::select(state, poll_weight)

prior_weights <- state_prior_summary_stats %>%
  mutate(prior_weight = 2 / (biden_prior_variance + trump_prior_variance)) %>%
  dplyr::select(state, prior_weight)

pres_simulation_weights <- prior_weights %>%
  left_join(poll_weights, by = "state") %>%
  mutate(poll_weight = case_when(is.na(poll_weight) ~ 0,
                                 !is.na(poll_weight) ~ poll_weight),
         weight_sum = prior_weight + poll_weight,
         poll_weight = poll_weight / weight_sum,
         prior_weight = prior_weight / weight_sum,
         state = case_when(grepl("CD-1", state) ~ gsub(" CD-1", "'s 1st congressional district", state),
                           grepl("CD-2", state) ~ gsub(" CD-2", "'s 2nd congressional district", state),
                           grepl("CD-3", state) ~ gsub(" CD-3", "'s 3rd congressional district", state),
                           !grepl("CD-", state) ~ state)) %>%
  dplyr::select(state, prior_weight, poll_weight)

# Simulate state poll distribution draws
state_polling_error_sims <- rmvn(n_sims, mu = rep(0, nrow(state_cov_adj)), sigma = state_cov_adj) %>%
  as.data.frame()
names(state_polling_error_sims) <- colnames(state_cov_adj)

pres_state_sims <- state_polling_error_sims %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "state", value.name = "error") %>%
  left_join(state_poll_averages_today %>% dplyr::select(state, candidate, avg) %>% spread(candidate, avg), by = "state") %>%
  mutate(biden_poll = biden + error / 2,
         trump_poll = trump - error / 2,
         state = case_when(grepl("CD-1", state) ~ gsub(" CD-1", "'s 1st congressional district", state),
                           grepl("CD-2", state) ~ gsub(" CD-2", "'s 2nd congressional district", state),
                           grepl("CD-3", state) ~ gsub(" CD-3", "'s 3rd congressional district", state),
                           !grepl("CD-", state) ~ state)) %>%
  dplyr::select(sim_id, state, biden_poll, trump_poll) %>%
  right_join(state_priors %>% dplyr::select(sim_id, state, electoral_votes, biden_prior = biden, trump_prior = trump), 
            by = c("sim_id", "state")) %>%
  mutate_at(vars(c("biden_poll", "trump_poll")), replace_na_zero) %>%
  left_join(pres_simulation_weights, by = "state") %>%
  left_join(tibble(sim_id = 1:n_sims, biden_undecided_pct = biden_undecided_frac), by = "sim_id") %>%
  mutate(biden = biden_prior * prior_weight + biden_poll * poll_weight,
         trump = trump_prior * prior_weight + trump_poll * poll_weight,
         undecided = 1 - biden - trump,
         biden = biden + biden_undecided_frac * undecided,
         trump = trump + (1 - biden_undecided_frac) * undecided) %>%
  dplyr::select(sim_id, state, electoral_votes, biden, trump) %>%
  as.tbl()

# Probabilities
pres_win_probabilities <- pres_state_sims %>%
  mutate(biden_ev = (biden > 0.5) * electoral_votes) %>%
  group_by(sim_id) %>%
  summarise(biden_ev = sum(biden_ev)) %>%
  mutate(candidate = case_when(biden_ev >= 270 ~ "biden",
                               biden_ev < 270 ~ "trump")) %>%
  group_by(state = "National", candidate) %>%
  summarise(prob = n() / n_sims)

pres_state_probabilities <- pres_state_sims %>%
  mutate(winner = case_when(biden > trump ~ "biden",
                            trump > biden ~ "trump")) %>%
  group_by(state, winner) %>%
  summarise(prob = n() / n_sims) %>%
  spread(winner, prob, fill = 0)

presidential_forecast_probabilities_today <- pres_state_probabilities %>%
  melt(id.vars = "state", variable.name = "candidate", value.name = "prob") %>% 
  bind_rows(pres_win_probabilities) %>%
  arrange(state, candidate) %>%
  mutate(date = today()) %>%
  dplyr::select(date, state, candidate, prob)

# Write this to an output file
if(!("presidential_forecast_probabilities_history.csv" %in% list.files("output"))) {
  write_csv(presidential_forecast_probabilities_today, "output/presidential_forecast_probabilities_history.csv")
}

presidential_forecast_probabilities_history <- read_csv("output/presidential_forecast_probabilities_history.csv") %>%
  bind_rows(presidential_forecast_probabilities_today) %>%
  group_by(date, state, candidate) %>%
  dplyr::slice(n()) %>%
  ungroup()

write_csv(presidential_forecast_probabilities_history, "output/presidential_forecast_probabilities_history.csv")

# Cleanup
rm(list = c("regional_deviations", "state_deviations", "state_polling_error_sims", "total_deviations", "biden_undecided_frac",
            "biden_undecided_pct", "trump_undecided_frac", "trump_undecided_pct", "undecided_pct"))

# For the House ####
pres_generic_ballot_data <- national_president_polls_adj %>%
  dplyr::select(poll_id, question_id, candidate, pct) %>%
  spread(candidate, pct) %>%
  mutate(pres_margin = biden - trump) %>%
  dplyr::select(poll_id, question_id, pres_margin) %>%
  inner_join(generic_ballot_polls_adj %>%
              dplyr::select(poll_id, question_id, loess_weight, candidate, pct) %>%
              spread(candidate, pct) %>%
              mutate(house_margin = dem - rep) %>%
              dplyr::select(poll_id, loess_weight, house_margin),
            by = "poll_id")

pres_generic_ballot_lm <- lm(house_margin ~ pres_margin, data = pres_generic_ballot_data, weight = loess_weight)
generic_ballot_sigma <- sqrt(sum(pres_generic_ballot_lm$residuals^2) / pres_generic_ballot_lm$df.residual)
pres_generic_ballot_r2 <- summary(pres_generic_ballot_lm)$r.squared

# House popular vote sims: predict national House vote from presidential sims and add appropriate noise to ensure correlation
# matches generic ballot
house_two_party_sims <- national_popular_vote_sims %>%
  dplyr::select(sim_id, pres_margin = national_two_party_margin) %>%
  mutate(expected_house_two_party_margin = predict(pres_generic_ballot_lm, newdata = .),
         dev = rnorm(n(), 0, generic_ballot_sigma),
         house_two_party_margin = expected_house_two_party_margin + dev)

simulated_house_r2 <- var(house_two_party_sims$expected_house_two_party_margin) / var(house_two_party_sims$house_two_party_margin)
r2_ratio <- simulated_house_r2 / pres_generic_ballot_r2

house_two_party_sims <- national_popular_vote_sims %>%
  dplyr::select(sim_id, pres_margin = national_two_party_margin) %>%
  mutate(house_two_party_margin = predict(pres_generic_ballot_lm, newdata = .) + rnorm(n(), 0, r2_ratio * generic_ballot_sigma))

# Center and rescale so that mean and standard deviation match two-party generic ballot
generic_ballot_2party_avg <- generic_ballot_averages_adj %>%
  filter(median_date == today())

house_margin_mean <- -diff(generic_ballot_2party_avg$avg)
house_margin_var <- sum(generic_ballot_2party_avg$var) + abs(2 * generic_ballot_poll_covariance$cov[1, 2])
house_margin_eff_n <- mean(generic_ballot_2party_avg$eff_n)
house_margin_scale_ratio <- sqrt(house_margin_var) / sd(house_two_party_sims$house_two_party_margin)

house_two_party_sims <- national_popular_vote_sims %>%
  dplyr::select(sim_id, pres_margin = national_two_party_margin) %>%
  mutate(house_two_party_margin = predict(pres_generic_ballot_lm, newdata = .) + rnorm(n(), 0, r2_ratio * generic_ballot_sigma)) %>%
  mutate(house_margin_rescaled = house_two_party_margin * house_margin_scale_ratio,
         house_margin_rescaled = house_margin_rescaled - mean(house_margin_rescaled) + house_margin_mean + rnorm(n(), 0, 0.06) +
           rnorm(n(), 0, sqrt(house_margin_var / house_margin_eff_n))) %>%
  dplyr::select(sim_id, natl_margin = house_margin_rescaled) 


# Fit House model if it doesn't exist yet
if(!exists("house_lm")) {
  source("src/house_modeling.R")
}

# Shape House dataset
house_2020_data <- read_csv("data/house_candidates.csv", na = character()) %>%
  filter(candidate_firstname != "None") %>%
  dplyr::select(state, seat_number, incumbent_running, candidate_party) %>%
  group_by(state, seat_number) %>%
  mutate(dem_running = any(candidate_party == "DEM") | state == "Alaska",
         rep_running = any(candidate_party == "REP")) %>%
  dplyr::select(-candidate_party) %>%
  distinct() %>%
  left_join(house_results_2party %>%
              filter(year == 2018) %>%
              mutate(margin = DEM - REP) %>%
              dplyr::select(state, seat_number, region, incumbent_running_last = incumbent_running, democrat_running_last = democrat_running, 
                            republican_running_last = republican_running, last_margin = margin),
            by = c("state", "seat_number")) %>%
  mutate(incumbent_running_last = ifelse(grepl("redistricting", incumbent_running_last), "None", incumbent_running_last),
         incumbency_change = paste(incumbent_running_last, incumbent_running, sep = " to "),
         last_natl_margin = national_house_results %>% filter(year == 2018) %>% pull(natl_margin)) %>%
  left_join(read_csv("data/nc_redistricted.csv") %>% dplyr::select(state, seat_number, lean), by = c("state", "seat_number")) %>%
  left_join(read_csv("data/presidential_results_by_2020_cd.csv") %>%
              mutate(pres_2party_2016 = (clinton_2016_pct - trump_2016_pct) / (clinton_2016_pct + trump_2016_pct),
                     pres_2party_2012 = (obama_2012_pct - romney_2012_pct) / (obama_2012_pct + romney_2012_pct)), 
            by = c("state", "seat_number")) %>%
  ungroup() %>%
  mutate(fillin = (state == "North Carolina") | (abs(last_margin) == 1) | (state == "New York" & seat_number == 27) |
           (state == "California" & seat_number == 50),
         last_margin = case_when(fillin ~ predict(contested_2018_lm, newdata = .),
                                 !fillin ~ last_margin),
         incumbency_change = case_when(state == "North Carolina" ~ "None to None",
                                       state != "North Carolina" ~ incumbency_change)) %>%
  ungroup() %>%
  dplyr::select(state, seat_number, region, incumbency_change, dem_running, rep_running, last_margin, last_natl_margin) %>%
  left_join(dem_house_fundraising_frac_2020, by = c("state", "seat_number"))

# Generate district simulations
if(exists("house_district_sims")) {
  rm(house_district_sims)
}
gc()

house_n_sims <- 0.75 * n_sims

## State and regional-level deviations
house_region_deviations <- regions %>%
  dplyr::select(region) %>%
  distinct() %>%
  dplyr::slice(rep(1:n(), each = house_n_sims)) %>%
  mutate(sim_id = rep(1:house_n_sims, 10),
         region_dev = rnorm(n(), 0, region_sd))

house_state_deviations <- regions %>%
  dplyr::select(state) %>%
  dplyr::slice(rep(1:n(), each = house_n_sims)) %>%
  mutate(sim_id = rep(1:house_n_sims, 56),
         state_dev = rnorm(n(), 0, state_sd))

## District-level polling simulations
n_districts_polled <- nrow(district_averages)

district_poll_sims <- district_averages %>%
  dplyr::slice(rep(1:n(), each = house_n_sims)) %>%
  mutate(sim_id = rep(1:house_n_sims, n_districts_polled),
         poll_margin = poll_margin + rnorm(n(), 0, sqrt(poll_var))) %>%
  dplyr::select(-poll_var)

house_district_sims <- house_2020_data %>%
  dplyr::slice(rep(1:n(), each = house_n_sims)) %>%
  mutate(sim_id = rep(1:house_n_sims, 435)) %>%
  left_join(house_two_party_sims, by = "sim_id") %>%
  left_join(house_region_deviations, by = c("region", "sim_id")) %>%
  left_join(house_state_deviations, by = c("state", "sim_id")) %>%
  mutate(sim_margin = predict(house_lm_fundraising, newdata = .) + region_dev + state_dev + rnorm(n(), -0.01, residual_sd + 0.01),
         sim_margin = case_when(!dem_running ~ -1,
                                !rep_running ~ 1,
                                dem_running & rep_running ~ sim_margin)) %>%
  group_by(state, seat_number) %>%
  mutate(prior_variance = var(sim_margin)) %>%
  ungroup() %>%
  mutate(prior_weight = 1 / prior_variance) %>%
  dplyr::select(sim_id, state, seat_number, prior_margin = sim_margin, prior_weight) %>%
  left_join(district_poll_sims, by = c("state", "seat_number", "sim_id")) %>%
  mutate(poll_margin = ifelse(is.na(poll_margin), 0, poll_margin),
         poll_weight = ifelse(is.na(poll_weight), 0, poll_weight),
         prior_weight = ifelse(prior_weight == Inf, 99999, prior_weight),
         margin = (prior_margin * prior_weight + poll_margin * poll_weight) / (prior_weight + poll_weight)) %>%
  dplyr::select(sim_id, state, seat_number, margin)

## Timeline for the House forecast
house_forecast_probability_today <- house_district_sims %>%
  mutate(party = ifelse(margin > 0, "Democrats", "Republicans")) %>%
  group_by(sim_id, party) %>%
  summarise(seats = n()) %>%
  group_by(sim_id) %>%
  filter(seats == max(seats)) %>%
  group_by(party) %>%
  summarise(prob = n() / house_n_sims) %>%
  mutate(date = today()) %>%
  dplyr::select(date, party, prob)

house_states_won <- house_district_sims %>%
  group_by(sim_id, state) %>%
  summarise(frac_dem_seats_won = mean(margin > 0)) %>%
  group_by(sim_id) %>%
  summarise(dem_states_won = sum(frac_dem_seats_won > 0.5),
            rep_states_won = sum(frac_dem_seats_won < 0.5))

pres_sim_results <- pres_state_sims %>%
  mutate(biden_ev = (biden > trump) * electoral_votes,
         trump_ev = (trump >= biden) * electoral_votes) %>%
  group_by(sim_id) %>%
  summarise(biden = sum(biden_ev),
            trump = sum(trump_ev)) %>%
  left_join(house_states_won %>% 
              mutate(contingent_win = case_when(dem_states_won > rep_states_won ~ "biden",
                                                rep_states_won >= dem_states_won ~ "trump")) %>%
              dplyr::select(sim_id, contingent_win),
            by = "sim_id") %>%
  mutate(contingent_win = ifelse(!is.na(contingent_win), contingent_win, "trump"),
         winner = case_when(biden >= 270 ~ "biden",
                            trump >= 270 ~ "trump",
                            biden == 269 & contingent_win == "biden" ~ "biden",
                            trump == 269 & contingent_win == "trump" ~ "trump"))

# Write this to an output file
if(!("house_forecast_probability_history.csv" %in% list.files("output"))) {
  write_csv(house_forecast_probability_today, "output/house_forecast_probability_history.csv")
}

house_forecast_probability_history <- read_csv("output/house_forecast_probability_history.csv") %>%
  bind_rows(house_forecast_probability_today) %>%
  group_by(date, party) %>%
  dplyr::slice(n()) %>%
  ungroup()

write_csv(house_forecast_probability_history, "output/house_forecast_probability_history.csv")

rm(district_poll_sims)
gc()

# Fit Senate model if it doesn't exist yet
if(!exists("senate_lm")) {
  source("src/senate_modeling.R")
}

senate_region_deviations <- expand.grid(region = region_names,
                                        sim_id = 1:n_sims) %>%
  as.tbl() %>%
  mutate(region_deviation = rnorm(n(), 0, senate_region_sd))

senate_2020_prior_sims <- read_csv("data/senate_candidates.csv") %>%
  dplyr::select(state, seat_name, incumbent_running, dem_statewide_elected, rep_statewide_elected) %>%
  mutate(class = case_when(seat_name == "Class I" ~ 1,
                           seat_name == "Class II" ~ 2,
                           seat_name == "Class III" ~ 3)) %>%
  dplyr::distinct() %>%
  left_join(pres_state_sims %>% mutate(state_margin = biden - trump) %>% dplyr::select(sim_id, state, state_margin), by = "state") %>%
  left_join(last_senate_results %>% dplyr::select(state, class, last_margin = margin), by = c("state", "class")) %>%
  left_join(regions %>% dplyr::select(state, region), by = "state") %>%
  left_join(senate_region_deviations, by = c("region", "sim_id")) %>%
  mutate(prior_margin = predict(senate_lm, newdata = .) + region_deviation + rnorm(n(), 0, senate_residual_sd),
         prior_margin = case_when(state == "Arkansas" ~ -1,
                                  state != "Arkansas" ~ prior_margin)) %>%
  group_by(state, seat_name) %>%
  mutate(prior_weight = ifelse(var(prior_margin) > 0, 1 / var(prior_margin), 1)) %>%
  dplyr::select(sim_id, state, seat_name, class, prior_margin, prior_weight)

senate_2020_states_polled <- unique(senate_average_margins$state)

senate_state_cov <- state_cov[senate_2020_states_polled, senate_2020_states_polled]
diag(senate_state_cov) <- diag(senate_state_cov) + senate_average_margins$var

while(!is.positive.definite(senate_state_cov)) {
  diag(senate_state_cov) <- diag(senate_state_cov) + 1e-6
}

senate_poll_errors <- rmvn(n_sims, mu = rep(0, length(senate_2020_states_polled)), sigma = senate_state_cov)
colnames(senate_poll_errors) <- senate_2020_states_polled
senate_poll_errors <- as.data.frame(senate_poll_errors) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "state", value.name = "error") %>%
  as.tbl()

senate_poll_sims <- senate_poll_errors %>%
  left_join(senate_average_margins %>% dplyr::select(state, seat_name, avg), by = "state") %>%
  mutate(poll_margin = avg + error) %>%
  left_join(senate_undecided_pct, by = c("state", "seat_name")) %>%
  mutate(dem_undecided_frac = rbeta(n(), 15, 15),
         poll_margin = poll_margin + undecided * (2 * dem_undecided_frac - 1)) %>%
  group_by(state, seat_name) %>%
  mutate(poll_weight = 1 / var(poll_margin)) %>%
  ungroup() %>%
  dplyr::select(sim_id, state, seat_name, poll_margin, poll_weight)

## Real quick, do the Georgia special election
georgia_primary_undecided_dirichlet_params <- c(5, 1, 2, 5, 3, 5, 1, 1)
georgia_primary_undecided_pct <- rdirichlet(n_sims, alpha = georgia_primary_undecided_dirichlet_params) * georgia_primary_undecided

georgia_primary_sims <- logit_inv(rmvn(n_sims, mu = georgia_primary_average$logit, sigma = georgia_primary_cov))
georgia_primary_sims_decided <- rowSums(georgia_primary_sims)
georgia_primary_sims <- ((1 - georgia_primary_undecided) * georgia_primary_sims / georgia_primary_sims_decided + georgia_primary_undecided_pct) %>%
  as.data.frame() %>%
  as.tbl() %>%
  mutate(sim_id = 1:n())

names(georgia_primary_sims) <- c(georgia_primary_average$candidate, "sim_id")

georgia_runoff_undecided_frac <- tibble(sim_id = 1:n_sims,
                                        firstcand_frac = rbeta(n_sims, 10, 10)) %>%
  mutate(secondcand_frac = 1 - firstcand_frac) %>%
  melt(id.vars = "sim_id", value.name = "undecided_frac") %>%
  arrange(sim_id, variable) %>%
  dplyr::select(undecided_frac) %>%
  as.tbl()

georgia_runoff_sims <- georgia_primary_sims %>%
  melt(id.vars = "sim_id", variable.name = "candidate", value.name = "pct") %>%
  arrange(sim_id, desc(pct)) %>%
  group_by(sim_id) %>%
  dplyr::slice(1:2) %>%
  left_join(georgia_primary_candidates, by = "candidate") %>%
  arrange(sim_id, candidate_party, candidate) %>%
  mutate(majority_win = any(pct > 0.5),
         matchup = glue_collapse(as.character(candidate), sep = " vs. ")) %>%
  ungroup() %>%
  left_join(georgia_runoff_average, by = c("matchup", "candidate")) %>%
  bind_cols(georgia_runoff_undecided_frac) %>%
  mutate(var = ifelse(var < 1e-4, 0.05^2, var) + 0.04^2,
         pct = avg + rnorm(n(), 0, sqrt(var / eff_n))) %>%
  group_by(sim_id) %>%
  mutate(pct = (1 - undecided) * pct / sum(pct) + undecided * undecided_frac) %>%
  ungroup() %>%
  mutate(placeholder_party = rep(c("DEM", "REP"), n_sims)) %>%
  group_by(sim_id, majority_win, matchup, candidate_party) %>%
  summarise(pct = sum(pct)) %>%
  mutate(pct = ifelse(is.na(pct), 1, pct)) %>%
  spread(candidate_party, pct, fill = 0) %>%
  mutate(runoff_margin = DEM - REP,
         party = ifelse(runoff_margin > 0, "Democrats", "Republicans"))

senate_state_sims <- senate_2020_prior_sims %>%
  left_join(senate_poll_sims, by = c("sim_id", "state", "seat_name")) %>%
  mutate_at(vars(c("poll_margin", "poll_weight")), replace_na_zero) %>%
  ungroup() %>%
  mutate(margin = (prior_margin * prior_weight + poll_margin * poll_weight) / (prior_weight + poll_weight),
         party = ifelse(margin > 0, "Democrats", "Republicans")) %>%
  dplyr::select(sim_id, state, seat_name, class, margin, party)

georgia_special_sim_rows <- which(senate_state_sims$state == "Georgia" & senate_state_sims$class == 3)
senate_state_sims$margin[georgia_special_sim_rows] <- georgia_runoff_sims$runoff_margin
senate_state_sims$party[georgia_special_sim_rows] <- georgia_runoff_sims$party

## Timeline for the Senate forecast
current_dem_senate_seats <- 35
current_rep_senate_seats <- 30

senate_seat_distribution <- senate_state_sims %>%
  group_by(sim_id, party) %>%
  summarise(seats_won = n()) %>%
  ungroup() %>%
  mutate(seats_held = case_when(party == "Democrats" ~ seats_won + current_dem_senate_seats,
                                party == "Republicans" ~ seats_won + current_rep_senate_seats)) %>%
  left_join(pres_sim_results %>% dplyr::select(sim_id, pres_winner = winner), by = "sim_id") 

senate_majority_winners <- senate_seat_distribution %>%
  filter(party == "Democrats") %>%
  mutate(majority = case_when(seats_held > 50 ~ "Democrats",
                              seats_held < 50 ~ "Republicans",
                              seats_held == 50 & pres_winner == "biden" ~ "Democrats",
                              seats_held == 50 & pres_winner == "trump" ~ "Republicans"))

senate_majority_probability_today <- senate_seat_distribution %>%
  left_join(senate_majority_winners %>% dplyr::select(sim_id, majority), by = "sim_id") %>%
  group_by(date = today(), state = "National", party) %>%
  summarise(prob = mean(party == majority)) %>%
  ungroup()

senate_state_probabilities <- senate_state_sims %>%
  group_by(state, seat_name) %>%
  summarise(Democrats = mean(party == "Democrats")) %>%
  mutate(Republicans = 1 - Democrats) %>%
  melt(id.vars = c("state", "seat_name"), variable.name = "party", value.name = "prob") %>%
  mutate(date = today()) %>%
  arrange(state, seat_name, party) %>%
  dplyr::select(date, state, seat_name, party, prob)

senate_forecast_probabilities_today <- bind_rows(senate_state_probabilities, senate_majority_probability_today) %>%
  arrange(state, seat_name, party)

# Write this to an output file
if(!("senate_forecast_probability_history.csv" %in% list.files("output"))) {
  write_csv(senate_forecast_probabilities_today, "output/senate_forecast_probability_history.csv")
}

senate_forecast_probability_history <- read_csv("output/senate_forecast_probability_history.csv") %>%
  bind_rows(senate_forecast_probabilities_today) %>%
  group_by(date, state, seat_name, party) %>%
  dplyr::slice(n()) %>%
  ungroup()

write_csv(senate_forecast_probability_history, "output/senate_forecast_probability_history.csv")

rm(senate_poll_sims)
gc()
