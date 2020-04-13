source("src/poll_averages.R")
source("src/state_partisan_priors.R")

set.seed(2020)

n_sims <- 100000

# Simulated national popular vote based on national polling
poll_pcts <- rmvn(n_sims, mu = president_poll_covariance$center, president_poll_covariance$cov + diag(c(0.03^2, 0.03^2)))
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
  filter(!grepl("congressional", state)) %>%
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
           two_party_margin_natl_change * fixed_effect_coefficients["last_two_party_margin"] + total_deviation,
         prior_predicted_two_party_margin = pmax(prior_predicted_two_party_margin, -1),
         prior_predicted_two_party_margin = pmin(prior_predicted_two_party_margin, 1),
         biden = 0.5 + prior_predicted_two_party_margin / 2,
         trump = 0.5 - prior_predicted_two_party_margin / 2) %>%
  dplyr::select(sim_id, state, region, electoral_votes, total_deviation, biden, trump, prior_predicted_two_party_margin)

# State prior probabilities
state_priors %>%
  mutate(winner = case_when(biden > trump ~ "Biden",
                            trump > biden ~ "Trump")) %>%
  group_by(state, winner) %>%
  summarise(prob = n() / n_sims) %>%
  spread(winner, prob, fill = 0) %>%
  View()

# Electoral votes
state_priors %>%
  mutate(biden_ev = (biden > 0.5) * electoral_votes) %>%
  group_by(sim_id) %>%
  summarise(biden_ev = sum(biden_ev)) %>%
  summarise(pct05 = quantile(biden_ev, 0.05),
            pct25 = quantile(biden_ev, 0.25),
            avg = mean(biden_ev),
            pct75 = quantile(biden_ev, 0.75),
            pct95 = quantile(biden_ev, 0.95))

# Probability Biden wins
(state_priors %>%
  mutate(biden_ev = (biden > 0.5) * electoral_votes) %>%
  group_by(sim_id) %>%
  summarise(biden_ev = sum(biden_ev)) %>%
  filter(biden_ev >= 270) %>%
  nrow()) / n_sims
