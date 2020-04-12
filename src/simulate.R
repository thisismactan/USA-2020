source("src/poll_averages.R")
source("src/state_partisan_priors.R")

set.seed(2020)

n_sims <- 100000

# Simulated national popular vote based on national polling
poll_pcts <- rmvn(n_sims, mu = president_poll_covariance$center, president_poll_covariance$cov)
colnames(poll_pcts) <- c("biden", "trump")
undecided_pct <- 1 - rowSums(poll_pcts)

# Undecided split: Biden's share of undecided voters
biden_undecided_frac <- rbeta(n_sims, 15, 15)
trump_undecided_frac <- 1 - biden_undecided_frac

biden_undecided_pct <- biden_undecided_frac * undecided_pct
trump_undecided_pct <- trump_undecided_frac * undecided_pct

national_popular_vote_sims <- poll_pcts + cbind(biden_undecided_pct, trump_undecided_pct)

national_popular_vote_sims %>%
  as.data.frame() %>%
  mutate(biden_win = biden > trump) %>%
  group_by(biden_win) %>%
  summarise(prob = n() / n_sims)
