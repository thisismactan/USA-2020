source("src/simulate.R")

#### President ####
# State priors
## Total electoral votes
state_prior_summary_stats <- state_priors %>%
  mutate(biden_ev = (biden > trump) * electoral_votes,
         trump_ev = (trump >= biden) * electoral_votes) %>%
  group_by(sim_id) %>%
  summarise(biden = sum(biden_ev),
            trump = sum(trump_ev)) %>%
  melt(id.vars = "sim_id", variable.name = "Candidate", value.name = "ev") %>%
  group_by(Candidate) %>%
  summarise(pct05 = quantile(ev, 0.05),
            pct25 = quantile(ev, 0.25),
            avg = mean(ev),
            pct75 = quantile(ev, 0.75),
            pct95 = quantile(ev, 0.95))

## Histogram that shit
state_priors %>%
  mutate(biden_ev = (biden > 0.5) * electoral_votes) %>%
  group_by(sim_id) %>%
  summarise(biden = sum(biden_ev)) %>%
  mutate(trump = 538 - biden) %>%
  melt(id.vars = "sim_id", variable.name = "Candidate", value.name = "ev") %>%
  ggplot(aes(x = ev, y = ..density.., fill = Candidate)) +
  facet_wrap(~str_to_title(Candidate), nrow = 2) +
  geom_vline(data = state_prior_summary_stats, aes(xintercept = avg, col = Candidate), size = 1) +
  geom_histogram(alpha = 0.7, binwidth = 1) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(name = "Candidate", values = candidate_colors, labels = candidate_fullnames) +
  scale_colour_manual(name = "Candidate", values = candidate_colors, labels = candidate_fullnames) +
  labs(title = "2020 United States presidential election forecast", subtitle = "National polls only",
       x = "Electoral votes", y = "Probability")

## Probability Biden wins
(state_priors %>%
    mutate(biden_ev = (biden > 0.5) * electoral_votes) %>%
    group_by(sim_id) %>%
    summarise(biden_ev = sum(biden_ev)) %>%
    filter(biden_ev >= 270) %>%
    nrow()) / n_sims

## Popular vote/Electoral College winner crosstab
prior_pop_ev_crosstab <- state_priors %>%
  mutate(biden_ev = (biden > 0.5) * electoral_votes) %>%
  group_by(sim_id) %>%
  summarise(biden_ev = sum(biden_ev)) %>%
  left_join(national_popular_vote_sims %>% dplyr::select(sim_id, national_two_party_margin), by = "sim_id") %>%
  mutate(popular_vote_winner = ifelse(national_two_party_margin >= 0, "Biden", "Trump"),
         electoral_college_winner = ifelse(biden_ev >= 270, "Biden", "Trump")) %>%
  group_by(popular_vote_winner, electoral_college_winner) %>%
  summarise(prob = n() / n_sims)

pop_ev_crosstab

# Actual forecast
pres_state_sims %>%
  mutate(biden_ev = (biden > trump) * electoral_votes,
         trump_ev = (trump >= biden) * electoral_votes) %>%
  group_by(sim_id) %>%
  summarise(biden = sum(biden_ev),
            trump = sum(trump_ev)) %>%
  melt(id.vars = "sim_id", variable.name = "Candidate", value.name = "ev") %>%
  group_by(Candidate) %>%
  summarise(pct05 = quantile(ev, 0.05),
            pct25 = quantile(ev, 0.25),
            avg = mean(ev),
            pct75 = quantile(ev, 0.75),
            pct95 = quantile(ev, 0.95))

pres_state_sims

## Histogram that shit
pres_state_sims %>%
  mutate(biden_ev = (biden > 0.5) * electoral_votes) %>%
  group_by(sim_id) %>%
  summarise(biden = sum(biden_ev)) %>%
  mutate(trump = 538 - biden) %>%
  melt(id.vars = "sim_id", variable.name = "Candidate", value.name = "ev") %>%
  ggplot(aes(x = ev, y = ..density.., fill = Candidate)) +
  facet_wrap(~str_to_title(Candidate), nrow = 2) +
  geom_vline(data = state_prior_summary_stats, aes(xintercept = avg, col = Candidate), size = 1) +
  geom_histogram(alpha = 0.7, binwidth = 1) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(name = "Candidate", values = candidate_colors, labels = candidate_fullnames) +
  scale_colour_manual(name = "Candidate", values = candidate_colors, labels = candidate_fullnames) +
  labs(title = "2020 United States presidential election forecast", subtitle = "National polls only",
       x = "Electoral votes", y = "Probability")

## Probability Biden wins
(state_priors %>%
    mutate(biden_ev = (biden > 0.5) * electoral_votes) %>%
    group_by(sim_id) %>%
    summarise(biden_ev = sum(biden_ev)) %>%
    filter(biden_ev >= 270) %>%
    nrow()) / n_sims

## Popular vote/Electoral College winner crosstab
prior_pop_ev_crosstab <- state_priors %>%
  mutate(biden_ev = (biden > 0.5) * electoral_votes) %>%
  group_by(sim_id) %>%
  summarise(biden_ev = sum(biden_ev)) %>%
  left_join(national_popular_vote_sims %>% dplyr::select(sim_id, national_two_party_margin), by = "sim_id") %>%
  mutate(popular_vote_winner = ifelse(national_two_party_margin >= 0, "Biden", "Trump"),
         electoral_college_winner = ifelse(biden_ev >= 270, "Biden", "Trump")) %>%
  group_by(popular_vote_winner, electoral_college_winner) %>%
  summarise(prob = n() / n_sims)

pop_ev_crosstab