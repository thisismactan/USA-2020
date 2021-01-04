source("src/simulate.R")
  
## President ####
# State priors ####
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
  geom_vline(aes(xintercept = 270)) +
  geom_vline(data = state_prior_summary_stats, aes(xintercept = avg, col = Candidate), size = 1) +
  geom_histogram(alpha = 0.7, binwidth = 1) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(name = "Candidate", values = candidate_colors, labels = candidate_fullnames) +
  scale_colour_manual(name = "Candidate", values = candidate_colors, labels = candidate_fullnames) +
  labs(title = "2020 United States presidential election forecast", subtitle = "National polls only",
       x = "Electoral votes", y = "Probability", caption = "270 electoral votes needed to win")

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

prior_pop_ev_crosstab

# Actual forecast ####
pres_summary_stats <- pres_sim_results %>%
  melt(id.vars = c("sim_id", "contingent_win", "winner"), variable.name = "Candidate", value.name = "ev") %>%
  group_by(Candidate = as.character(Candidate)) %>%
  summarise(win_prob = mean((ev >= 270) | (ev == 269 & contingent_win == Candidate)),
            pct05 = quantile(ev, 0.05),
            avg = mean(ev),
            pct95 = quantile(ev, 0.95))

pres_summary_stats

pres_state_summary_stats <- pres_state_sims %>%
  mutate(biden_margin = biden - trump) %>%
  dplyr::select(-biden, -trump) %>%
  group_by(State = state, EVs = electoral_votes) %>%
  summarise(biden_prob = mean(biden_margin > 0),
            pct05 = quantile(biden_margin, 0.05),
            pct50 = mean(biden_margin),
            pct95 = quantile(biden_margin, 0.95))

pres_state_summary_stats %>%
  print(n = Inf)

## Histogram that shit
pres_state_sims %>%
  mutate(biden_ev = (biden > 0.5) * electoral_votes) %>%
  group_by(sim_id) %>%
  summarise(biden = sum(biden_ev)) %>%
  mutate(trump = 538 - biden) %>%
  melt(id.vars = "sim_id", variable.name = "Candidate", value.name = "ev") %>%
  ggplot(aes(x = ev, y = ..density.., fill = Candidate)) +
  facet_wrap(~str_to_title(Candidate), nrow = 2) +
  geom_vline(aes(xintercept = 270)) +
  geom_vline(data = pres_summary_stats, aes(xintercept = avg, col = Candidate), size = 1) +
  geom_histogram(alpha = 0.7, binwidth = 1) +
  scale_x_continuous(breaks = seq(from = 0, to = 500, by = 50)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(name = "Candidate", values = candidate_colors, labels = candidate_fullnames) +
  scale_colour_manual(name = "Candidate", values = candidate_colors, labels = candidate_fullnames) +
  labs(title = "2020 United States presidential election forecast",  x = "Electoral votes", y = "Probability", 
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())), 
       caption = "270 electoral votes needed to win")

## Probability Biden wins
biden_win_pres_prob <- (pres_state_sims %>%
    mutate(biden_ev = (biden > trump) * electoral_votes) %>%
    group_by(sim_id) %>%
    summarise(biden_ev = sum(biden_ev)) %>%
    filter(biden_ev >= 270) %>%
    nrow()) / n_sims

### By state
biden_win_prob_by_state <- pres_state_sims %>%
  group_by(state) %>%
  summarise(biden_win_prob = mean(biden > trump))

## Popular vote/Electoral College winner crosstab
pop_ev_crosstab <- pres_state_sims %>%
  mutate(biden_ev = (biden > 0.5) * electoral_votes) %>%
  group_by(sim_id) %>%
  summarise(biden_ev = sum(biden_ev)) %>%
  left_join(national_popular_vote_sims %>% dplyr::select(sim_id, national_two_party_margin), by = "sim_id") %>%
  mutate(popular_vote_winner = ifelse(national_two_party_margin >= 0, "Biden", "Trump"),
         electoral_college_winner = ifelse(biden_ev >= 270, "Biden", "Trump")) %>%
  group_by(popular_vote_winner, electoral_college_winner) %>%
  summarise(prob = n() / n_sims)

pop_ev_crosstab

## Bellwetheriness
pres_conditional_distribution <- pres_state_sims %>%
  mutate(biden_won_state = biden > trump,
         trump_won_state = trump >= biden) %>%
  group_by(sim_id) %>%
  mutate(biden_total_ev = sum(biden_won_state * electoral_votes),
         trump_total_ev = sum(trump_won_state * electoral_votes),
         biden_won_pres = biden_total_ev >= 270,
         trump_won_pres = trump_total_ev >= 270)

biden_bellwetherogram <- pres_state_sims %>%
  mutate(biden_won_state = biden > trump) %>%
  group_by(sim_id) %>%
  mutate(biden_total_ev = sum(biden_won_state * electoral_votes),
         biden_won_pres = biden_total_ev >= 270) %>%
  filter(biden_won_state) %>%
  group_by(state) %>%
  summarise(biden_cond_prob = sum(biden_won_pres) / n())

trump_bellwetherogram <- pres_conditional_distribution %>%
  filter(trump_won_state) %>%
  group_by(state) %>%
  summarise(trump_cond_prob = sum(trump_won_pres) / n())

bellwetherogram <- biden_bellwetherogram %>%
  full_join(trump_bellwetherogram, by = "state") %>%
  left_join(biden_win_prob_by_state, by = "state") %>%
  mutate(BPI = (biden_cond_prob - biden_win_pres_prob) * (trump_cond_prob - (1 - biden_win_pres_prob))) %>%
  left_join(regions %>% dplyr::select(state, abbrev), by = "state") %>%
  dplyr::select(state, abbrev, biden_cond_prob, trump_cond_prob, biden_win_prob, BPI) %>%
  arrange(desc(BPI))

bellwetherogram %>%
  arrange(desc(BPI)) %>%
  print(n = Inf)

# Plotting the bellwetherogram
bellwetherogram %>% 
  ggplot(aes(x = biden_cond_prob - biden_win_pres_prob, y = trump_cond_prob - (1 - biden_win_pres_prob), col = biden_win_prob)) +
  geom_text(aes(label = abbrev), size = 3) +
  scale_colour_gradient2(name = "P(Biden wins state)", low = "red", mid = "#880088", high = "blue", midpoint = 0.5,
                         labels = scales::percent) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Bellwether-o-gram", x = "P(Biden wins presidency | Biden wins state) - P(Biden wins presidency)",
       y = "P(Trump wins presidency | Trump wins state) - P(Trump wins presidency)",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())))

# Tipping points
pres_tipping_points <- pres_state_sims %>%
  mutate(margin = biden - trump) %>%
  arrange(sim_id, desc(margin)) %>%
  group_by(sim_id) %>%
  mutate(cum_ev = cumsum(electoral_votes)) %>%
  filter(cum_ev >= 270) %>%
  dplyr::slice(1) %>%
  group_by(state) %>%
  summarise(tipping_point_prob = n() / nrow(.))

pres_tipping_points %>%
  arrange(desc(tipping_point_prob)) %>%
  dplyr::slice(1:15) %>%
  ggplot(aes(x = reorder(state, tipping_point_prob))) +
  geom_col(aes(y = tipping_point_prob)) +
  geom_text(aes(y = tipping_point_prob + 0.01, label = scales::percent(tipping_point_prob, accuracy = 0.1)), size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Tipping point state probabilities", x = "", y = "Probability of being the tipping point state") +
  coord_flip()

# Forecast over time
comp_states <- c("Arizona", "Colorado", "Florida", "Georgia", "Iowa", "Maine", "Michigan", "Minnesota", "Nebraska's 2nd congressional district",
                 "Nevada", "New Hampshire", "New Mexico", "North Carolina", "Ohio", "Pennsylvania", "Texas", "Virginia", "Wisconsin")

swing_state_pres_forecast_history <- presidential_forecast_probabilities_history %>%
  filter(state %in% comp_states)

# National
presidential_forecast_probabilities_history %>%
  filter(state == "National") %>%
  ggplot(aes(x = date, y = prob, col = candidate)) +
  geom_line(size = 1) +
  geom_vline(xintercept = as.Date("2020-11-03")) +
  geom_text(data = pres_summary_stats %>% mutate(date = today() + 6) %>% dplyr::select(candidate = Candidate, everything()), 
            aes(x = date, y = win_prob, label = scales::percent(win_prob, accuracy = 1)), size = 3) +
  scale_colour_manual(name = "Candidate", values = candidate_colors, labels = candidate_fullnames) +
  scale_x_date(limits = as.Date(c("2020-04-17", "2020-11-04")), breaks = date_breaks("months"), labels = date_format("%b %Y")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  theme(legend.position = "bottom") +
  labs(title = "StatSheet 2020 presidential election forecast over time", x = "Date", y = "Probability of winning",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today()))) 

# Competitive states
ggplot(swing_state_pres_forecast_history, aes(x = date, y = prob, col = candidate)) +
  facet_wrap(~state, nrow = 3) +
  geom_line(data = presidential_forecast_probabilities_history %>% filter(state == "National") %>% dplyr::select(date, candidate, prob),
            alpha = 1/5, size = 1) +
  geom_line(size = 1) +
  geom_vline(xintercept = as.Date("2020-11-03")) +
  geom_text(data = pres_state_summary_stats %>% mutate(date = today() + 10, trump = 1 - biden_prob) %>% 
              filter(State %in% comp_states) %>%
              dplyr::select(state = State, date, biden = biden_prob, trump) %>% 
              melt(id.vars = c("state", "date"), variable.name = "candidate", value.name = "prob"), 
            aes(x = date, y = prob, label = scales::percent(prob, accuracy = 1)), size = 3, show.legend = FALSE) +
  scale_colour_manual(name = "Candidate", values = candidate_colors, labels = candidate_fullnames) +
  scale_x_date(limits = as.Date(c("2020-04-17", "2020-11-04")), breaks = date_breaks("months"), labels = date_format("%b %Y")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "StatSheet 2020 presidential election forecast over time by state", x = "Date", y = "Probability of winning",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())),
       caption = "States and districts decided by single digits in 2016\nNational forecast shown in lighter colors") 

## ####
## House
house_candidates_2020 <- read_csv("data/house_candidates.csv")

# Probabilities by district
district_summary_stats <- house_district_sims %>%
  group_by(state, seat_number) %>%
  summarise(dem_prob = mean(margin > 0),
            pct05 = quantile(margin, 0.05),
            avg = mean(margin),
            pct95 = quantile(margin, 0.95))

district_summary_stats %>%
  print(n = Inf)

# Distribution of seat totals
house_seat_distribution <- house_district_sims %>%
  group_by(sim_id) %>% 
  summarise(Democrats = sum(margin > 0),
            Republicans = sum(margin <= 0)) %>%
  melt(id.vars = "sim_id", variable.name = "Party", value.name = "seats") %>%
  as.tbl()

house_summary_stats <- house_seat_distribution %>% 
  group_by(Party) %>%
  summarise(majority_prob = mean(seats >= 218),
            pct05 = quantile(seats, 0.05),
            avg = mean(seats),
            pct95 = quantile(seats, 0.95))

house_summary_stats

# Histogram
house_seat_distribution %>%
  ggplot(aes(x = seats, y = ..density.., fill = Party)) +
  facet_wrap(~Party, nrow = 2) +
  geom_vline(xintercept = 218, size = 1, col = "black") +
  geom_vline(data = house_summary_stats, aes(xintercept = avg, col = Party)) +
  geom_histogram(binwidth = 1, alpha = 0.7) +
  scale_fill_manual(name = "Party", values = c("Democrats" = "blue", "Republicans" = "red")) +
  scale_colour_manual(name = "Party", values = c("Democrats" = "blue", "Republicans" = "red")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "2020 House of Representatives elections forecast", x = "Seats", y = "Probability",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())),
       caption = "218 seats needed for a majority")

# Forecast probabilities over time
house_forecast_probability_history %>%
  ggplot(aes(x = date, y = prob, col = party)) +
  geom_line(size = 1) +
  geom_vline(xintercept = as.Date("2020-11-03")) +
  scale_colour_manual(name = "Party", values = c("Democrats" = "blue", "Republicans" = "red")) +
  scale_x_date(limits = as.Date(c("2020-04-17", "2020-11-04")), breaks = date_breaks("months"), labels = date_format("%b %Y")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "StatSheet 2020 House forecast over time", x = "Date", y = "Probability of majority",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today()))) 

# House bellwetherogram
house_conditional_probs <- house_district_sims %>%
  left_join(house_seat_distribution %>% filter(Party == "Democrats"), by = "sim_id") %>%
  mutate(house_majority = ifelse(seats >= 218, "Democrats", "Republicans"),
         seat_winner = ifelse(margin > 0, "Democrats", "Republicans")) %>%
  group_by(state, seat_number, seat_winner) %>%
  summarise(cond_prob = mean(house_majority == seat_winner)) %>%
  spread(seat_winner, cond_prob) %>%
  mutate(dem_prob_increase = Democrats - house_forecast_probability_today$prob[1],
         rep_prob_increase = Republicans - house_forecast_probability_today$prob[2])

house_bellwetherogram <- house_conditional_probs %>%
  left_join(house_candidates_2020 %>% dplyr::select(state, seat_number, district_abbr) %>% distinct(), by = c("state", "seat_number")) %>%
  left_join(district_summary_stats %>% dplyr::select(state, seat_number, dem_prob), by = c("state", "seat_number")) %>%
  mutate(dem_logit = logit(dem_prob))

house_bellwetherogram %>%
  na.omit() %>%
  filter(abs(dem_logit) < logit(0.9)) %>%
  ggplot(aes(x = dem_prob_increase, y = rep_prob_increase, col = dem_prob)) +
  geom_text(aes(label = district_abbr), size = 2) +
  scale_colour_gradient2(name = "P(D wins district)", low = "red", mid = "#880088", high = "blue", midpoint = 0.5,
                         labels = scales::percent) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "House bellwether-o-gram", subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())),
       x = "P(Democratic majority | Democrat wins district) - P(Democratic majority)",
       y = "P(Republican majority | Republican wins district) - P(Republican wins district)")

# Senate ####
senate_summary_stats <- senate_seat_distribution %>%
  filter(sim_id %in% condition_sim_ids) %>%
  left_join(senate_majority_winners %>% dplyr::select(sim_id, majority), by = "sim_id") %>%
  group_by(party) %>%
  summarise(majority_prob = mean(party == majority),
            pct05 = quantile(seats_held, 0.05),
            avg = mean(seats_held),
            pct95 = quantile(seats_held, 0.95))

senate_summary_stats  

# Summary stats by state
senate_state_summary_stats <- senate_state_sims %>%
  filter(sim_id %in% condition_sim_ids) %>%
  group_by(state, seat_name) %>%
  summarise(dem_prob = mean(party == "Democrats"),
            pct05 = quantile(margin, 0.05),
            avg = mean(margin),
            pct95 = quantile(margin, 0.95))

senate_state_summary_stats %>%
  print(n = Inf)

# Histogram (a bit more complicated this time)
senate_seat_distribution %>%
  left_join(senate_majority_winners %>% dplyr::select(sim_id, majority), by = "sim_id") %>%
  filter(party == "Democrats") %>% 
  group_by(majority, seats_held) %>% 
  summarise(prob = n() / n_sims) %>% 
  ungroup() %>%
  ggplot(aes(x = seats_held, y = prob, fill = majority)) +
  geom_col(alpha = 0.7) +
  geom_vline(data = senate_summary_stats, aes(xintercept = avg, col = party), size = 1) + 
  scale_x_continuous(breaks = seq(from = 42, to = 58, by = 2), limits = c(42, 58)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(name = "Majority party", values = c("Democrats" = "blue", "Republicans" = "red")) +
  scale_colour_manual(name = "Majority party", values = c("Democrats" = "blue", "Republicans" = "red")) +
  labs(title = "2020 Senate elections forecast", x = "Democratic seats held after election", y = "Probability",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())),
       caption = "51 seats or 50 seats + vice presidency needed for majority") 

# Bellwetherogram
senate_conditional_probs <- senate_state_sims %>%
  left_join(senate_state_probabilities %>% filter(party == "Democrats") %>% dplyr::select(state, seat_name, dem_prob = prob), 
            by = c("state", "seat_name")) %>%
  left_join(senate_majority_winners %>% dplyr::select(sim_id, majority), by = "sim_id") %>%
  mutate(state_winner = ifelse(margin > 0, "Democrats", "Republicans")) %>%
  group_by(state, seat_name, dem_prob, state_winner) %>%
  summarise(cond_prob = mean(majority == state_winner)) %>%
  spread(state_winner, cond_prob) %>%
  mutate(dem_prob_increase = Democrats - senate_summary_stats$majority_prob[1],
         rep_prob_increase = Republicans - senate_summary_stats$majority_prob[2])

senate_conditional_probs %>% 
  mutate(BPI = dem_prob_increase * rep_prob_increase) %>%
  arrange(desc(BPI)) %>%
  filter(dem_prob >= 0.05, dem_prob <= 0.95) %>%
  print(n = Inf)

senate_conditional_probs %>%
  filter(dem_prob <= 0.95, dem_prob >= 0.05) %>%
  left_join(regions %>% dplyr::select(state, abbrev), by = "state") %>%
  mutate(abbrev = case_when(state == "Georgia" & seat_name == "Class II" ~ "GA-2",
                            state == "Georgia" & seat_name == "Class III" ~ "GA-3",
                            state != "Georgia" ~ abbrev)) %>%
  ggplot(aes(x = dem_prob_increase, y = rep_prob_increase, col = dem_prob)) +
  geom_text(aes(label = abbrev), size = 3)  +
  scale_colour_gradient2(name = "P(Democratic win)", low = "red", mid = "#880088", high = "blue", midpoint = 0.5,
                         labels = scales::percent) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Senate bellwether-o-gram", x = "P(Democratic majority | Democrat wins state) - P(Democratic majority)",
       y = "P(Republican majority | Republican wins state) - P(Republican majority)",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())))
  
# Ant farm
senate_comp_states <- c("Alabama", "Alaska", "Arizona", "Colorado", "Georgia", "Iowa", "Kansas", "Maine", "Michigan",
                        "Montana", "New Hampshire", "North Carolina", "South Carolina", "Texas", "Virginia")

senate_forecast_probability_history %>%
  filter(state %in% senate_comp_states) %>%
  mutate(state = case_when(seat_name != "Class II" ~ paste(state, "(special)"),
                           seat_name == "Class II" ~ state),
         election_date = ifelse(state == "Georgia (special)", as.Date("2021-01-05"), as.Date("2020-11-03"))) %>%
  ggplot(aes(x = date, y = prob, col = party)) +
  facet_wrap(~state, nrow = 4) +
  geom_vline(aes(xintercept = election_date)) +
  geom_line(data = senate_forecast_probability_history %>% filter(state == "National") %>% dplyr::select(date, party, natl_prob = prob),
            aes(y = natl_prob), alpha = 1/5, size = 1) +
  geom_line(size = 1) +
  scale_colour_manual(name = "Candidate", values = c("blue", "red"), labels = c("Democrat", "Republican")) +
  scale_x_date(limits = as.Date(c("2020-05-23", "2021-01-06")), breaks = date_breaks("months"), labels = date_format("%b %Y")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "StatSheet 2020 Senate election forecast over time by state", x = "Date", y = "Probability of winning",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())),
       caption = "National forecast shown in lighter colors") 

# President, House, and Senate results
pres_sim_results %>%
  filter(sim_id <= house_n_sims) %>%
  left_join(house_seat_distribution %>% filter(Party == "Democrats") %>% mutate(house_maj = seats >= 218) %>% dplyr::select(sim_id, house_maj), 
            by = "sim_id") %>%
  left_join(senate_majority_winners %>% dplyr::select(sim_id, senate_maj = majority), by = "sim_id") %>%
  mutate(President = str_to_title(winner),
         House = ifelse(house_maj, "Democratic", "Republican"),
         Senate = ifelse(senate_maj == "Democrats", "Democratic", "Republican")) %>%
  group_by(President, House, Senate) %>%
  summarise(Probability = n() / house_n_sims) %>%
  as.data.frame() %>%
  mutate(Probability = scales::percent(Probability, accuracy = 1))
