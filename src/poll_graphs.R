source("src/poll_averages.R")

# President ####
graph_states <- c("Iowa")

# Setting limits for the graphs
graph_state_polls <- president_polls %>% 
  filter(state %in% graph_states, candidate %in% c("biden", "trump", "amash"))

max_pct <- max(graph_state_polls$pct)
min_pct <- min(graph_state_polls$pct)

# Current average
current_poll_average <- president_averages %>%
  filter(median_date == today(), state %in% graph_states)

current_poll_average %>%
  ggplot(aes(x = candidate, y = avg, fill = candidate)) +
  geom_col() +
  geom_errorbar(aes(ymin = avg - 1.645 * sqrt(var / eff_n), ymax = avg + 1.645 * sqrt(var / eff_n)), col = "#666666") +
  geom_text(aes(y = avg + 0.02, label = scales::percent(avg, accuracy = 0.1)), size = 4) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(name = "Candidate", values = candidate_colors, labels = candidate_fullnames) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(title = paste0(graph_states, " presidential polling average"), x = "Candidate", y = "Average %",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())),
       caption = "Error bars indicate 90% confidence intervals")

# Polls over time
president_averages_smoothed %>%
  filter(state %in% graph_states, candidate %in% c("biden", "trump", "amash")) %>%
  ggplot(aes(x = median_date, y = avg, col = candidate, fill = candidate)) +
  geom_vline(xintercept = as.Date("2020-11-03")) +
  geom_ribbon(aes(ymin = avg - 1.645 * sqrt(var / eff_n), ymax = avg + 1.645 * sqrt(var / eff_n)), alpha = 0.2, col = NA) +
  geom_point(data = graph_state_polls, 
             aes(y = pct), alpha = 0.5, size = 1) +
  geom_line(size = 1) +
  scale_colour_manual(name = "Candidate", values = candidate_colors, labels = candidate_fullnames) +
  scale_fill_manual(name = "Candidate", values = candidate_colors, labels = candidate_fullnames) +
  scale_y_continuous(labels = scales::percent, limits = c(min_pct - 0.1, max_pct + 0.1)) +
  scale_x_date(date_labels = "%b %Y", limits = as.Date(c("2020-01-01", "2020-11-03")), breaks = date_breaks("2 months")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = -0.01)) +
  labs(title = paste0(graph_states, " presidential polling"), x = "Date", y = "%",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())),
       caption = "Averages smoothed over past five days")


# Generic ballot ####
current_poll_average <- generic_ballot_averages_adj %>%
  filter(median_date == today())

current_poll_average %>%
  ggplot(aes(x = candidate, y = avg, fill = candidate)) +
  geom_col() +
  geom_errorbar(aes(ymin = avg - 1.645 * sqrt(var / eff_n), ymax = avg + 1.645 * sqrt(var / eff_n)), col = "#666666") +
  geom_text(aes(y = avg + 0.01, label = scales::percent(avg, accuracy = 0.1)), size = 4) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(name = "Party", values = party_colors, labels = party_names) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(title = "Generic House ballot polling average", x = "Party", y = "Average %",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())),
       caption = "Error bars indicate 90% confidence intervals")

# Polls over time
generic_ballot_averages_smoothed %>%
  ggplot(aes(x = median_date, y = avg, col = candidate, fill = candidate)) +
  geom_vline(xintercept = as.Date("2020-11-03")) +
  geom_ribbon(aes(ymin = avg - 1.645 * sqrt(var / eff_n), ymax = avg + 1.645 * sqrt(var / eff_n)), alpha = 0.2, col = NA) +
  geom_point(data = generic_ballot_polls, aes(y = pct), alpha = 0.5, size = 1) +
  geom_line(size = 1) +
  scale_colour_manual(name = "Party", values = party_colors, labels = party_names) +
  scale_fill_manual(name = "Party", values = party_colors, labels = party_names) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_labels = "%b %Y", limits = as.Date(c("2019-01-01", "2020-11-03")), breaks = date_breaks("2 months")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = -0.01)) +
  labs(title = "Generic House ballot polling", x = "Date", y = "%",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())),
       caption = "Averages smoothed over past five days")


# Senate ####
graph_states <- "North Carolina"
graph_seats <- "Class II"

graph_state_polls <- senate_polls %>% 
  filter(state %in% graph_states, seat_name %in% graph_seats)

current_poll_average <- senate_averages_adj %>%
  filter(median_date == today(), state == graph_states, seat_name == graph_seats) %>%
  na.omit()

graph_senate_candidate_names <- current_poll_average %>%
  mutate(candidate_party_letter = substr(candidate_party, 1, 1),
         candidate_label = paste0(candidate, " (", candidate_party_letter, ")")) %>%
  pull(candidate_label)

current_poll_average %>%
  ggplot(aes(x = candidate, y = avg, fill = candidate_party)) +
  geom_col() +
  geom_errorbar(aes(ymin = avg - 1.645 * sqrt(var / eff_n), ymax = avg + 1.645 * sqrt(var / eff_n)), col = "#666666") +
  geom_text(aes(y = avg + 0.01, label = scales::percent(avg, accuracy = 0.1)), size = 4) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(name = "Candidate", values = c("blue", "red"), labels = graph_senate_candidate_names) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(title = paste0(graph_states, " Senate polling average"), x = "Party", y = "Average %",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())),
       caption = "Error bars indicate 90% confidence intervals")

# Polls over time
senate_averages_smoothed %>%
  filter(state == graph_states, seat_name == graph_seats, candidate_party %in% c("DEM", "REP")) %>%
  ggplot(aes(x = median_date, y = avg, col = candidate_party, fill = candidate_party)) +
  geom_vline(xintercept = as.Date("2020-11-03")) +
  geom_ribbon(aes(ymin = avg - 1.645 * sqrt(var / eff_n), ymax = avg + 1.645 * sqrt(var / eff_n)), alpha = 0.2, col = NA) +
  geom_point(data = graph_state_polls %>% filter(candidate_party %in% c("DEM", "REP")), aes(y = pct), alpha = 0.5, size = 1) +
  geom_line(size = 1) +
  scale_colour_manual(name = "Candidate", values = c("blue", "red"), labels = graph_senate_candidate_names) +
  scale_fill_manual(name = "Candidate", values = c("blue", "red"), labels = graph_senate_candidate_names) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_labels = "%b %Y", limits = as.Date(c("2020-01-01", "2020-11-03")), breaks = date_breaks("2 months")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = -0.01)) +
  labs(title = paste0(graph_states, " Senate polling average"), x = "Candidate", y = "Average %",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())),
       caption = "Error bars indicate 90% confidence intervals")
