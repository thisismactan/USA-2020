source("src/library.R")

# President
pres_results_prelim <- read_csv("data/called_pres_winners.csv") %>%
  mutate(state = gsub("'s 1st", " CD-1", state),
         state = gsub("'s 2nd", " CD-2", state),
         state = gsub("'s 3rd", " CD-3", state))

pres_polling_error <- pres_results_prelim %>%
  left_join(president_averages %>% filter(median_date == as.Date("2020-11-03")) %>% dplyr::select(-var, -median_date) %>% spread(candidate, avg),
            by = "state") %>%
  mutate(poll_error = (biden - trump) - margin) %>%
  # Merge in number of polls in final month of campaign
  left_join(
    president_polls %>%
      filter(end_date >= as.Date("2020-10-03"), is.na(tracking)) %>%
      group_by(state) %>%
      summarise(n_polls = n_distinct(poll_id)) %>%
      ungroup(),
    by = "state"
  )

pres_polling_error %>% 
  print(n = Inf)

## Make a map of it
error_palette <- colorNumeric(colorRamp(colors = c("#104E8B", "white", "firebrick")), domain = c(-20, 20))

polling_error_shp <- us_states() %>%
  filter(name != "Puerto Rico") %>%
  left_join(pres_polling_error %>% dplyr::select(state, biden, trump, poll_error, n_polls), by = c("name" = "state")) %>%
  mutate(poll_error = 100 * poll_error,
         poll_error_na = ifelse(n_polls < 5, as.numeric(NA), poll_error))

leaflet(polling_error_shp) %>%
  addPolygons(weight = 1, color = "#666666", opacity = 1, fillColor = ~error_palette(poll_error_na), fillOpacity = 1) %>%
  addPolylines(weight = 1, color = "#555555") %>%
  addLegend(position = "bottomright", pal = error_palette, values = ~poll_error, title = "Trump poll\noverperformance", 
            labFormat = labelFormat(suffix = ".0 pp"), na.label = "N/A")

## State-level calibration
pres_result_comparison <- pres_state_summary_stats %>%
  ungroup() %>%
  left_join(pres_results_prelim %>%
              mutate(state = gsub(" CD-1", "'s 1st congressional district", state),
                     state = gsub(" CD-2", "'s 2nd congressional district", state),
                     state = gsub(" CD-3", "'s 3rd congressional district", state)),
            by = c("State" = "state"))

pres_result_comparison %>%
  mutate(prob_group = cut(biden_prob, breaks = c(-Inf, 0.1, 0.5, 0.9, Inf))) %>%
  group_by(prob_group) %>%
  summarise(n = n(),
            expected_wins = sum(biden_prob),
            actual_wins = sum(margin > 0))

pres_result_comparison %>%
  left_join(regions %>% dplyr::select(State = state, abbrev), by = "State") %>%
  mutate(two_party_margin = (biden_pct - trump_pct) / (biden_pct + trump_pct)) %>%
  ggplot(aes(x = 100 * pct50, y = 100 * (pct50 - two_party_margin), col = biden_pct)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_text(aes(label = abbrev), size = 3) +
  scale_x_continuous(breaks = seq(from = -60, to = 100, by = 10)) +
  scale_y_continuous(breaks = seq(from = -6, to = 20, by = 2)) +
  scale_colour_gradient2(name = "Biden vote share", low = "red", mid = "#880088", high = "blue", midpoint = 0.5, label = scales::percent) +
  theme(legend.position = "bottom") +
  labs(title = "Forecast error vs. actual margin", x = "Biden two-party margin, pp", y = "Mean forecast margin minus actual Biden margin, pp",
       caption = "Actual margin as of 6:27 PM EST November 14, 2020")

## Boxplot it
president_polls %>%
  filter(end_date >= as.Date("2020-10-03")) %>%
  dplyr::select(poll_id, question_id, median_date, state, tracking, loess_weight, candidate, pct) %>%
  spread(candidate, pct) %>%
  mutate(poll_margin = biden - trump) %>%
  left_join(pres_results_prelim, by = "state") %>%
  filter(!is.na(margin)) %>%
  mutate(biden_overperformance = 100 * (margin - poll_margin)) %>%
  left_join(pres_polling_error %>% dplyr::select(state, poll_error, n_polls), by = "state") %>%
  filter(state %in% comp_states, n_polls >= 5, is.na(tracking)) %>%
  ggplot(aes(x = biden_overperformance, y = reorder(state, poll_error), weight = loess_weight, fill = 100 * margin)) +
  geom_vline(xintercept = 0) +
  geom_boxplot(varwidth = TRUE) +
  scale_fill_gradient2(name = "Biden actual\nmargin, pp", low = "red", high = "blue") +
  labs(title = "2020 state-level presidential polling errors", subtitle = "Final 30 days of campaign",
       x = "Biden actual margin - Biden margin in poll", y = "", 
       caption = "States decided by single digits in 2016\nActual margin as of 6:27 PM EST November 14, 2020")

## How the model would have done with just the right national popular vote numbers alone
pres_model_only_results <- state_two_party_margins_2016 %>%
  mutate(national_two_party_margin = (77961485 - 72643856) / (77961485 + 72643856),
         two_party_margin_natl_change = national_two_party_margin - two_party_margin_2016,
         predicted_two_party_margin =  state_two_party_margin * fixed_effect_coefficients["last_two_party_margin"] +
           two_party_margin_natl_change * fixed_effect_coefficients["two_party_margin_natl_change"],
         state = gsub(" congressional district", "", state)) %>%
  left_join(regions %>% mutate(state = gsub(" congressional district", "", state)) %>% dplyr::select(state, abbrev, electoral_votes), 
            by = "state") %>%
  left_join(pres_results_prelim %>% 
              mutate(state = gsub(" CD-1", "'s 1st", state),
                     state = gsub(" CD-2", "'s 2nd", state),
                     state = gsub(" CD-3", "'s 3rd", state)), by = "state") %>%
  mutate(actual_two_party_margin = (biden_pct - trump_pct) / (biden_pct + trump_pct))

pres_model_only_results %>%
  filter(abbrev != "DC") %>%
  ggplot(aes(x = 100 * predicted_two_party_margin, y = 100 * actual_two_party_margin, col = biden_pct)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") + 
  geom_text(aes(label = abbrev), size = 3) +
  scale_x_continuous(breaks = seq(from = -60, to = 100, by = 10)) +
  scale_y_continuous(breaks = seq(from = -60, to = 100, by = 10)) +
  scale_colour_gradient2(name = "Biden vote share", low = "red", mid = "#880088", high = "blue", midpoint = 0.5, label = scales::percent) +
  theme(legend.position = "bottom") +
  labs(title = "Actual presidential results by state", subtitle = "vs. model predictions given actual national popular vote",
       x = "Biden predicted two-party margin, pp", y = "Biden actual two-party margin")

# House
house_results_prelim <- read_csv("data/called_house_winners.csv") %>%
  mutate(margin = (dem_pct - rep_pct) / (dem_pct + rep_pct))

house_result_comparison <- district_summary_stats %>%
  ungroup() %>%
  left_join(house_results_prelim, by = c("state", "seat_number")) %>%
  filter(!is.na(margin))

## Calibration
house_result_comparison %>%
  mutate(prob_group = cut(dem_prob, breaks = c(-Inf, 0.05, 0.25, 0.4, 0.6, 0.75, 0.95, Inf))) %>%
  group_by(prob_group) %>%
  summarise(n = n(),
            expected_wins = sum(dem_prob),
            actual_wins = sum(margin > 0),
            avg_error = mean(avg - margin)) %>%
  as.data.frame()

## Error
house_result_comparison %>%
  filter(abs(margin) != 1) %>%
  ggplot(aes(x = 100 * (avg - margin))) +
  geom_vline(xintercept = house_result_comparison %>% filter(abs(margin) != 1) %>% 
               summarise(avg_error = 100 * mean(avg - margin, na.rm = TRUE)) %>% pull(avg_error),
             col = "blue", size = 1) +
  geom_histogram(col = "black", fill = "blue", alpha = 0.5, binwidth = 1) +
  scale_x_continuous(breaks = seq(from = -30, to = 30, by = 10), limits = c(-30, 30)) +
  theme(legend.position = "bottom") +
  labs(title = "Distribution of forecast error", 
       x = "Average final forecast prediction minus actual Democratic margin (two-party, pp)", y = "Number of districts") +
  lims(y = c(0, 40))

### By incumbency
house_result_comparison %>%
  filter(abs(margin) != 1) %>%
  left_join(house_candidates_2020 %>% dplyr::select(state, seat_number, incumbent_running), by = c("state", "seat_number")) %>%
  mutate(incumbent = case_when(incumbent_running == "DEM" ~ "Democratic incumbent",
                               incumbent_running == "REP" ~ "Republican incumbent",
                               incumbent_running == "None" ~ "No incumbent running"))  %>%
  ggplot(aes(x = 100 * (avg - margin))) +
  facet_wrap(~incumbent, nrow = 3) +
  geom_vline(data = house_result_comparison %>%
               filter(abs(margin) != 1) %>%
               left_join(house_candidates_2020 %>% dplyr::select(state, seat_number, incumbent_running), by = c("state", "seat_number")) %>%
               mutate(incumbent = case_when(incumbent_running == "DEM" ~ "Democratic incumbent",
                                            incumbent_running == "REP" ~ "Republican incumbent",
                                            incumbent_running == "None" ~ "No incumbent running")) %>%
               group_by(incumbent) %>%
               summarise(avg_error = 100 * mean(avg - margin, na.rm = TRUE)),
             aes(xintercept = avg_error, col = incumbent), size = 1, show.legend = FALSE) +
  geom_histogram(aes(fill = incumbent), col = "black", alpha = 0.5, binwidth = 1, show.legend = FALSE) +
  scale_x_continuous(breaks = seq(from = -40, to = 40, by = 10), limits = c(-30, 30)) +
  scale_colour_manual(name = "Incumbent party", values = c("Democratic incumbent" = "blue", "Republican incumbent" = "red",
                                                           "No incumbent running" = "gray")) +
  scale_fill_manual(name = "Incumbent party", values = c("Democratic incumbent" = "blue", "Republican incumbent" = "red",
                                                         "No incumbent running" = "gray")) +
  labs(title = "Distribution of forecast error by incumbent party", 
       x = "Average final forecast prediction minus actual Democratic margin (two-party, pp)", y = "Number of districts")
  

## How the model would have done with just the right national popular vote numbers alone
house_model_only_results <- house_2020_data %>%
  mutate(natl_margin = (77128499 - 72730624) / (77128499 + 72730624)) %>%
  mutate(predicted_two_party_margin = predict(house_lm_fundraising, newdata = .)) %>%
  left_join(house_candidates_2020 %>% dplyr::select(state, seat_number, district_abbr) %>% distinct(),
            by = c("state", "seat_number")) %>%
  left_join(house_results_prelim, by = c("state", "seat_number")) %>%
  mutate(error = margin - predicted_two_party_margin) %>%
  filter(dem_running, rep_running) %>%
  dplyr::select(state, seat_number, district_abbr, predicted_two_party_margin, dem_pct, rep_pct, margin, error)

house_model_only_results %>%
  filter(abs(margin) != 1) %>%
  ggplot(aes(x = 100 * error)) +
  geom_vline(xintercept = house_model_only_results %>% 
               filter(abs(margin) != 1) %>%
               summarise(avg_error = 100 * mean(error, na.rm = TRUE)) %>% 
               pull(avg_error),
             col = "blue", size = 1) +
  geom_histogram(col = "black", fill = "blue", alpha = 0.5, binwidth = 1) +
  scale_x_continuous(breaks = seq(from = -40, to = 40, by = 10), limits = c(-30, 30)) +
  theme(legend.position = "bottom") +
  labs(title = "Distribution of model error", subtitle = "Conditional on actual national House popular vote",
       x = "Model prediction minus actual Democratic margin (two-party, pp)", y = "Number of districts") +
  lims(y = c(0, 40))

# Senate
senate_results_prelim <- read_csv("data/called_senate_winners.csv") %>%
  mutate(margin = (dem_pct - rep_pct) / (dem_pct + rep_pct))

senate_polling_error <- senate_results_prelim %>%
  left_join(senate_averages_adj %>% ungroup() %>% filter(median_date == as.Date("2020-11-03")) %>% dplyr::select(-var, -median_date, -candidate) %>% 
              spread(candidate_party, avg) %>% mutate(poll_margin = (DEM - REP) / (DEM + REP)) %>% dplyr::select(state, seat_name, poll_margin),
            by = c("state", "seat_name")) %>%
  mutate(poll_error = poll_margin - margin) %>%
  # Merge in number of polls in final month of campaign
  left_join(
    senate_polls %>%
      filter(end_date >= as.Date("2020-10-03"), is.na(tracking)) %>%
      group_by(state, seat_name) %>%
      summarise(n_polls = n_distinct(poll_id)) %>%
      ungroup(),
    by = c("state", "seat_name")
  )

senate_polling_error %>% summarise(avg_error = mean(poll_error, na.rm = TRUE))

## Correlation with presidential polling error
senate_polling_error %>%
  filter(n_polls > 2) %>%
  mutate(senate_winner = ifelse(dem_pct > rep_pct, "Democrat", "Republican")) %>%
  dplyr::select(state, senate_winner, senate_poll_error = poll_error, n_senate_polls = n_polls) %>%
  filter(!is.na(senate_poll_error)) %>%
  left_join(pres_polling_error %>%
              dplyr::select(state, pres_poll_error = poll_error, n_pres_polls = n_polls),
            by = "state") %>%
  left_join(regions %>% dplyr::select(state, abbrev), by = "state") %>%
  ggplot(aes(x = 100 * pres_poll_error, y = 100 * senate_poll_error)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_text(aes(label = abbrev, col = senate_winner), size = 3) +
  scale_x_continuous(breaks = seq(from = -4, to = 16, by = 4)) +
  scale_y_continuous(breaks = seq(from = -4, to = 16, by = 4)) +
  scale_colour_manual(name = "Senate winner", values = c("Democrat" = "blue", "Republican" = "red")) +
  labs(title = "Senate polling error vs. presidential polling error", subtitle = "Democratic two-party margin",
       x = "Presidential polling error (pp)", y = "Senate polling error (pp)",
       caption = "States with fewer than three Senate polls not shown")

## If I just ignored state polls
senate_error_comparison <- senate_2020_prior_sims %>% 
  group_by(state, seat_name) %>% 
  summarise(prior_avg = mean(prior_margin)) %>%
  left_join(senate_results_prelim %>% dplyr::select(state, seat_name, margin),
            by = c("state", "seat_name")) %>%
  mutate(prior_error = prior_avg - margin) %>%
  left_join(senate_state_sims %>% 
              group_by(state, seat_name) %>%
              summarise(forecast_avg = mean(margin)) %>%
              ungroup(),
            by = c("state", "seat_name")) %>%
  mutate(forecast_error = forecast_avg - margin) %>%
  dplyr::select(state, seat_name, prior_error, forecast_error) %>%
  ungroup() %>%
  filter(state != "Arkansas", !(state == "Georgia" & seat_name == "Class III"))

senate_error_comparison %>%
  filter(prior_error != forecast_error) %>%
  mutate(forecast_error_2 = forecast_error) %>%
  melt(id.vars = c("state", "seat_name", "forecast_error_2"), variable.name = "estimate", value.name = "error") %>%
  ggplot(aes(x = reorder(state, forecast_error_2), y = 100 * error, col = estimate, fill = estimate)) +
  geom_hline(yintercept = 0) +
  geom_point(aes(shape = estimate)) +
  scale_colour_manual(name = "", values = c("prior_error" = "green4", "forecast_error" = "orange"),
                      labels = c("prior_error" = "Without polls", "forecast_error" = "With polls")) +
  scale_fill_manual(name = "", values = c("prior_error" = "green4", "forecast_error" = "orange"),
                    labels = c("prior_error" = "Without polls", "forecast_error" = "With polls")) +
  scale_shape_manual(name = "", values = c("prior_error" = 21, "forecast_error" = 22),
                     labels = c("prior_error" = "Without polls", "forecast_error" = "With polls")) +
  labs(title = "Senate forecast average error with and without polls", x = "State", y = "Simulation average error (pp)",
       caption = "States with no Senate polling not shown") +
  coord_flip()

### Some error statistics
senate_error_comparison %>%
  melt(id.vars = c("state", "seat_name"), variable.name = "estimate", value.name = "error") %>%
  group_by(estimate) %>%
  summarise(bias = mean(error),
            mad = mean(abs(error)),
            rmse = sqrt(mean(error^2)))

## Map
error_palette <- colorNumeric(colorRamp(colors = c("#104E8B", "white", "firebrick")), domain = c(-20, 22))

polling_error_shp <- us_states() %>%
  filter(name != "Puerto Rico") %>%
  left_join(senate_polling_error %>% dplyr::select(state, dem_pct, rep_pct, poll_error, n_polls), by = c("name" = "state")) %>%
  mutate(poll_error = 100 * poll_error,
         poll_error_na = ifelse(n_polls < 3, as.numeric(NA), poll_error)) %>%
  filter(!(name == "Georgia" & is.na(dem_pct)))

leaflet(polling_error_shp) %>%
  addPolygons(weight = 1, color = "#666666", opacity = 1, fillColor = ~error_palette(poll_error_na), fillOpacity = 1) %>%
  addPolylines(weight = 1, color = "#555555") %>%
  addLegend(position = "bottomright", pal = error_palette, values = ~poll_error, title = "GOP poll\noverperformance", 
            labFormat = labelFormat(suffix = ".0 pp"), na.label = "N/A")

senate_polls %>%
  filter(end_date >= as.Date("2020-10-03"), end_date <= as.Date("2020-11-03")) %>%
  dplyr::select(poll_id, question_id, median_date, state, tracking, loess_weight, candidate_party, pct) %>%
  spread(candidate_party, pct) %>%
  mutate(poll_margin = DEM - REP) %>%
  left_join(senate_results_prelim, by = "state") %>%
  filter(!is.na(margin)) %>%
  mutate(dem_overperformance = 100 * (margin - poll_margin)) %>%
  left_join(senate_polling_error %>% dplyr::select(state, poll_error, n_polls), by = "state") %>%
  filter(n_polls >= 3, is.na(tracking)) %>%
  ggplot(aes(x = dem_overperformance, y = reorder(state, poll_error), weight = loess_weight, fill = 100 * margin)) +
  geom_vline(xintercept = 0) +
  geom_boxplot(varwidth = TRUE) +
  scale_fill_gradient2(name = "Democratic actual\nmargin, pp", low = "red", high = "blue") +
  labs(title = "2020 Senate polling errors", subtitle = "Final 30 days of campaign",
       x = "Democratic actual margin - Democratic margin in poll", y = "", 
       caption = "States with 3 or more polls in final month of campaign")
