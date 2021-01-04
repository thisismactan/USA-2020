source("src/shape_polls.R")

# Getting polling averages and covariance
georgia_runoff_polls_adj <- senate_polls_all %>%
  filter(state == "Georgia", start_date > as.Date("2020-11-03")) %>%
  left_join(house_effects, by = "pollster") %>%
  mutate(house = ifelse(is.na(house), 0, house),
         pct_adj = case_when(candidate_party == "DEM" ~ pct - 0.5 * house + 0.01 * (party == "REP") - 0.01 * (party == "DEM"),
                             candidate_party == "REP" ~ pct + 0.5 * house - 0.01 * (party == "REP") + 0.01 * (party == "DEM")),
         weight = (age <= 30) * (age >= 0) * loess_weight / exp((age + 1)^0.5),
         candidate = gsub("A. ", "", candidate))

georgia_runoff_average <- georgia_runoff_polls_adj %>%
  group_by(seat_name, candidate_party, candidate) %>%
  summarise(avg = wtd.mean(pct, weight),
            eff_n = sum(weight)^2 / sum(weight^2),
            var = wtd.var(pct_adj, weight) * n() / (n() - 1))

georgia_eff_n <- mean(georgia_runoff_average$eff_n)

georgia_runoff_average

## Graph of polling averages
georgia_runoff_average %>%
  ggplot(aes(x = reorder(candidate, 1:4), y = avg, fill = candidate_party)) +
  facet_wrap(~seat_name, scales = "free_x") + 
  geom_col() +
  geom_errorbar(aes(ymin = avg - 1.645 * sqrt(var / eff_n), ymax = avg + 1.645 * sqrt(var / eff_n)), col = "#444444") +
  geom_text(aes(y = avg + 0.01, label = scales::percent(avg, accuracy = 0.1)), size = 3) +
  scale_fill_manual(name = "Party", values = c("DEM" = "blue", "REP" = "red"), labels = c("DEM" = "Democratic", "REP" = "Republican")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Georgia Senate runoff polling", x = "", y = "Polling average",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())),
       caption = "Error bars indicate 90% confidence intervals")

georgia_poll_margins <- georgia_runoff_polls_adj %>%
  dplyr::select(-candidate, -question_id, -pct) %>%
  spread(candidate_party, pct_adj) %>%
  mutate(margin = DEM - REP) %>%
  dplyr::select(-DEM, -REP) %>%
  spread(seat_name, margin) %>%
  dplyr::select(weight, `Class II`, `Class III`)

georgia_poll_cov <- cov.wt(georgia_poll_margins %>% dplyr::select(-weight) %>% na.omit() %>% as.matrix(), 
                           wt = georgia_poll_margins %>% na.omit() %>% dplyr::select(weight) %>% pull(weight), cor = TRUE)

## Georgia Senate polling error from November
senate_results_prelim <- read_csv("data/called_senate_winners.csv") %>%
  mutate(margin = dem_pct - rep_pct,
         margin_2p = (dem_pct - rep_pct) / (dem_pct + rep_pct))

georgia_poll_mse <- senate_polls_adj %>%
  filter(state == "Georgia", end_date <= as.Date("2020-11-03")) %>%
  mutate(weight = (age <= 30) * (age >= 0) * loess_weight / exp((age + 1)^0.5)) %>%
  dplyr::select(-candidate) %>%
  spread(candidate_party, pct_adj) %>%
  mutate(margin = DEM - REP) %>%
  left_join(senate_results_prelim, by = c("state", "seat_name")) %>%
  mutate(error = margin.x - margin.y) %>%
  summarise(wmse = wtd.mean(error^2, weight))

# Simulation
set.seed(2020)
n_sims <- 25000

georgia_runoff_sims <- rmvt(n_sims, mu = georgia_poll_cov$center, sigma = georgia_poll_cov$cov + diag(georgia_poll_mse$wmse, nrow = 2), 
                            df = georgia_eff_n) %>%
  as.data.frame() %>%
  mutate(sim_id = 1:n()) %>%
  dplyr::select(sim_id, `Class II` = V1, `Class III` = V2) %>%
  melt(id.vars = "sim_id", variable.name = "seat_name", value.name = "margin") %>%
  as.tbl()

## Outcomes
georgia_runoff_sims %>%
  group_by(seat_name) %>%
  summarise(prob = mean(margin > 0),
            pct05 = quantile(margin, 0.05),
            avg = mean(margin),
            pct95 = quantile(margin, 0.95))
  
georgia_runoff_sims %>%
  mutate(winner = ifelse(margin > 0, "Democrat", "Republican")) %>%
  dplyr::select(-margin) %>%
  spread(seat_name, winner) %>%
  group_by(`Class II`, `Class III`) %>%
  summarise(prob = n() / n_sims)

# Showing results
georgia_runoff_sims %>%
  spread(seat_name, margin) %>%
  mutate(senate_majority = ifelse(`Class II` > 0 & `Class III` > 0, "Democratic", "Republican")) %>%
  ggplot(aes(x = 100 * `Class II`, y = 100 * `Class III`, col = senate_majority)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point(size = 1, alpha = 0.05, show.legend = FALSE) +
  scale_colour_manual(name = "Senate majority", values = c("Democratic" = "blue", "Republican" = "red")) +
  labs(title = "Simulated outcomes of Georgia Senate runoffs", x = "Class II (Ossoff - Perdue) margin", y = "Class III (Warnock - Loeffler) margin", 
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today()))) +
  lims(x = c(-40, 40), y = c(-40, 40))
