source("src/shape_polls.R")

poll_dates <- seq(from = as.Date("2019-06-01"), to = today(), by = 1)
n_days <- length(poll_dates)

national_president_poll_list <- national_president_average_list <- national_president_sd_list <- vector("list", n_days)

# Presidential polls
for(i in 1:n_days) {
  current_date <- poll_dates[i]
  
  # Compute weights
  national_president_poll_list[[i]] <- national_president_polls %>%
    mutate(age = as.numeric(current_date - median_date),
           weight = (age <= 30) * (age >= 0) * loess_weight / exp((age + 1)^0.5)) %>%
    filter(weight > 0)
  
  # Compute averages and standard errors
  national_president_average_list[[i]] <- national_president_poll_list[[i]] %>%
    group_by(candidate, state) %>%
    summarise(avg = wtd.mean(pct, weight),
              var = n() * wtd.var(pct, weight) / (n() - 1),
              eff_n = sum(weight)^2 / sum(weight^2)) %>%
    mutate(median_date = current_date)
}

national_president_averages <- bind_rows(national_president_average_list)

# Computing house effects
trump_leads <- national_president_polls %>%
  left_join(national_president_averages, by = c("candidate", "median_date")) %>%
  filter(candidate %in% c("biden", "trump")) %>%
  arrange(poll_id, question_id, candidate) %>%
  group_by(poll_id, question_id) %>%
  mutate(trump_lead = pct - lag(pct),
         avg_trump_lead = avg - lag(avg),
         diff = trump_lead - avg_trump_lead) %>%
  filter(!is.na(trump_lead), !is.na(avg_trump_lead)) %>%
  dplyr::select(poll_id, question_id, median_date, pollster, pop, loess_weight, trump_lead, avg_trump_lead, diff)

house_effect_model <- lmer(diff ~ (1|pop) + (1|pollster), data = trump_leads, weights = loess_weight)

# Converting to a mergeable data frame
house_effects <- ranef(house_effect_model)$pollster %>%
  as.data.frame()
house_effects <- house_effects %>% 
  mutate(pollster = rownames(house_effects)) %>%
  dplyr::select(pollster, house = `(Intercept)`) %>%
  as.tbl()

# Bias due to RV
rv_bias <- ranef(house_effect_model)$pop["rv", 1]

# Adjusted national polls
national_president_polls_adj <- national_president_polls %>%
  left_join(house_effects, by = c("pollster")) %>%
  mutate(pct_adj = case_when(candidate == "biden" ~ pct + house / 2 + rv_bias / 2,
                             candidate == "trump" ~ pct - house / 2 - rv_bias / 2,
                             !(candidate %in% c("biden", "trump")) ~ pct))

# Recompute with house effect-adjusted polls
national_president_polls_adj_list <- national_president_average_adj_list <- national_president_sd_adj_list <- vector("list", n_days)

# Presidential polls
for(i in 1:n_days) {
  current_date <- poll_dates[i]
  
  # Compute weights
  national_president_polls_adj_list[[i]] <- national_president_polls_adj %>%
    mutate(age = as.numeric(current_date - median_date),
           weight = (age <= 30) * (age >= 0) * loess_weight / exp((age + 1)^0.5)) %>%
    filter(weight > 0)
  
  # Compute averages and standard errors
  national_president_average_adj_list[[i]] <- national_president_polls_adj_list[[i]] %>%
    group_by(candidate, state) %>%
    summarise(avg = wtd.mean(pct, weight),
              var = n() * wtd.var(pct, weight) / (n() - 1),
              eff_n = sum(weight)^2 / sum(weight^2)) %>%
    mutate(median_date = current_date)
}

# Averages
national_president_averages_adj <- bind_rows(national_president_average_adj_list)

national_president_averages_adj_smoothed <- national_president_averages_adj %>%
  arrange(candidate, median_date) %>%
  mutate(avg = (lag(avg, 2) + lag(avg) + avg) / 3,
         var = (lag(var, 2) + lag(var) + var) / 3,
         eff_n = (lag(eff_n, 2) + lag(eff_n) + eff_n) / 3)

# Time trend-adjusting state polls