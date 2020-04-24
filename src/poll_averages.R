source("src/shape_polls.R")

poll_dates <- seq(from = as.Date("2019-06-01"), to = today(), by = 1)
n_days <- length(poll_dates)

national_president_poll_list <- national_president_average_list <- national_president_sd_list <- vector("list", n_days)

# Presidential polls ####
for(i in 1:n_days) {
  current_date <- poll_dates[i]
  
  # Compute weights
  national_president_poll_list[[i]] <- national_president_polls %>%
    mutate(age = as.numeric(current_date - median_date),
           weight = (age <= 60) * (age >= 0) * loess_weight / exp((age + 1)^0.5)) %>%
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
  dplyr::select(poll_id, question_id, median_date, pollster, pop, party, loess_weight, trump_lead, avg_trump_lead, diff)

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
  mutate(pct_adj = case_when(candidate == "biden" ~ pct + house / 2 + rv_bias + 0.01 * (party == "REP") - 0.01 * (party == "DEM"),
                             candidate == "trump" ~ pct - house / 2 - rv_bias + 0.01 * (party == "DEM") - 0.01 * (party == "REP"),
                             !(candidate %in% c("biden", "trump")) ~ pct))


# Covariance matrix for current polls
president_poll_matrix <- national_president_polls_adj %>%
  mutate(weight = (age <= 60) * (age >= 0) * loess_weight / exp((age + 1)^0.5)) %>%
  filter(weight > 0) %>%
  dplyr::select(weight, poll_id, question_id, candidate, pct) %>% 
  spread(candidate, pct) %>%
  dplyr::select(weight, biden, trump)

president_poll_covariance <- cov.wt(as.matrix(president_poll_matrix %>% dplyr::select(biden, trump)), wt = president_poll_matrix$weight)

# Recompute with house effect-adjusted polls
national_president_polls_adj_list <- national_president_average_adj_list <- national_president_sd_adj_list <- vector("list", n_days)


# Presidential polls
for(i in 1:n_days) {
  current_date <- poll_dates[i]
  
  # Compute weights
  national_president_polls_adj_list[[i]] <- national_president_polls_adj %>%
    mutate(age = as.numeric(current_date - median_date),
           weight = (age <= 60) * (age >= 0) * loess_weight / exp((age + 1)^0.5)) %>%
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

# Time trend-adjusting state polls
state_president_poll_leans <- state_president_polls %>%
  left_join(national_president_averages_adj %>% dplyr::select(-state), by = c("candidate", "median_date")) %>%
  left_join(house_effects, by = "pollster") %>%
  mutate(house = case_when(is.na(house) ~ 0,
                           !is.na(house) ~ house),
         pct = case_when(candidate == "biden" ~ pct + house / 2 + rv_bias + 0.01 * (party == "REP") - 0.01 * (party == "DEM"),
                         candidate == "trump" ~ pct - house / 2 - rv_bias - 0.01 * (party == "REP") + 0.01 * (party == "DEM"),
                         !(candidate %in% c("biden", "trump")) ~ pct),
         state_lean = pct - avg)

state_president_poll_list <- state_president_average_list <- state_president_sd_list <- vector("list", n_days)

for(i in 1:n_days) {
  current_date <- poll_dates[i]
  
  # Compute weights
  state_president_poll_list[[i]] <- state_president_poll_leans %>%
    mutate(age = as.numeric(current_date - median_date),
           weight = 100 * (age >= 0) * loess_weight / exp((age + 1)^0.5)) %>%
    filter(weight > 0)
  
  # Compute averages and standard errors
  state_president_average_list[[i]] <- state_president_poll_list[[i]] %>%
    group_by(candidate, state) %>%
    summarise(avg_lean = wtd.mean(state_lean, weight),
              lean_var = wtd.var(state_lean, weight),
              lean_eff_n = sum(weight)^2 / sum(weight^2)) %>%
    mutate(median_date = current_date)
}

state_president_averages <- bind_rows(state_president_average_list) %>%
  arrange(state, median_date, candidate) %>%
  left_join(national_president_averages_adj %>% dplyr::select(-state), by = c("candidate", "median_date")) %>%
  mutate(state_avg = avg + avg_lean,
         state_var = lean_var + var,
         state_eff_n = lean_eff_n) %>%
  dplyr::select(candidate, state, avg = state_avg, var = state_var, eff_n = state_eff_n, median_date) %>%
  arrange(state, candidate, median_date) %>%
  group_by(state, candidate) %>%
  na.locf()
  
president_averages <- bind_rows(national_president_averages_adj, state_president_averages)

# Smoothed averages
president_averages_smoothed <- president_averages %>%
  mutate(avg = (lag(avg, 4) + lag(avg, 3) + lag(avg, 2) + lag(avg) + avg) / 5,
         var = (lag(var, 4) + lag(var, 3) + lag(var, 2) + lag(var) + var) / 5,
         eff_n = (lag(eff_n, 4) + lag(eff_n, 3) + lag(eff_n, 2) + lag(eff_n) + eff_n) / 5)


# House polls ####
generic_ballot_poll_list <- generic_ballot_average_list <- generic_ballot_sd_list <- vector("list", n_days)

for(i in 1:n_days) {
  current_date <- poll_dates[i]
  
  # Compute weights
  generic_ballot_poll_list[[i]] <- generic_ballot_polls %>%
    mutate(age = as.numeric(current_date - median_date),
           weight = (age <= 60) * (age >= 0) * loess_weight / exp((age + 2)^0.5)) %>%
    filter(weight > 0)
  
  # Compute averages and standard errors
  generic_ballot_average_list[[i]] <- generic_ballot_poll_list[[i]] %>%
    group_by(candidate) %>%
    summarise(avg = wtd.mean(pct, weight),
              var = n() * wtd.var(pct, weight) / (n() - 1),
              eff_n = sum(weight)^2 / sum(weight^2)) %>%
    mutate(median_date = current_date)
}

generic_ballot_averages <- bind_rows(generic_ballot_average_list)

# Computing house effects
dem_generic_ballot_leads <- generic_ballot_polls %>%
  filter(candidate %in% c("dem", "rep")) %>%
  left_join(generic_ballot_averages, by = c("candidate", "median_date")) %>%
  arrange(poll_id, question_id, candidate) %>%
  group_by(poll_id, question_id) %>%
  mutate(dem_lead = pct - lead(pct),
         avg_dem_lead = avg - lead(avg),
         diff = dem_lead - avg_dem_lead) %>%
  filter(!is.na(dem_lead), !is.na(avg_dem_lead)) %>%
  dplyr::select(poll_id, question_id, median_date, pollster, pop, party, loess_weight, dem_lead, avg_dem_lead, diff)

generic_ballot_house_effect_model <- lmer(diff ~ (1|pop) + (1|pollster), data = dem_generic_ballot_leads, weights = loess_weight)

# Converting to a mergeable data frame
generic_ballot_house_effects <- ranef(generic_ballot_house_effect_model)$pollster %>%
  as.data.frame()
generic_ballot_house_effects <- generic_ballot_house_effects %>% 
  mutate(pollster = rownames(generic_ballot_house_effects)) %>%
  dplyr::select(pollster, house = `(Intercept)`) %>%
  as.tbl()

# Bias due to RV
generic_ballot_rv_bias <- ranef(generic_ballot_house_effect_model)$pop["rv", 1]

# Adjusted national polls
generic_ballot_polls_adj <- generic_ballot_polls %>%
  left_join(generic_ballot_house_effects, by = c("pollster")) %>%
  mutate(pct_adj = case_when(candidate == "dem" ~ pct - house / 2 - generic_ballot_rv_bias + 0.01 * (party == "REP") - 0.01 * (party == "DEM"),
                             candidate == "rep" ~ pct + house / 2 + generic_ballot_rv_bias + 0.01 * (party == "DEM") - 0.01 * (party == "REP"),
                             !(candidate %in% c("dem", "rep")) ~ pct))


# Covariance matrix for current polls
generic_ballot_poll_matrix <- generic_ballot_polls_adj %>%
  mutate(weight = (age <= 60) * (age >= 0) * loess_weight / exp((age + 2)^0.5)) %>%
  filter(weight > 0) %>%
  dplyr::select(weight, poll_id, question_id, candidate, pct) %>% 
  spread(candidate, pct) %>%
  dplyr::select(weight, dem, rep)

generic_ballot_poll_covariance <- cov.wt(as.matrix(generic_ballot_poll_matrix %>% dplyr::select(dem, rep)), 
                                         wt = generic_ballot_poll_matrix$weight)

# Recompute with house effect-adjusted polls
generic_ballot_polls_adj_list <- generic_ballot_average_adj_list <- vector("list", n_days)

# Presidential polls
for(i in 1:n_days) {
  current_date <- poll_dates[i]
  
  # Compute weights
  generic_ballot_polls_adj_list[[i]] <- generic_ballot_polls_adj %>%
    mutate(age = as.numeric(current_date - median_date),
           weight = (age <= 60) * (age >= 0) * loess_weight / exp((age + 1)^0.5)) %>%
    filter(weight > 0)
  
  # Compute averages and standard errors
  generic_ballot_average_adj_list[[i]] <- generic_ballot_polls_adj_list[[i]] %>%
    group_by(candidate) %>%
    summarise(avg = wtd.mean(pct, weight),
              var = n() * wtd.var(pct, weight) / (n() - 1),
              eff_n = sum(weight)^2 / sum(weight^2)) %>%
    mutate(median_date = current_date)
}

# Averages
generic_ballot_averages_adj <- bind_rows(generic_ballot_average_adj_list)

# Smoothed averages
generic_ballot_averages_smoothed <- generic_ballot_averages_adj %>%
  group_by(candidate) %>%
  arrange(candidate, median_date) %>%
  mutate(avg = (lag(avg, 4) + lag(avg, 3) + lag(avg, 2) + lag(avg) + avg) / 5,
         var = (lag(var, 4) + lag(var, 3) + lag(var, 2) + lag(var) + var) / 5,
         eff_n = (lag(eff_n, 4) + lag(eff_n, 3) + lag(eff_n, 2) + lag(eff_n) + eff_n) / 5) %>%
  ungroup()
