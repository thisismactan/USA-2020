source("src/shape_house_data.R")

set.seed(2020)

# Models
## Linear regression
house_lm_pre_2016 <- lm(margin ~ natl_margin + last_natl_margin + last_margin + incumbency_change + dem_pct_fundraising, 
               data = house_results_2party_filtered %>% filter(year < 2016))
summary(house_lm_pre_2016)

## Random forest
house_rf_pre_2016 <- randomForest(formula = margin ~ last_margin + natl_margin + last_natl_margin + state_margin + last_state_margin + pres_year + 
                           incumbency_change + dem_pct_fundraising + multiterm_dem + multiterm_rep, 
                           data = house_results_2party_filtered %>% filter(year < 2016), ntree = 200, 
                           importance = TRUE, mtry = 3)
house_rf_pre_2016

## Gradient boosted trees
house_results_matrix <- model.matrix(~0 + last_margin + natl_margin + last_natl_margin + state_margin + last_state_margin + pres_year + 
                                       incumbency_change + dem_pct_fundraising + multiterm_dem + multiterm_rep, 
                                     data = house_results_2party_filtered %>% filter(year < 2016))
house_results_dmatrix <- xgb.DMatrix(data = house_results_matrix, label = house_results_2party_filtered %>% filter(year < 2016) %>% pull(margin))

xgb_params_list <- list(objective = "reg:squarederror",
                        eta = 0.1, 
                        max_depth = 3,
                        nthread = 10,
                        alpha = 0)

house_xgb_cv <- xgb.cv(params = xgb_params_list,
                       data = house_results_dmatrix,
                       nrounds = 1000,
                       nfold = 10,
                       early_stopping_rounds = 20)

# Estimating error from 2016 model
house_results_pre_2016_matrix <- model.matrix(~0 + last_margin + natl_margin + last_natl_margin + state_margin + last_state_margin + pres_year + 
                                               incumbency_change, data = house_results_2party_filtered %>% filter(year < 2016))
house_results_pre_2016_dmatrix <- xgb.DMatrix(data = house_results_pre_2016_matrix, 
                                              label = house_results_2party_filtered %>% filter(year < 2016) %>% pull(margin))

house_pre_2016_xgb <- xgb.train(params = xgb_params_list, data = house_results_pre_2016_dmatrix, nrounds = house_xgb_cv$best_iteration, nfold = 10)

house_results_2016_matrix <- model.matrix(~0 + last_margin + natl_margin + last_natl_margin + state_margin + last_state_margin + pres_year + 
                                            incumbency_change, data = house_results_2party_filtered %>% filter(year == 2016))
house_results_2016_dmatrix <- xgb.DMatrix(data = house_results_2016_matrix)

# Model evaluation
## Linear regression
house_results_2party_filtered %>%
  ungroup() %>%
  filter(year == 2016, !(state == "Hawaii" & seat_number == 1)) %>%
  mutate(pred = predict(house_lm_pre_2016, newdata = .),
         residual = pred - margin) %>%
  summarise(avg_residual = mean(residual),
            residual_sd = sd(residual),
            rmse = sqrt(mean(residual^2)),
            mad = mean(abs(residual)),
            smape = mean(2 * abs(residual) / (abs(pred) + abs(margin))))

house_results_2party_filtered %>%
  ungroup() %>%
  filter(year == 2016, !(state == "Hawaii" & seat_number == 1)) %>%
  mutate(pred = predict(house_lm_pre_2016, newdata = .),
         residual = pred - margin) %>%
  ggplot(aes(x = margin, y = residual)) +
  geom_point() +
  geom_smooth(method = "loess") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Linear regression prediction error on 2016 House results", x = "Actual margin",
       y = "Residual")

## Random forest
house_results_2party_filtered %>%
  ungroup() %>%
  filter(year == 2016, !(state == "Hawaii" & seat_number == 1)) %>%
  mutate(pred = predict(house_rf_pre_2016, newdata = .),
         residual = pred - margin) %>%
  summarise(avg_residual = mean(residual),
            residual_sd = sd(residual),
            rmse = sqrt(mean(residual^2)),
            mad = mean(abs(residual)),
            smape = mean(2 * abs(residual) / (abs(pred) + abs(margin))))

house_results_2party_filtered %>%
  ungroup() %>%
  filter(year == 2016, !(state == "Hawaii" & seat_number == 1)) %>%
  mutate(pred = predict(house_rf_pre_2016, newdata = .),
         residual = pred - margin) %>%
  ggplot(aes(x = margin, y = residual)) +
  geom_point() +
  geom_smooth(method = "loess") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Random forest prediction error on 2016 House results", x = "Actual margin",
       y = "Residual")

## XGBoost
house_results_2party_filtered %>%
  ungroup() %>%
  filter(year == 2016) %>%
  mutate(pred = predict(house_pre_2016_xgb, newdata = house_results_2016_dmatrix),
         residual = pred - margin) %>%
  summarise(avg_residual = mean(residual),
            residual_sd = sd(residual),
            rmse = sqrt(mean(residual^2)),
            mad = mean(abs(residual)),
            smape = mean(2 * abs(residual) / (abs(pred) + abs(margin))))

house_results_2party_filtered %>%
  ungroup() %>%
  filter(year == 2016) %>%
  mutate(pred = predict(house_pre_2016_xgb, newdata = house_results_2016_dmatrix),
         residual = pred - margin) %>%
  ggplot(aes(x = margin, y = residual)) +
  geom_point() +
  geom_smooth(method = "loess") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "XGBoost prediction error on 2016 House results", x = "Actual margin",
       y = "Residual")

# 2018 model
house_lm <- lm(margin ~ natl_margin + last_natl_margin + last_margin + incumbency_change, 
               data = house_results_2party_filtered)
house_lm_fundraising <- lm(margin ~ natl_margin + last_natl_margin + last_margin + incumbency_change + dem_pct_fundraising, 
                           data = house_results_2party_filtered)
house_lmer <- lmer(margin ~ natl_margin + last_natl_margin + last_margin + incumbency_change + (1|state) + (1|region), 
                   data = house_results_2party_filtered)
house_lmer_fundraising <- lmer(margin ~ natl_margin + last_natl_margin + last_margin + incumbency_change + dem_pct_fundraising + (1|state) + 
                                 (1|region), data = house_results_2party_filtered)

region_sd <- sqrt(as.vector(summary(house_lmer_fundraising)$varcor$region))
state_sd <- sqrt(as.vector(summary(house_lmer_fundraising)$varcor$state))
residual_sd <- summary(house_lmer_fundraising)$sigma
