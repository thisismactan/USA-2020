source("src/shape_house_data.R")

# Models
## Linear regression
house_lm <- lm(margin ~ last_margin + natl_margin + last_natl_margin + state_margin + last_state_margin + pres_year * incumbent_running, 
               data = house_results_2party_filtered)
summary(house_lm)

## Random forest
house_rf <- randomForest(formula = margin ~ last_margin + natl_margin + last_natl_margin + state_margin + last_state_margin + pres_year + 
                           incumbency_change, data = house_results_2party_filtered, ntree = 150, importance = TRUE)
house_rf

## Gradient boosted trees
house_results_matrix <- model.matrix(~0 + last_margin + natl_margin + last_natl_margin + state_margin + last_state_margin + pres_year + 
                                       incumbency_change, data = house_results_2party_filtered)
house_results_dmatrix <- xgb.DMatrix(data = house_results_matrix, label = house_results_2party_filtered$margin)

house_xgb_cv <- xgb.cv(params = list(objective = "reg:squarederror",
                                     eta = 0.05, 
                                     max_depth = 3,
                                     nthread = 10),
                       data = house_results_dmatrix,
                       nrounds = 500,
                       nfold = 10,
                       early_stopping_rounds = 20)