src("data/shape_senate_data.R")

senate_lm <- lm(margin ~ state_margin + last_margin + incumbent_running + dem_statewide_elected + rep_statewide_elected, 
                data = historical_senate_results_filtered)

senate_sigma <- 