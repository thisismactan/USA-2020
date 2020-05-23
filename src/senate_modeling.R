source("src/shape_senate_data.R")

senate_lm <- lm(margin ~ state_margin + last_margin + incumbent_running + dem_statewide_elected + rep_statewide_elected, 
                data = historical_senate_results_filtered)

senate_lmer <- lmer(margin ~ state_margin + last_margin + incumbent_running + dem_statewide_elected + rep_statewide_elected + (1|region), 
                    data = historical_senate_results_filtered)

summary(senate_lm)
summary(senate_lmer)

# Variance decomposition
senate_region_sd <- sqrt(as.vector(summary(senate_lmer)$varcor$region))
senate_residual_sd <- summary(house_lm)$sigma
