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

historical_senate_results_filtered %>%
  ggplot(aes(x = state_margin, y = margin)) +
  geom_point(aes(col = factor(year))) +
  geom_smooth(method = "lm", col = "black", linetype = 2) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_colour_manual(name = "Year", values = c("2012" = "green4", "2016" = "blue")) +
  labs(title = "Senate election results vs. presidential election results", subtitle = "Contested races in presidential election years",
       x = "Democratic margin of victory in state presidential election", y = "Democratic margin of victory in Senate election")

