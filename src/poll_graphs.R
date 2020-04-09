source("src/poll_averages.R")

state <- "National"

# Current average
current_poll_average <- tail(national_president_average_adj_list, 1) %>%
  as.data.frame()
