source("src/library.R")

# President
president_shp <- us_states() %>%
  filter(name != "Puerto Rico") %>%
  left_join(pres_state_probabilities %>% dplyr::select(state, Biden = biden, Trump = trump), by = c("name" = "state")) %>%
  mutate(color = case_when(Biden > Trump ~ "#104E8B",
                           Trump >= Biden ~ "firebrick"),
         alpha = case_when(Biden > Trump ~ sqrt(2*(Biden - 0.5)),
                           Trump >= Biden ~ sqrt(2*(Trump - 0.5))),
         winner = case_when(Biden > Trump ~ "Biden",
                            Trump >= Biden ~ "Trump"),
         max_prob = pmax(Biden, Trump),
         qual_prob = case_when(max_prob < 0.65 ~ "Toss-up",
                               max_prob >= 0.65 & max_prob < 0.8 ~ "Leans",
                               max_prob >= 0.8 & max_prob < 0.95 ~ "Likely",
                               max_prob >= 0.95 ~ "Safe"),
         qual_forecast = case_when(qual_prob == "Toss-up" ~ "Toss-up",
                                   qual_prob != "Toss-up" & winner == "Biden" ~ paste0(qual_prob, " <font color = 'blue'>", winner, "</font>"),
                                   qual_prob != "Toss-up" & winner == "Trump" ~ paste0(qual_prob, " <font color = 'red'>", winner, "</font>")),
         infobox = paste0("<b><u>", name, "</b></u><br>",
                          "<b>", qual_forecast, "</b> (", scales::percent(max_prob, accuracy = 1), ")<br>"))

leaflet(president_shp) %>%
  addPolygons(weight = 1, color = "#666666", opacity = 1, fillColor = ~color, fillOpacity = ~alpha, label = ~name, popup = ~infobox) %>%
  addPolylines(weight = 1, color = "#555555")

# Senate
senate_shp <- us_states() %>%
  filter(name != "Puerto Rico") %>%
  left_join(senate_state_probabilities %>% 
              spread(party, prob) %>% 
              dplyr::select(state, seat_name, Democrats, Republicans) %>% 
              filter(!(state == "Georgia" & seat_name == "Class III")), 
            by = c("name" = "state")) %>%
  mutate(color = case_when(Democrats > Republicans ~ "#104E8B",
                           Republicans >= Democrats ~ "firebrick"),
         alpha = case_when(Democrats > Republicans ~ sqrt(2*(Democrats - 0.5)),
                           Republicans >= Democrats ~ sqrt(2*(Republicans - 0.5))),
         winner = case_when(Democrats > Republicans ~ "Democrat",
                            Republicans >= Democrats ~ "Republican"),
         max_prob = pmax(Democrats, Republicans),
         qual_prob = case_when(max_prob < 0.65 ~ "Toss-up",
                               max_prob >= 0.65 & max_prob < 0.8 ~ "Leans",
                               max_prob >= 0.8 & max_prob < 0.95 ~ "Likely",
                               max_prob >= 0.95 ~ "Safe"),
         qual_forecast = case_when(qual_prob == "Toss-up" ~ "Toss-up",
                                   qual_prob != "Toss-up" & winner == "Democrats" ~ paste0(qual_prob, " <font color = 'blue'>", winner, "</font>"),
                                   qual_prob != "Toss-up" & winner == "Republican" ~ paste0(qual_prob, " <font color = 'red'>", winner, "</font>")),
         infobox = paste0("<b><u>", name, "</b></u><br>",
                          "<b>", qual_forecast, "</b> (", scales::percent(max_prob, accuracy = 1), ")<br>"))

leaflet(senate_shp) %>%
  addPolygons(weight = 1, color = "#666666", opacity = 1, fillColor = ~color, fillOpacity = ~alpha, label = ~name, popup = ~infobox) %>%
  addPolylines(weight = 1, color = "#555555")
