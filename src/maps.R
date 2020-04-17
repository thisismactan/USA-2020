source("src/library.R")

president_shp <- us_states() %>%
  filter(name != "Puerto Rico") %>%
  left_join(pres_state_probabilities, by = c("name" = "state")) %>%
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
  addPolygons(weight = 1, color = "#666666", opacity = 1, fillColor = ~color, fillOpacity = ~alpha, label = ~name, popup = ~infobox)
