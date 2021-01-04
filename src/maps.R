source("src/library.R")

# President
president_shp <- us_states() %>%
  filter(name != "Puerto Rico") %>%
  left_join(conditional_state_probabilities %>% dplyr::select(state, Biden = biden, Trump = trump), by = c("name" = "state")) %>%
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

# House
house_district_key <- us_congressional() %>%
  as.data.frame() %>%
  dplyr::select(statefp, state = state_name) %>%
  distinct()

house_shp_simplified <- st_read("data/shapefiles/house/tl_2018_us_cd116.shp") %>%
  ms_simplify()

house_shp <- house_shp_simplified %>%
  left_join(house_district_key, by = c("STATEFP" = "statefp")) %>%
  mutate(seat_number = as.numeric(as.character(CD116FP)),
         seat_number = ifelse(seat_number == 0, 1, seat_number)) %>%
  left_join(district_summary_stats, by = c("state", "seat_number")) %>%
  left_join(house_candidates_2020, by = c("state", "seat_number")) %>%
  mutate(color = case_when(dem_prob >= 0.5 ~ "#104E8B",
                           dem_prob < 0.5 ~ "firebrick"),
         alpha = 0.75 * (2*abs(dem_prob - 0.5)),
         winner = case_when(dem_prob >= 0.5 ~ "Democrat",
                            dem_prob < 0.5 ~ "Republican"),
         max_prob = pmax(dem_prob, 1 - dem_prob),
         qual_prob = case_when(max_prob < 0.65 ~ "Toss-up",
                               max_prob >= 0.65 & max_prob < 0.8 ~ "Leans",
                               max_prob >= 0.8 & max_prob < 0.95 ~ "Likely",
                               max_prob >= 0.95 ~ "Safe"),
         qual_forecast = case_when(qual_prob == "Toss-up" ~ "Toss-up",
                                   qual_prob != "Toss-up" & winner == "Democrat" ~ paste0(qual_prob, " <font color = 'blue'>", winner, "</font>"),
                                   qual_prob != "Toss-up" & winner == "Republican" ~ paste0(qual_prob, " <font color = 'red'>", winner, "</font>")),
         infobox = paste0("<b><u>", district_abbr, "</b></u><br>",
                          "<b>", qual_forecast, "</b> (", scales::percent(max_prob, accuracy = 1), ")<br>"))

leaflet(house_shp) %>%
  addTiles() %>%
  addPolygons(weight = 1, color = "#666666", opacity = 1, fillColor = ~color, fillOpacity = ~alpha, label = ~district_abbr, popup = ~infobox) %>%
  addPolylines(weight = 1, color = "#555555") %>%
  addPolylines(data = us_states(), weight = 1.5, color = "black")


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
                                   qual_prob != "Toss-up" & winner == "Democrat" ~ paste0(qual_prob, " <font color = 'blue'>", winner, "</font>"),
                                   qual_prob != "Toss-up" & winner == "Republican" ~ paste0(qual_prob, " <font color = 'red'>", winner, "</font>")),
         infobox = paste0("<b><u>", name, "</b></u><br>",
                          "<b>", qual_forecast, "</b> (", scales::percent(max_prob, accuracy = 1), ")<br>"))

senate_candidates <- read_csv("data/senate_candidates.csv")

senate_pickups_shp <- senate_shp %>%
  left_join(senate_candidates %>% dplyr::select(state, incumbent_party) %>% distinct(), by = c("name" = "state")) %>%
  mutate(pickup = case_when(incumbent_party == "DEM" & Republicans > Democrats ~ "Republican",
                            incumbent_party == "REP" & Democrats > Republicans ~ "Democratic"),
         pickup_color = case_when(pickup == "Democratic" ~ "blue",
                                  pickup == "Republican" ~ "red")) %>%
  filter(!is.na(pickup))

leaflet(senate_shp) %>%
  addPolygons(weight = 1, color = "#666666", opacity = 1, fillColor = ~color, fillOpacity = ~alpha, label = ~name, popup = ~infobox) %>%
  addPolylines(weight = 1, color = "#555555") %>%
  addPolylines(data = senate_pickups_shp, weight = 3, color = ~pickup_color)
