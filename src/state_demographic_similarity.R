source("src/library.R")
source("src/census-mining/api_key.R")

# You should have your own Census API key; either assign it to api_key in an R script at "src/census-mining/api_key.R"
# or directly set it in the working environment using Sys.setenv()
Sys.setenv(CENSUS_KEY = api_key)
Sys.getenv("CENSUS_KEY")

varstoget <- c("DP05_0028E", "DP02_0032PE", "DP05_0072PE", "DP05_0033PE", "DP05_0066PE", "DP02_0092PE", 
               "DP02_0067PE", "DP03_0062E", "DP03_0119PE", "DP05_0017E")

states_2017 <- getCensus(name = "acs/acs1/profile", vintage = 2016, vars = varstoget, region = "state:*")
names(states_2017) <- c("State", "Population", "PctMarried", "PctWhite", "PctBlack", "PctLatino", "PctImm", 
                       "PctColl", "MedInc", "Poverty", "MedAge")

states_2017 <- states_2017 %>%
  mutate(State = as.numeric(State))

state_fips <- read_csv("data/auxiliary-demographics/state_fips.csv")
state_area <- read_csv("data/auxiliary-demographics/state_area.csv")
state_religion <- read_csv("data/auxiliary-demographics/state_religion_2010.csv")
state_dem_margins <- read_csv("data/presidential_election_results_by_state.csv") %>%
  filter(year %in% 2012:2016) %>%
  group_by(year, state) %>%
  mutate(two_party_pct = votes / sum(votes)) %>%
  ungroup() %>%
  dplyr::select(-candidate, -votes) %>%
  spread(party, two_party_pct) %>%
  mutate(margin = Democratic - Republican) %>%
  dplyr::select(-Democratic, -Republican) %>%
  spread(year, margin) %>%
  dplyr::select(State = state, dem_2012 = `2012`, dem_2016 = `2016`)

state_features <- state_fips %>%
  left_join(states_2017, by = c("FIPS" = "State")) %>%
  left_join(state_area, by = "State") %>%
  mutate(Density = Population/Area) %>%
  left_join(state_religion %>% dplyr::select(State, PctReligious), by = "State") %>%
  left_join(state_dem_margins, by = "State")

cd_features <- read_csv("data/auxiliary-demographics/cd_demographics_me_ne.csv")

# Compute principal components
state_pca <- prcomp(~ PctWhite + PctBlack + PctLatino + PctColl + MedInc + Poverty + MedAge + Density + PctReligious, 
                    data = state_features %>% bind_rows(cd_features) %>% arrange(State), scale = TRUE)

# Grab principal component scores and compute covariance matrix
state_pcs <- state_pca$x
state_eigenvalues <- state_pca$sdev^2
state_cor <- round(cov.wt(t(state_pcs), wt = state_eigenvalues, cor = TRUE)$cor, 6)
rownames(state_cor) <- colnames(state_cor) <- state_features %>% bind_rows(cd_features) %>% arrange(State) %>% pull(State)

polling_error_sd <- 0.05

state_cov <- state_cor * (polling_error_sd^2)
