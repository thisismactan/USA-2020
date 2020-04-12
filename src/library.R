## LIBRARIES

#### Data manipulation ####
library(Hmisc)
library(lubridate)
library(reshape2)
library(scales)
library(tidyverse)

#### Modeling ####
library(lme4)
library(randomForest)
library(xgboost)

#### Scraping and data collection ####
library(httr)
library(tidycensus)
library(rvest)
library(utils)
library(xml2)

#### Simulation ####
library(doParallel)
library(foreach)
library(mvnfast)

#### Mapping, potentially ####
library(leaflet)
library(rgdal)
library(sf)

## COLORS/LABELS
candidate_fullnames <- c("biden" = "Joe Biden (D)", "trump" = "Donald Trump (R)")
candidate_colors <- c("biden" = "blue", "trump" = "red")
candidate_lastnames <- c("biden" = "Biden", "trump" = "Trump")