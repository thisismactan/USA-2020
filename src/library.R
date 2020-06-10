## LIBRARIES

#### Data manipulation ####
library(glue)
library(Hmisc)
library(lubridate)
library(reshape2)
library(scales)
library(tidyverse)
library(zoo)

#### Modeling ####
library(lme4)
library(MCMCpack)
library(mvnfast)
library(randomForest)
library(xgboost)

#### Scraping and data collection ####
library(censusapi)
library(httr)
library(tidycensus)
library(R.openFEC)
library(rvest)
library(utils)
library(xml2)

#### Simulation ####
library(doParallel)
library(foreach)
library(mvnfast)
library(matrixcalc)

#### Mapping ####
library(leaflet)
library(rgdal)
library(rmapshaper)
library(sf)
library(sp)
library(USAboundaries)

## COLORS/LABELS
candidate_fullnames <- c("biden" = "Joe Biden (D)", "trump" = "Donald Trump (R)", "amash" = "Justin Amash (L)")
candidate_colors <- c("biden" = "blue", "trump" = "red", "amash" = "gold3")
candidate_lastnames <- c("biden" = "Biden", "trump" = "Trump", "amash" = "Amash")

party_names <- c("dem" = "Democratic", "rep" = "Republican")
party_colors <- c("dem" = "blue", "rep" = "red")

## CUSTOM FUNCTIONS
replace_na_zero <- function(x) {
  x <- case_when(is.na(x) ~ 0,
                 !is.na(x) ~ x) 
  return(x)
}

logit <- function(x) {
  logit_x <- log(x / (1 - x))
  return(logit_x)
}

logit_inv <- function(x) {
  invlogit_x <- exp(x) / (1 + exp(x))
  return(invlogit_x)
}