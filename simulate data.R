#### Preamble ####
# Purpose: Simulates the data
# Author: Krishiv Jain
# Date: 6 February 2024
# Contact: krishiv.jain@mail.utoronto.ca

#### Workspace setup ####
library(tidyverse)

#### Simulate data ####
set.seed(555) #random seed

simulated_data <-  
  tibble(
    #Randomly pick numbers from the normal distribution
    prime_minister = 1:15,
    birth_year = runif(15, min = 1800, max = 2024),
    death_year = runif(15, min = 1800, max = 2024)
  )

simulated_data$death_year <- pmax(simulated_data$death_year, simulated_data$birth_year + 1)

simulated_data

### Tests ###

##Check the minimum number is equal, or greater than, 0 ##
#Based on: https://tellingstorieswithdata.com/02-drinking_from_a_fire_hose.html#simulate
simulated_data$birth_year |> min() >= 0
simulated_data$death_year |> min() >= 0