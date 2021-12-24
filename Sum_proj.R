# Austin's MSc Summer Project in R

library(pacman)
p_load(tidyverse, dplyr, forcats, ggplot2, lubridate, purrr, haven, survey, modelr)

# Read in datasets
child <- read_dta("/Users/austinheuer/Desktop/LSHTM/Summer Project/00_Ethiopia Data/Data_for_Austin_child1.dta")
house <- read_dta("/Users/austinheuer/Desktop/LSHTM/Summer Project/00_Ethiopia Data/Data_for_Austin_household.dta")

# Join datasets
comp_data <- left_join(child, house)

# Filter out children older than five
comp_data <- comp_data %>% filter(ageyears < 5)

# Organize ethnicity 
comp_data <- comp_data %>% mutate(ethnic = as.factor(eth))
levels(comp_data$ethnic) <- c("Oromo", "Amhara", "Tigrie", "Other")
comp_data <- comp_data %>% mutate(electricity = as.factor(electricity))
levels(comp_data$electricity) <- c("No", "Yes")
comp_data <- comp_data %>% mutate(radio = as.factor(radio))
levels(comp_data$radio) <- c("No", "Yes")
comp_data <- comp_data %>% mutate(tv = as.factor(tv))
levels(comp_data$tv) <- c("No", "Yes")
comp_data <- comp_data %>% mutate(fridge = as.factor(fridge))
levels(comp_data$fridge) <- c("No", "Yes")
comp_data <- comp_data %>% mutate(bicycle = as.factor(bicycle))
levels(comp_data$bicycle) <- c("No", "Yes")
comp_data <- comp_data %>% mutate(scooter = as.factor(scooter))
levels(comp_data$scooter) <- c("No", "Yes")
comp_data <- comp_data %>% mutate(car = as.factor(car))
levels(comp_data$car) <- c("No", "Yes")
comp_data$border <- cut(comp_data$border, 
                        breaks = c(-Inf, 1, 2, 3, 4, 5, 6, 20), 
                        labels = c("1", "2", "3", "4", "5", "6", "7+"))







