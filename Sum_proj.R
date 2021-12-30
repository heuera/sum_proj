# Austin's MSc Summer Project in R

library(pacman)
p_load(tidyverse, dplyr, forcats, ggplot2, lubridate, purrr, haven, survey, modelr)

# Read in datasets
child <- read_dta("/Users/austinheuer/Desktop/LSHTM/Summer Project/00_Ethiopia Data/Data_for_Austin_child1.dta")
house <- read_dta("/Users/austinheuer/Desktop/LSHTM/Summer Project/00_Ethiopia Data/Data_for_Austin_household.dta")

# Join data sets
comp_data <- left_join(child, house) 
comp_data <- as_tibble(comp_data)

# Filter out children older than five
comp_data <- comp_data %>% filter(ageyears < 5)

# Organize variables 
comp_data <- comp_data %>% mutate(haz = as.factor(haz))
levels(comp_data$haz) <- c("Not stunted", "Stunted")
comp_data$surveyyear <- cut(comp_data$surveyyear, 
                        breaks = c(-Inf, 2000, 2005, 2011, 2016), 
                        labels = c("2000", "2005", "2011", "2016"))
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
comp_data$religion <- as.factor(comp_data$religion)
comp_data$religion <- fct_collapse(comp_data$religion,
                                        Muslim = c("Moslem", "Muslim", "Muslin"),
                                        Catholic = "Catholic",
                                        Missing = "Missing",
                                        Orthodox = "Orthodox",
                                        Other = "Other",
                                        Protestant = "Protestant",
                                        Traditional = "Traditional")
comp_data$edhigh <- cut(comp_data$edhigh,
                        breaks = c(-Inf, 0, 1, 3),
                        labels = c("None", "Primary", "Secondary or more"))
comp_data$motherage <- cut(comp_data$motherage,
                        breaks = c(-Inf, 26.1, 31.1, 50),
                        labels = c("<26", "26-32", ">32"))                                   
comp_data$motherheightcm <- cut(comp_data$motherheightcm,
                        breaks = c(-Inf, 1550.1, 1599, 1999),
                        labels = c("<1550", "1551-1599", ">1600"))
comp_data$clusteraltitude <- cut(comp_data$clusteraltitude,
                        breaks = c(-Inf, 0, 1400, 1999, 4000),
                        labels = c("Missing", "<1400", "1401-1999", ">2000"))
comp_data$clusteraltitude <- na_if(comp_data$clusteraltitude, "Missing")
comp_data$precipitation_mean <- cut(comp_data$precipitation_mean,
                        breaks = c(-Inf, 70, 94.99, 185),
                        labels = c("<70", "70-95", ">95"))
comp_data$lstmean <- cut(comp_data$lstmean,
                         breaks = c(-Inf, 30, 33.999, 50),
                         labels = c("<30", "30-34", ">34"))

# Account for DHS survey design
comp_data <- comp_data %>% mutate(
            hh_samp_weight = as.numeric(hhweight/1000000),
            unique_id = as.numeric(paste0(surveyid, clusterid,"")))
DHSdesign <- comp_data %>% 
                    svydesign(id = comp_data$unique_id, 
                              strata = comp_data$urbanrural, 
                              weights = comp_data$hh_samp_weight, 
                              probs = NULL)

############################ Descriptive Analysis ##############################
svy: tab haz eth, column ci 

svyby(~ageyears, ~comp_data$ethnic, DHSdesign, svymean, na.rm = TRUE) #good
svymean(~comp_data$ageyears, DHSdesign, na.rm = TRUE) #good

svy: tab gender eth, column ci 
svy: tab healthcard eth, column ci 
svy: tab whz eth, column ci // wasted
svy: tab waz eth, column ci //underweight

svymean(~comp_data$border, DHSdesign, na.rm = TRUE) #good
svyby(~border, ~comp_data$ethnic, DHSdesign, svymean, na.rm = TRUE) #good

svy: tab religion_n eth, column ci 

svymean(~comp_data$motherage, DHSdesign, na.rm = TRUE) #good
svyby(~motherage, ~comp_data$ethnic, DHSdesign, svymean, na.rm = TRUE) #good
svymean(~comp_data$motherheightcm, DHSdesign, na.rm = TRUE) #good
svyby(~motherheightcm, ~comp_data$ethnic, DHSdesign, svymean, na.rm = TRUE) #good

svy: tab crowding eth, column ci //not avail. for all years
svy: tab swater eth, column ci 
svy: tab housetype eth, column ci //not avail. for all years
svy: tab tfacility eth, column ci 
svy: tab wealthq eth, column ci //not avail. for all years
svy: tab edhigh eth, column ci 
svy: tab urbanrural eth, column ci 

svymean(~comp_data$clusteraltitude, DHSdesign, na.rm = TRUE) #good
svyby(~clusteraltitude, ~comp_data$ethnic, DHSdesign, svymean, na.rm = TRUE) #good
svymean(~comp_data$precipitation_mean, DHSdesign, na.rm = TRUE) #good
svyby(~precipitation_mean, ~comp_data$ethnic, DHSdesign, svymean, na.rm = TRUE) #good
svymean(~comp_data$lstmean, DHSdesign, na.rm = TRUE) #good
svyby(~lstmean, ~comp_data$ethnic, DHSdesign, svymean, na.rm = TRUE) #good

# Association with stunting
svy: tab haz eth, count
svy: tab haz eth, col
svyglm(haz ~ ethnic + surveyyear, design = DHSdesign, family = "binomial") 

svy: tab haz ageyears, count
svy: tab haz ageyears, col
svyglm(haz ~ ageyears + surveyyear, design = DHSdesign, family = "binomial")

svy: tab haz gender, count
svy: tab haz gender, col
svyglm(haz ~ gender + surveyyear, design = DHSdesign, family = "binomial")

svy: tab haz healthcard, count
svy: tab haz healthcard, col
svyglm(haz ~ healthcard + surveyyear, design = DHSdesign, family = "binomial")

svy: tab haz diarrhoea, count
svy: tab haz diarrhoea, col
svyglm(haz ~ diarrhoea + surveyyear, design = DHSdesign, family = "binomial")

svy: tab haz whz, count format(%5.2f)
svy: tab haz whz, col
svyglm(haz ~ whz + surveyyear, design = DHSdesign, family = "binomial")

svy: tab haz waz, count
svy: tab haz waz, col
svyglm(haz ~ waz + surveyyear, design = DHSdesign, family = "binomial")

svy: tab haz border_cat, count
svy: tab haz border_cat, col
svyglm(haz ~ border + surveyyear, design = DHSdesign, family = "binomial")

svy: tab haz religion_n, count
svy: tab haz religion_n, col
svyglm(haz ~ religion + surveyyear, design = DHSdesign, family = "binomial")


svy: tab haz motherage_cat, count
svy: tab haz motherage_cat, col
svyglm(haz ~ motherage + surveyyear, design = DHSdesign, family = "binomial")


svy: tab haz motherheightcm_cat, count
svy: tab haz motherheightcm_cat, col
svyglm(haz ~ motherheightcm + surveyyear, design = DHSdesign, family = "binomial")

svy: tab haz crowding, count
svy: tab haz crowding, col
svyglm(haz ~ crowding + surveyyear, design = DHSdesign, family = "binomial")

svy: tab haz swater, count
svy: tab haz swater, col
svyglm(haz ~ swater + surveyyear, design = DHSdesign, family = "binomial")

svy: tab haz housetype, count
svy: tab haz housetype, col
svyglm(haz ~ housetype + surveyyear, design = DHSdesign, family = "binomial")

svy: tab haz tfacility, count format(%5.2f)
svy: tab haz tfacility, col
svyglm(haz ~ tfacility + surveyyear, design = DHSdesign, family = "binomial")

svy: tab haz wealthq, count
svy: tab haz wealthq, col
svyglm(haz ~ wealthq + surveyyear, design = DHSdesign, family = "binomial")

svy: tab haz edhigh, count
svy: tab haz edhigh, col
svyglm(haz ~ edhigh + surveyyear, design = DHSdesign, family = "binomial")


svy: tab haz urbanrural, count format(%5.2f)
svy: tab haz urbanrural, col
svyglm(haz ~ urbanrural + surveyyear, design = DHSdesign, family = "binomial")

svy: tab haz altitude_cat, count 
svy: tab haz altitude_cat, col
svyglm(haz ~ clusteraltitude + surveyyear, design = DHSdesign, family = "binomial")

svy: tab haz rain, count 
svy: tab haz rain, col
svyglm(haz ~ precipitation_mean + surveyyear, design = DHSdesign, family = "binomial")

svy: tab haz temp, count 
svy: tab haz temp, col
svyglm(haz ~ lstmean + surveyyear, design = DHSdesign, family = "binomial")


################################### Models #####################################
# Crude model
crude <- svyglm(haz ~ ethnic + surveyyear, design = DHSdesign, family = "binomial")
exp(coefficients(crude))

# Adjusted model one
adj1 <- svyglm(haz ~ ethnic + surveyyear + wealthq + edhigh + gender + ageyears + diarrhoea + 
                  religion + waz + whz + urbanrural + tfacility + swater + healthcard + 
                  crowding + housetype + border + clusteraltitude + lstmean +
                    motherheightcm + motherage + precipitation_mean, design = DHSdesign, family = "binomial")
exp(coefficients(adj1))

# Adjusted model two
adj2 <- svyglm(haz ~ ethnic + surveyyear + edhigh + gender + ageyears + diarrhoea + 
                 religion + waz + whz + urbanrural + tfacility + swater + healthcard + 
                 border + lstmean +
                 motherheightcm + motherage + precipitation_mean, design = DHSdesign, family = "binomial")
exp(coefficients(adj2))

# Adjusted model two (still need to do)
adj3 <- filter(comp_data$surveyyear != "2000") %>%  svyglm(haz ~ ethnic + surveyyear + edhigh + gender + ageyears + diarrhoea + 
                 religion + waz + whz + urbanrural + tfacility + swater + healthcard + 
                 border + lstmean +
                 motherheightcm + motherage + precipitation_mean, design = DHSdesign, family = "binomial")
exp(coefficients(adj3))



