# Austin's MSc Summer Project in R

################################################################################
################################### Setup ######################################
################################################################################
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

cols <- c('electricity', 'radio', 'tv', 'fridge', 'bicycle', 'scooter', 'car') 
comp_data[cols] <- lapply(comp_data[cols], factor)
levels(comp_data$electricity) <- c("No", "Yes")
levels(comp_data$radio) <- c("No", "Yes")
levels(comp_data$tv) <- c("No", "Yes")
levels(comp_data$fridge) <- c("No", "Yes")
levels(comp_data$bicycle) <- c("No", "Yes")
levels(comp_data$scooter) <- c("No", "Yes")
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
                              probs = NULL)

################################################################################
############################ Descriptive Analysis ##############################
################################################################################
# HAZ & ethnicity
svytable(~haz+ethnic, DHSdesign)
svychisq(~haz+ethnic, DHSdesign, statistic = "adjWald")
svyby(~haz, comp_data$ethnic, DHSdesign, svyciprop, vartype = "ci", na.rm = TRUE, level = 0.95)

# child age in years & ethnicity
svyby(~ageyears, ~comp_data$ethnic, DHSdesign, svymean, na.rm = TRUE) 
svymean(~comp_data$ageyears, DHSdesign, na.rm = TRUE) 

# gender & ethnicity
svytable(~gender+ethnic, DHSdesign)
svychisq(~gender+ethnic, DHSdesign, statistic = "adjWald")
svyby(~gender, comp_data$ethnic, DHSdesign, svyciprop, vartype = "ci", na.rm = TRUE, level = 0.95)

# has a health card & ethnicity
svytable(~healthcard+ethnic, DHSdesign)
svychisq(~healthcard+ethnic, DHSdesign, statistic = "adjWald")
svyby(~healthcard, comp_data$ethnic, DHSdesign, svyciprop, vartype = "ci", na.rm = TRUE, level = 0.95)

# WHZ & ethnicity
svytable(~whz+ethnic, DHSdesign)
svychisq(~whz+ethnic, DHSdesign, statistic = "adjWald")
svyby(~whz, comp_data$ethnic, DHSdesign, svyciprop, vartype = "ci", na.rm = TRUE, level = 0.95)

# WAZ & ethnicity
svytable(~waz+ethnic, DHSdesign)
svychisq(~waz+ethnic, DHSdesign, statistic = "adjWald")
svyby(~waz, comp_data$ethnic, DHSdesign, svyciprop, vartype = "ci", na.rm = TRUE, level = 0.95)

# birth order & ethnicity
svymean(~comp_data$border, DHSdesign, na.rm = TRUE)
svyby(~border, ~comp_data$ethnic, DHSdesign, svymean, na.rm = TRUE) 

# religion & ethnicity
svytable(~religion+ethnic, DHSdesign)
svychisq(~religion+ethnic, DHSdesign, statistic = "adjWald")
svyby(~religion, comp_data$ethnic, DHSdesign, svyciprop, vartype = "ci", na.rm = TRUE, level = 0.95)

# mother's age & ethnicity
svymean(~comp_data$motherage, DHSdesign, na.rm = TRUE) 
svyby(~motherage, ~comp_data$ethnic, DHSdesign, svymean, na.rm = TRUE) 

# mother's height & ethnicity
svymean(~comp_data$motherheightcm, DHSdesign, na.rm = TRUE) 
svyby(~motherheightcm, ~comp_data$ethnic, DHSdesign, svymean, na.rm = TRUE) 

# house crowding & ethnicity
svytable(~crowding+ethnic, DHSdesign)
svychisq(~crowding+ethnic, DHSdesign, statistic = "adjWald")
svyby(~crowding, comp_data$ethnic, DHSdesign, svyciprop, vartype = "ci", na.rm = TRUE, level = 0.95)

# water source & ethnicity
svytable(~swater+ethnic, DHSdesign)
svychisq(~swater+ethnic, DHSdesign, statistic = "adjWald")
svyby(~swater, comp_data$ethnic, DHSdesign, svyciprop, vartype = "ci", na.rm = TRUE, level = 0.95)

# house type & ethnicity
svytable(~housetype+ethnic, DHSdesign)
svychisq(~housetype+ethnic, DHSdesign, statistic = "adjWald")
svyby(~housetype, comp_data$ethnic, DHSdesign, svyciprop, vartype = "ci", na.rm = TRUE, level = 0.95)

# toilet facility & ethnicity
svytable(~tfacility+ethnic, DHSdesign)
svychisq(~tfaciity+ethnic, DHSdesign, statistic = "adjWald")
svyby(~tfacility, comp_data$ethnic, DHSdesign, svyciprop, vartype = "ci", na.rm = TRUE, level = 0.95)

# wealth quintile & ethnicity
svytable(~wealthq+ethnic, DHSdesign)
svychisq(~wealthq+ethnic, DHSdesign, statistic = "adjWald")
svyby(~wealthq, comp_data$ethnic, DHSdesign, svyciprop, vartype = "ci", na.rm = TRUE, level = 0.95)

# household head highest education level & ethnicity
svytable(~edhigh+ethnic, DHSdesign)
svychisq(~edhigh+ethnic, DHSdesign, statistic = "adjWald")
svyby(~edhigh, comp_data$ethnic, DHSdesign, svyciprop, vartype = "ci", na.rm = TRUE, level = 0.95)

# urban or rural & ethnicity
svytable(~urbanrural+ethnic, DHSdesign)
svychisq(~urbanrural+ethnic, DHSdesign, statistic = "adjWald")
svyby(~urbanrural, comp_data$ethnic, DHSdesign, svyciprop, vartype = "ci", na.rm = TRUE, level = 0.95)

# altitude & ethnicity
svymean(~comp_data$clusteraltitude, DHSdesign, na.rm = TRUE) 
svyby(~clusteraltitude, ~comp_data$ethnic, DHSdesign, svymean, na.rm = TRUE) 

# mean monthly precip & ethnicity
svymean(~comp_data$precipitation_mean, DHSdesign, na.rm = TRUE) 
svyby(~precipitation_mean, ~comp_data$ethnic, DHSdesign, svymean, na.rm = TRUE) 

# land surface temp mean & ethnicity
svymean(~comp_data$lstmean, DHSdesign, na.rm = TRUE) 
svyby(~lstmean, ~comp_data$ethnic, DHSdesign, svymean, na.rm = TRUE) 

###################### Association with stunting ###############################
# HAZ & ethnicity
svytable(~haz+ethnic, DHSdesign)
svyby(~haz, comp_data$ethnic, DHSdesign, svyciprop, vartype = "ci", na.rm = TRUE, level = 0.95)
summary(svyglm(haz ~ factor(ethnic) + factor(surveyyear), design = DHSdesign, family = "binomial"))

# HAZ & child age in years
svytable(~haz+ageyears, DHSdesign)
svyby(~haz, comp_data$ageyears, DHSdesign, svyciprop, vartype = "ci", na.rm = TRUE, level = 0.95)
summary(svyglm(haz ~ factor(ageyears) + factor(surveyyear), design = DHSdesign, family = "binomial"))

# HAZ & gender
svytable(~haz+gender, DHSdesign)
svyby(~haz, comp_data$gender, DHSdesign, svyciprop, vartype = "ci", na.rm = TRUE, level = 0.95)
summary(svyglm(haz ~ factor(gender) + factor(surveyyear), design = DHSdesign, family = "binomial"))

# HAZ & has a health card
svytable(~haz+healthcard, DHSdesign)
svyby(~haz, comp_data$healthcard, DHSdesign, svyciprop, vartype = "ci", na.rm = TRUE, level = 0.95)
summary(svyglm(haz ~ factor(healthcard) + factor(surveyyear), design = DHSdesign, family = "binomial"))

# HAZ & diarrhoea
svytable(~haz+diarrhoea, DHSdesign)
svyby(~haz, comp_data$diarrhoea, DHSdesign, svyciprop, vartype = "ci", na.rm = TRUE, level = 0.95)
summary(svyglm(haz ~ factor(diarrhoea) + factor(surveyyear), design = DHSdesign, family = "binomial"))

# HAZ & WHZ
svytable(~haz+whz, DHSdesign)
svyby(~haz, comp_data$whz, DHSdesign, svyciprop, vartype = "ci", na.rm = TRUE, level = 0.95)
summary(svyglm(haz ~ factor(whz) + factor(surveyyear), design = DHSdesign, family = "binomial"))

# HAZ & WAZ
svytable(~haz+waz, DHSdesign)
svyby(~haz, comp_data$waz, DHSdesign, svyciprop, vartype = "ci", na.rm = TRUE, level = 0.95)
summary(svyglm(haz ~ factor(waz) + factor(surveyyear), design = DHSdesign, family = "binomial"))

# HAZ & birth order
svytable(~haz+border, DHSdesign)
svyby(~haz, comp_data$border, DHSdesign, svyciprop, vartype = "ci", na.rm = TRUE, level = 0.95)
summary(svyglm(haz ~ factor(border) + factor(surveyyear), design = DHSdesign, family = "binomial"))

# HAZ & religion
svytable(~haz+religion, DHSdesign)
svyby(~haz, comp_data$religion, DHSdesign, svyciprop, vartype = "ci", na.rm = TRUE, level = 0.95)
summary(svyglm(haz ~ factor(religion) + factor(surveyyear), design = DHSdesign, family = "binomial"))

# HAZ & mother's age
svytable(~haz+motherage, DHSdesign)
svyby(~haz, comp_data$motherage, DHSdesign, svyciprop, vartype = "ci", na.rm = TRUE, level = 0.95)
summary(svyglm(haz ~ factor(motherage) + factor(surveyyear), design = DHSdesign, family = "binomial"))

# HAZ & mother's height
svytable(~haz+motherheightcm, DHSdesign)
svyby(~haz, comp_data$motherheightcm, DHSdesign, svyciprop, vartype = "ci", na.rm = TRUE, level = 0.95)
summary(svyglm(haz ~ factor(motherheightcm) + factor(surveyyear), design = DHSdesign, family = "binomial"))

# HAZ & house crowding
svytable(~haz+crowding, DHSdesign)
svyby(~haz, comp_data$crowding, DHSdesign, svyciprop, vartype = "ci", na.rm = TRUE, level = 0.95)
summary(svyglm(haz ~ factor(crowding) + factor(surveyyear), design = DHSdesign, family = "binomial"))

# HAZ & water source
svytable(~haz+swater, DHSdesign)
svyby(~haz, comp_data$swater, DHSdesign, svyciprop, vartype = "ci", na.rm = TRUE, level = 0.95)
summary(svyglm(haz ~ factor(swater) + factor(surveyyear), design = DHSdesign, family = "binomial"))

# HAZ & house type
svytable(~haz+housetype, DHSdesign)
svyby(~haz, comp_data$housetype, DHSdesign, svyciprop, vartype = "ci", na.rm = TRUE, level = 0.95)
summary(svyglm(haz ~ factor(housetype) + factor(surveyyear), design = DHSdesign, family = "binomial"))

# HAZ & toilet facility
svytable(~haz+tfacility, DHSdesign)
svyby(~haz, comp_data$tfacility, DHSdesign, svyciprop, vartype = "ci", na.rm = TRUE, level = 0.95)
summary(svyglm(haz ~ factor(tfacility) + factor(surveyyear), design = DHSdesign, family = "binomial"))

# HAZ & family wealth quintile
svytable(~haz+wealthq, DHSdesign)
svyby(~haz, comp_data$wealthq, DHSdesign, svyciprop, vartype = "ci", na.rm = TRUE, level = 0.95)
summary(svyglm(haz ~ factor(wealthq) + factor(surveyyear), design = DHSdesign, family = "binomial"))

# HAZ & household head highest education level
svytable(~haz+edhigh, DHSdesign)
svyby(~haz, comp_data$edhigh, DHSdesign, svyciprop, vartype = "ci", na.rm = TRUE, level = 0.95)
summary(svyglm(haz ~ factor(edhigh) + factor(surveyyear), design = DHSdesign, family = "binomial"))

# HAZ & urban or rural
svytable(~haz+urbanrural, DHSdesign)
svyby(~haz, comp_data$urbanrural, DHSdesign, svyciprop, vartype = "ci", na.rm = TRUE, level = 0.95)
summary(svyglm(haz ~ factor(urbanrural) + factor(surveyyear), design = DHSdesign, family = "binomial"))

# HAZ & altitude
svytable(~haz+clusteraltitude, DHSdesign)
svyby(~haz, comp_data$clusteraltitude, DHSdesign, svyciprop, vartype = "ci", na.rm = TRUE, level = 0.95)
summary(svyglm(haz ~ factor(clusteraltitude) + factor(surveyyear), design = DHSdesign, family = "binomial"))

# HAZ & mean monthly precip
svytable(~haz+precipitation_mean, DHSdesign)
svyby(~haz, comp_data$precipitation_mean, DHSdesign, svyciprop, vartype = "ci", na.rm = TRUE, level = 0.95)
summary(svyglm(haz ~ factor(precipitation_mean) + factor(surveyyear), design = DHSdesign, family = "binomial"))

# HAZ & land surface temperature mean
svytable(~haz+lstmean, DHSdesign)
svyby(~haz, comp_data$lstmean, DHSdesign, svyciprop, vartype = "ci", na.rm = TRUE, level = 0.95)
summary(svyglm(haz ~ factor(lstmean) + factor(surveyyear), design = DHSdesign, family = "binomial"))

################################################################################
################################### Models #####################################
################################################################################
# Crude model
crude <- svyglm(haz ~ factor(ethnic) + factor(surveyyear), design = DHSdesign, family = "binomial")
summary(crude)
exp(coefficients(crude))
regTermTest(crude,"factor(ethnic)")
  
# Adjusted model 1
adj1 <- svyglm(haz ~ factor(ethnic) + factor(surveyyear) + factor(wealthq) + factor(edhigh) + factor(gender) + factor(ageyears) + factor(diarrhoea) + 
                    factor(religion) + factor(waz) + factor(whz) + factor(urbanrural) + factor(tfacility) + factor(swater) + factor(healthcard) + 
                    factor(crowding) + factor(housetype) + factor(border) + clusteraltitude + lstmean +
                    motherheightcm + motherage + precipitation_mean, design = DHSdesign, family = "binomial")
summary(adj1)
exp(coefficients(adj1))
regTermTest(adj1,"factor(ethnic)")
  
# Adjusted model 2
adj2 <- svyglm(haz ~ factor(ethnic) + factor(surveyyear) + factor(edhigh) + factor(gender) + factor(ageyears) + factor(diarrhoea) + 
                     factor(religion) + factor(waz) + factor(whz) + factor(urbanrural) + factor(tfacility) + factor(swater) + factor(healthcard) + 
                     factor(border) + lstmean + motherheightcm + motherage + precipitation_mean, design = DHSdesign, family = "binomial")
summary(adj2)
exp(coefficients(adj2))
regTermTest(adj2,"factor(ethnic)")
  
# Adjusted model 3a (2000 survey year only)
comp_data2000 <- comp_data %>% filter(surveyyear == "2000")
DHSdesign2000 <- comp_data2000 %>% 
          svydesign(id = comp_data2000$unique_id, 
                    strata = comp_data2000$urbanrural, 
                    probs = NULL)
adj3_2000 <- svyglm(haz ~ factor(ethnic) + factor(edhigh) + factor(gender) + factor(ageyears) + factor(diarrhoea) + 
         factor(religion) + factor(waz) + factor(whz) + factor(urbanrural) + factor(tfacility) + factor(swater) + factor(healthcard) + 
         factor(border) + lstmean + motherheightcm + motherage + precipitation_mean, design = DHSdesign2000, family = "binomial")
summary(adj3_2000)
exp(coefficients(adj3_2000))
regTermTest(adj3_2000,"factor(ethnic)")

# Adjusted model 3b (2005 survey year only)
comp_data2005 <- comp_data %>% filter(surveyyear == "2005")
DHSdesign2005 <- comp_data2005 %>% 
  svydesign(id = comp_data2005$unique_id, 
            strata = comp_data2005$urbanrural, 
            probs = NULL)
adj3_2005 <- svyglm(haz ~ factor(ethnic) + factor(edhigh) + factor(gender) + factor(ageyears) + factor(diarrhoea) + 
                      factor(religion) + factor(waz) + factor(whz) + factor(urbanrural) + factor(tfacility) + factor(swater) + factor(healthcard) + 
                      factor(border) + lstmean + motherheightcm + motherage + precipitation_mean, design = DHSdesign2005, family = "binomial")
summary(adj3_2005)
exp(coefficients(adj3_2005))
regTermTest(adj3_2005,"factor(ethnic)")
        
# Adjusted model 3c (2011 survey year only)
comp_data2011 <- comp_data %>% filter(surveyyear == "2011")
DHSdesign2011 <- comp_data2011 %>% 
  svydesign(id = comp_data2011$unique_id, 
            strata = comp_data2011$urbanrural, 
            probs = NULL)
adj3_2011 <- svyglm(haz ~ factor(ethnic) + factor(edhigh) + factor(gender) + factor(ageyears) + factor(diarrhoea) + 
                      factor(religion) + factor(waz) + factor(whz) + factor(urbanrural) + factor(tfacility) + factor(swater) + factor(healthcard) + 
                      factor(border) + lstmean + motherheightcm + motherage + precipitation_mean, design = DHSdesign2011, family = "binomial")
summary(adj3_2011)
exp(coefficients(adj3_2011))
regTermTest(adj3_2011,"factor(ethnic)")

# Adjusted model 3d (2016 survey year only)
comp_data2016 <- comp_data %>% filter(surveyyear == "2016")
DHSdesign2016 <- comp_data2016 %>% 
  svydesign(id = comp_data2016$unique_id, 
            strata = comp_data2016$urbanrural, 
            probs = NULL)
adj3_2016 <- svyglm(haz ~ factor(ethnic) + factor(edhigh) + factor(gender) + factor(ageyears) + factor(diarrhoea) + 
                      factor(religion) + factor(waz) + factor(whz) + factor(urbanrural) + factor(tfacility) + factor(swater) + factor(healthcard) + 
                      factor(border) + lstmean + motherheightcm + motherage + precipitation_mean, design = DHSdesign2016, family = "binomial")
summary(adj3_2016)
exp(coefficients(adj3_2016))
regTermTest(adj3_2016,"factor(ethnic)")
