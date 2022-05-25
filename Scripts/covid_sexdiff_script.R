#------------------------------------------------------------------------------#
# Name : covid_sexdiff_script.R                                                #
# Description : Standardize and compare mortality rates for COVID-19           #
# and other diseases, as well as calculating and plotting their male-to-female #
# ratios                                                                       #
# Author : Pietro Violo                                                        #
# Date : May 15 2022                                                           #
#------------------------------------------------------------------------------#

#remotes::install_github("eshom/covid-age-data")

# Libraries
library(tidyverse)
library(ggthemes)
library(remotes)
library(lubridate)
library(viridis)

# Data
load("./Data/df_first_month.RData")


#'* Function to attach population at risk for a given subgroup*
# We are working with cumulative data.

source("./Scripts/mortality_functions.R")

# Calculate numbers of deaths that have occurred during the month

df_monthly_deaths <- joined %>% group_by(Region, Sex, Age) %>% 
  mutate(Deaths_per_month = dplyr::lead(Deaths) - Deaths,
         Deaths_per_month_mono = ifelse(c(0,diff(Deaths_mono))<0,0, diff(Deaths_mono))) %>% 
  na.omit()


# Mortality rate : attach population
df_mort <- mortality_function(df_monthly_deaths)

# Negative means 0 deaths

df_mort <- df_mort %>% mutate(Deaths_per_month = ifelse(Deaths_per_month <= 0, 0, Deaths_per_month),
                   Deaths_per_month_mono = ifelse(Deaths_per_month_mono <= 0, 0, Deaths_per_month_mono))


df_mort <- df_mort %>% mutate(monthly_mort_rate = Deaths_per_month/(Pop/12),
                              monthly_mono_mort_rate = Deaths_per_month_mono/(Pop/12))

# pivot wider for males and females on the same line to calculate
# male-excess mortality

excess_male_mort <- pivot_wider(df_mort %>%  select(Region, Sex, Age, date, monthly_mort_rate), 
            names_from = Sex, values_from = monthly_mort_rate)

excess_male_mort <- excess_male_mort %>% mutate(excess_male = m/f)

excess_male_mort %>% ggplot(aes(x = excess_male, color = as.character(Age))) + 
  geom_histogram(binwidth = 0.1, fill = "white", position = "dodge") + 
  xlim(c(0,7)) 

# male-excess mortality with mono deaths

excess_male_mort <- pivot_wider(df_mort %>%  select(Region, Sex, Age, date, monthly_mono_mort_rate), 
                                names_from = Sex, values_from = monthly_mono_mort_rate)

excess_male_mort <- excess_male_mort %>% mutate(excess_male = m/f)

excess_male_mort %>% ggplot(aes(x = excess_male, color = as.character(Age))) + 
  geom_histogram(binwidth = 0.1, fill = "white", position = "dodge") + 
  xlim(c(0,7)) 




#'* Lexis surfaces of mortality ratio *

# January and may missing for Oregon

states <- excess_male_mort %>% pull(Region) %>% unique()

for(state in states){
  
  test <- excess_male_mort %>% filter(Region == state)
  
  png(file = paste("./Graphs/",state,"_excess.png", sep = ""), res = 300, width = 3000, height = 2600)
  
  print(test %>% ggplot(aes(x = date, y = Age)) +
    geom_tile(aes(fill = excess_male), width = 31) +
    scale_fill_viridis(trans="log",
                       breaks=c(0.5,1,2,4),
                       limits=c(0.5,4),
                       name = "Male excess mortality") +
    coord_cartesian(ylim = c(30, 85),
                    xlim = c(as.Date("2021-01-01"),as.Date("2022-01-01"))) +
    theme_light() +
    labs(x = "Month",
         y = "Age group",
         title = paste("Male excess mortality by month, ",state,", 2021-2022", sep = "")))
  
  dev.off()
  
}






