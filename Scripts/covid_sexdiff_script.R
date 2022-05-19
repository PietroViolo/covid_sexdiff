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

# Data
load("./Data/df_first_month.RData")


#'* Function to attach population at risk for a given subgroup*
# We are working with cumulative data.

source("./Scripts/mortality_functions.R")

# Calculate numbers of deaths that have occurred during the month

df_monthly_deaths <- df_first_month %>% group_by(Region, Sex, Age) %>% 
  mutate(Deaths_per_month = dplyr::lead(Deaths) - Deaths)

df_monthly_deaths <- cbind(df_first_month,df_monthly_deaths$Deaths_per_month) 

# Mortality rate : attach population
df_mort <- mortality_function(df_monthly_deaths)


df_mort <- df_mort %>% mutate(monthly_mort_rate = Deaths_per_month/(Pop/12)) %>% 
  select(-Deaths_per_month,-Pop)

# pivot wider for males and females on the same line to calculate
# male-excess mortality
pivot_wider(df_mort, names_from = Sex, values_from = monthly_mort_rate)
