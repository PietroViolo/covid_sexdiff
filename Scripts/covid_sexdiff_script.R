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
library(mortalitySmooth)
library(remotes)
library(covidAgeData)

# Download data
df <- download_covid(data = "Output_5")

# filter for United States only
df <- df %>% filter(Country == "USA")

# Transform to date
df <- df %>% mutate(Date = as.Date(Date, tryFormats = c("%d.%m.%Y")))

# Sum up groups that are over 85+, because we only have 85+ population groups
# in Census data

df <- df %>% mutate(Age = ifelse(Age >=85, 85, Age)) %>% 
  group_by(Region, Date, Sex, Age) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup()



#'* Function to calculate daily male and female mortality ratios, and their ratio.*

source("./Scripts/mortality_functions.R")

mortality_function(df)

