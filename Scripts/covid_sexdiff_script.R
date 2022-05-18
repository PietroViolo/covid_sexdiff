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










#'* Function to calculate daily male and female mortality ratios, and their ratio.*
# We are working with cumulative data.

source("./Scripts/mortality_functions.R")

df_mort <- mortality_function(df)

df_mort %>% arrange(Date)

splinefun(, method = "monoH.FC")



#'* Group by month and sum up all mortality rates to obtain monthly mortality rate*

# df_mort_month <- df_mort %>% 
#   group_by(month = lubridate::floor_date(Date, "month"),
#            Region,
#            Age) %>%
#   summarise(m = sum(m, na.rm = T),
#             f = sum(f, na.rm = T))
