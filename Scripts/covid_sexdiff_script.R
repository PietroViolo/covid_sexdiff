#------------------------------------------------------------------------------#
# Name : covid_sexdiff_script.R                                                #
# Description : Standardize and compare mortality rates for COVID-19           #
# and other diseases, as well as calculating and plotting their male-to-female #
# ratios                                                                       #
# Author : Pietro Violo                                                        #
# Date : May 15 2022                                                           #
#------------------------------------------------------------------------------#

# Libraries
library(ggthemes)
library(mortalitySmooth)

# Import data
covid_data <- read.csv("./Data/Provisional_COVID-19_Deaths_by_Sex_and_Age.csv")

