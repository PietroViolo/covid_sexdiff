#------------------------------------------------------------------------------#
# Name : EPC2022.R                                                             #
# Description : For the EPC presentation                                       #
# Author : Pietro Violo                                                        #
# Date : June 21 2022                                                          #
#------------------------------------------------------------------------------#

#remotes::install_github("eshom/covid-age-data")

# Libraries
library(tidyverse)
library(ggthemes)
library(remotes)
library(lubridate)
library(viridis)
library(colorspace)
library(ggridges)
library(plotly)
library(rayshader)

# Data
load("./Data/df_first_month.RData")

#'* Function to attach population at risk for a given subgroup*
# We are working with cumulative data.

source("./Scripts/mortality_functions.R")

