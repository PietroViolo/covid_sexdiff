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
library(colorspace)
library(ggridges)

# Data
load("./Data/df_first_month.RData")


#'* Function to attach population at risk for a given subgroup*
# We are working with cumulative data.

source("./Scripts/mortality_functions.R")

# Calculate numbers of deaths that have occurred during the month

df_monthly_deaths <- joined %>% 
  group_by(Region, Sex, Age) %>% 
  mutate(Deaths_per_month = dplyr::lead(Deaths) - Deaths, # Calculate number of deaths per month
         Deaths_per_month_mono = ifelse(c(0,diff(Deaths_mono))<0,0, diff(Deaths_mono)), # Same calculation, but for monotonic splines
         Region = ifelse(Region=="All", "United States", Region))  #Change "All" name to allow joining
  


# Mortality rate : attach population
df_mort <- mortality_function(df_monthly_deaths)

# Negative deaths are neglible, we can round to 0

df_mort <- df_mort %>% mutate(Deaths_per_month = ifelse(Deaths_per_month <= 0, 0, Deaths_per_month),
                   Deaths_per_month_mono = ifelse(Deaths_per_month_mono <= 0, 0, Deaths_per_month_mono)) %>% 
  mutate(monthly_mort_rate = Deaths_per_month/(Pop/12), # Deaths divided by estimated monthly population is the monthly mortality rate
         monthly_mono_mort_rate = Deaths_per_month_mono/(Pop/12))

# pivot wider for males and females on the same line to calculate
# male-excess mortality

excess_male_mort <- pivot_wider(df_mort %>%  select(Region, Sex, Age, date, monthly_mort_rate), 
            names_from = Sex, values_from = monthly_mort_rate) %>% mutate(excess_male = m/f)


# male-excess mortality with mono deaths

excess_male_mort_mono <- pivot_wider(df_mort %>%  select(Region, Sex, Age, date, monthly_mono_mort_rate), 
                                names_from = Sex, values_from = monthly_mono_mort_rate) %>% mutate(excess_male = m/f)



#'* GGRidges, for US as a whole *

excess_male_mort <- excess_male_mort %>%              # Change age groups id
  mutate(Age = case_when(Age == 0 ~ "0-4",
                         Age == 5 ~ "5-9",
                         Age == 10 ~ "10-14",
                         Age == 15 ~ "15-19",
                         Age == 20 ~ "20-24",
                         Age == 25 ~ "25-29",
                         Age == 30 ~ "30-34",
                         Age == 35 ~ "35-39",
                         Age == 40 ~ "40-44",
                         Age == 45 ~ "45-49",
                         Age == 50 ~ "50-54",
                         Age == 55 ~ "55-59",
                         Age == 60 ~ "60-64",
                         Age == 65 ~ "65-69",
                         Age == 70 ~ "70-74",
                         Age == 75 ~ "75-79",
                         Age == 80 ~ "80-84",
                         Age == 85 ~ "85 +"))

order <- excess_male_mort %>% pull(Age) %>% unique()

excess_male_mort <- excess_male_mort %>% mutate(Age = factor(Age, levels = order))


excess_male_mort %>% # Ridge lines plot
  filter(Region == "United States") %>% ggplot(aes( x = excess_male, y = Age, fill = Region))+ 
  geom_density_ridges(alpha = 0.6,
                      quantile_lines = TRUE,
                      quantile_fun = function(x,...)mean(x))+
  theme_ridges(font_size = 12, grid = TRUE) +
  theme_fivethirtyeight() + 
  xlim(c(0.5,4)) + 
  labs (title = "Male-to-female ratios of COVID-19 for the United States",
                           x = "Male-to-female mortality ratio",
                           y = "Age groups") + 
  geom_vline(xintercept = 1, color = "white")
  


#'* Evolution of mortality ratio, for US as a whole *

excess_male_mort %>% filter(!(is.infinite(excess_male)),
                            Region == "United States") %>% na.omit() %>% 
  ggplot(aes(x = date, y = excess_male, color = Age)) +
  geom_line() + 
  scale_y_log10(limits = c(0.5,4))

excess_male_mort %>% filter(!(is.infinite(excess_male)),
                            Region == "United States",
                            Age %in% c("30-34",
                                       "35-39",
                                       "40-44",
                                       "45-49",
                                       "50-54",
                                       Age == 55 ~ "55-59",
                                       Age == 60 ~ "60-64",
                                       Age == 65 ~ "65-69",
                                       Age == 70 ~ "70-74",
                                       Age == 75 ~ "75-79",
                                       Age == 80 ~ "80-84",
                                       Age == 85 ~ "85 +")) %>% na.omit() %>% 
  ggplot(aes(x = date, y = excess_male, color = Age)) +
  geom_line() + 
  scale_y_log10(limits = c(0.5,4))


#'* Lexis surfaces of mortality ratio *

# January and may missing for Oregon

states <- excess_male_mort %>% pull(Region) %>% unique()


for(state in states){
  
  test <- excess_male_mort %>% filter(Region == state)
  
  png(file = paste("./Graphs/",state,"_excess.png", sep = ""), res = 300, width = 3000, height = 2600)
  
  print(test %>% ggplot(aes(x = date, y = Age)) +
    geom_tile(aes(fill = excess_male), width = 31)  +
    coord_cartesian(ylim = c(30, 85),
                    xlim = c(as.Date("2021-01-01"),as.Date("2022-01-01"))) +
    theme_light() +
    labs(x = "Month",
         y = "Age group",
         title = paste("COVID-19 mortality ratio by month, ",state,", 2021-2022", sep = "")) +
    scale_fill_continuous_diverging(trans = "log",
                                    breaks = c(0.5,1,2,4),
                                    limits = c(0.5,4),
                                    palette = "Purple-Green",
                                    name = "COVID-19 mortality ratio"))
  
  dev.off()
  
}






