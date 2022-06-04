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
  mutate(Deaths_per_month = dplyr::lead(Deaths_linint) - Deaths_linint, # Calculate number of deaths per month
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

excess_male_mort <- pivot_wider(df_mort %>%  select(Region, Sex, Age, Date, monthly_mort_rate), 
            names_from = Sex, values_from = monthly_mort_rate) %>% mutate(excess_male = m/f)


# male-excess mortality with mono deaths

excess_male_mort_mono <- pivot_wider(df_mort %>%  select(Region, Sex, Age, Date, monthly_mono_mort_rate), 
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


png(file = paste("./Graphs/GGridges/USA_ages.png"), res = 300, width = 4400, height = 3500)

excess_male_mort %>% # Ridge lines plot
  filter(Region == "United States" &
           !(is.infinite(excess_male))) %>%  ggplot(aes( x = excess_male, y = Age, fill = Region))+ 
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

dev.off()
  


#'* Evolution of mortality ratio, for US as a whole *

png(file = paste("./Graphs/Trends/USA_trends.png"), res = 300, width = 4400, height = 3500)

excess_male_mort %>% filter(!(is.infinite(excess_male)),
                            Region == "United States") %>% na.omit() %>% 
  ggplot(aes(x = Date, y = excess_male, color = Age)) +
  geom_line() + 
  scale_y_log10(limits = c(0.5,4))

excess_male_mort %>% filter(!(is.infinite(excess_male)),
                            Region == "United States",
                            Age %in% c("30-34",
                                       "35-39",
                                       "40-44",
                                       "45-49",
                                       "50-54",
                                       "55-59",
                                       "60-64",
                                       "65-69",
                                       "70-74",
                                       "75-79",
                                       "80-84",
                                       "85 +"))%>% 
  ggplot(aes(x = Date, y = excess_male, color = Age)) +
  geom_line() + 
  scale_y_log10(limits = c(0.5,4))

dev.off()


#'* Lexis surfaces of mortality ratio *

# January and may missing for Oregon

states <- excess_male_mort %>% pull(Region) %>% unique()

for(state in states){
  
  test <- excess_male_mort %>% filter(Region == state)
  
  png(file = paste("./Graphs/",state,"_excess.png", sep = ""), res = 300, width = 3000, height = 2600)
  
  print(test %>% ggplot(aes(x = Date, y = Age)) +
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



#'* Median mortality ratios on the map for each age group*

#devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)


median_mort <- excess_male_mort %>% summarise(median = median(excess_male, na.rm = T))

ages <-          c("30-34",
                   "35-39",
                   "40-44",
                   "45-49",
                   "50-54",
                   "55-59",
                   "60-64",
                   "65-69",
                   "70-74",
                   "75-79",
                   "80-84",
                   "85 +")

median_mort <- median_mort %>% filter(Age %in% ages) %>% 
  pivot_wider(names_from = Age, values_from = median) %>% 
  rename(state_name = Region)


median_mort <- left_join(median_mort,urbnmapr::states, by = 'state_name')


for(age.group in ages){
  
  png(file = paste("./Graphs/Maps/",age.group,"_map.png",sep = ""), res = 300, width = 4400, height = 2600)
  
  print(ggplot(data = median_mort,
         aes(x = long, y = lat, group = group, fill = get(age.group))) +
    geom_polygon() +
    ggtitle(paste("Median mortality ratio for COVID-19 for the ",age.group," age group", sep = ""))+
    coord_map(projection = "albers", lat = 45, lat1 = 55) +
    theme(legend.position="bottom",
          axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          plot.background = element_rect(fill = "#f5f5f2", color = NA),
          panel.background = element_rect(fill = "#f5f5f2", color = NA), 
          legend.background = element_rect(fill = "#f5f5f2", color = NA),
          panel.border=element_blank(),
          panel.grid=element_blank(),
          plot.title = element_text(size= 18, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm"))) +
    scale_fill_continuous_diverging(trans = "log",
                                    breaks = c(0.5,1,2,4),
                                    limits = c(0.5,4),
                                    palette = "Purple-Green",
                                    name = "COVID-19 mortality ratio"))
  dev.off()
      
}








