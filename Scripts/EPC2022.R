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

source("./Scripts/mortality_functions.R")

df_monthly_deaths <- joined %>% 
  group_by(Region, Sex, Age) %>% 
  select(-diff) %>% 
  mutate(Deaths_per_month = dplyr::lead(Deaths_linint) - Deaths_linint, # Calculate number of deaths per month
         Deaths_per_month_mono = ifelse(c(0,diff(Deaths_mono))<0,0, diff(Deaths_mono)), # Same calculation, but for monotonic splines
         Region = ifelse(Region=="All", "United States", Region), #Change "All" name to allow joining
         Region = ifelse(Region == "New York City", "New York", Region)) %>%  # Change New York City to New York to combine
  group_by(Region, Sex, Age, Date) %>% 
  summarise(Deaths_per_month = sum(Deaths_per_month, na.rm = T)) %>% 
  ungroup()

# Mortality rate : attach population
df_mort <- mortality_function(df_monthly_deaths) %>% 
  mutate(Deaths_per_month = ifelse(Deaths_per_month <= 0, 0, Deaths_per_month))%>% 
  mutate(monthly_mort_rate = Deaths_per_month/(Pop/12)) # Deaths divided by estimated monthly population is the monthly mortality rate

# pivot wider for males and females on the same line to calculate
# male-excess mortality

excess_male_mort <- pivot_wider(df_mort %>%  select(Region, Sex, Age, Date, monthly_mort_rate), 
                                names_from = Sex, values_from = monthly_mort_rate) %>% mutate(excess_male = m/f)




# First graph : Males die more than females for all-cause deaths in 2019, in comparison with COVID-19

Chapter_age <-read.table("Data/Chapter_age.txt",header=TRUE, sep="\t") %>% 
  filter(ICD.Chapter %in% c("Diseases of the circulatory system","Diseases of the respiratory system","Diseases of the nervous system")) %>%  
  select(State,Five.Year.Age.Groups.Code,Gender,ICD.Chapter,Deaths,Population) %>% 
  `colnames<-` (c("State","Age","Sex","Cause","Deaths","Pop")) 

Subchapter_age <-read.table("Data/Subchapter_age.txt",header=TRUE, sep="\t") %>%
  filter(ICD.Sub.Chapter %in% c("Diabetes mellitus","Influenza and pneumonia","Malignant neoplasms")) %>% 
  select(State,Five.Year.Age.Groups.Code,Gender,ICD.Sub.Chapter,Deaths,Population) %>% 
  `colnames<-` (c("State","Age","Sex","Cause","Deaths","Pop")) 

Causes <- tbl_df(rbind(Chapter_age,Subchapter_age)) %>% 
  mutate(Deaths=as.integer(Deaths), Pop=as.integer(Pop)) %>% 
  filter(Cause!="Diseases of the respiratory system") %>% 
  rbind(Chapter_age %>% 
          subset(Cause=="Diseases of the respiratory system") %>% 
          cbind(Subchapter_age %>% 
                  subset(Cause == "Influenza and pneumonia") %>% 
                  select(Deaths))%>% 
          `colnames<-` (c("State","Age","Sex","Cause","Deaths.resp","Pop","Deaths.influenza")) %>% 
          mutate(Deaths.resp=as.double(Deaths.resp),
                 Deaths.resp=replace_na(Deaths.resp,0),
                 Deaths.influenza=as.double(Deaths.influenza),
                 Deaths.influenza=replace_na(Deaths.influenza,0),
                 Deaths=Deaths.resp-Deaths.influenza) %>%  
          select(State,Age,Sex,Cause,Pop,Deaths) %>% 
          relocate(Pop,.after=Deaths)) %>% 
  mutate(Pop=as.double(Pop),Rate=Deaths/Pop) %>% 
  arrange(by=Sex) %>% 
  select(c("State","Age","Sex","Cause","Rate")) %>% 
  pivot_wider(names_from="Sex",values_from="Rate") %>%
  mutate(ratio = Male/Female) %>% 
  mutate("Region" = case_when(State %in% midwest ~ "Midwest",
                              State %in% northeast ~ "Northeast",
                              State %in% south ~ "South",
                              State %in% west ~ "West" ))
  
  









