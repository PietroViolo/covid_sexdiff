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
library(cowplot)

# Data
load("./Data/df_first_month.RData")

source("./Scripts/mortality_functions.R")

ages <- c("30-34","35-39","40-44","45-49","50-54","55-59", "60-64","65-69","70-74","75-79", "80-84","85 +")

# Evolution of mortality rate and total deaths

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




# CDC all deaths
cdc_deaths <- read.csv("./Data/Provisional_COVID-19_Deaths_by_Sex_and_Age.csv") %>% 
  filter(State == "United States", 
         Sex == "All Sexes",
         Age.Group == "All Ages",
         Group == "By Month") %>% 
  mutate(Date = as.Date(paste(Year, "-", Month, "-01", sep = "")))

covid_deaths <- cdc_deaths %>% ggplot(aes(x = Date, y = COVID.19.Deaths)) +
  geom_line()+
  xlim(as.Date(c("2020-01-01", "2022-06-01")))


for(age_group in ages){
  
  x <- excess_male_mort %>% filter(Region != "United States",
                                 Age == age_group) %>% 
  ggplot(aes(x = Date, y = excess_male, group = Region)) +
  geom_line(color = "gray53") + geom_line(data = excess_male_mort %>% filter(Region == "United States",
                                                             Age == age_group), 
                          aes(x = Date, y = excess_male), color = "red") +
  scale_y_log10(limits = c(0.5, 4)) +
  xlim(as.Date(c("2020-01-01", "2022-06-01"))) +
  theme(legend.position = "bottom") +
  labs(x = "Date", y = "Male-to-female mortality ratio")
  
  assign(paste("excess_",age_group,sep =""), x)
  
}


plot_grid(`excess_30-34`,
          `excess_35-39`,
          `excess_40-44`,
          `excess_45-49`,
          `excess_50-54`,
          `excess_55-59`,
          `excess_60-64`,
          `excess_65-69`,
          `excess_70-74`,
          `excess_75-79`,
          `excess_80-84`,
          `excess_85 +`
          ,ncol = 1, align = "v")










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
  
  









