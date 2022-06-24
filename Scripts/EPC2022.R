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

# Let's try with provisional counts and put them in the same format as joined

joined2 <- read.csv("./Data/Provisional_COVID-19_Deaths_by_Sex_and_Age.csv") %>% 
  filter(Group == "By Month",
         Sex != "All Sexes") %>% 
  mutate(COVID.19.Deaths = ifelse(is.na(COVID.19.Deaths), 5, COVID.19.Deaths))

ages_10 <- (joined2 %>% pull(Age.Group) %>% unique())[c(2,4,5,6,8,10,12,14,15:17)]

joined2 <- joined2 %>% filter(Age.Group %in% ages_10) %>% select(Year,
                                                                 Month, 
                                                                 State, 
                                                                 Sex, 
                                                                 Age.Group,
                                                                 COVID.19.Deaths) %>% 
  rename(Region = State,
         Age = Age.Group,
         Covid_deaths = COVID.19.Deaths) %>% 
  mutate(Date = paste(Year,"-", Month,"-01", sep = ""),
         Date = as.Date(Date))


uspop <- read.csv("./Data/USpop.csv") %>% select(NAME, SEX, POPEST2019_CIV, AGE) %>% 
  mutate(AGE=case_when(AGE==0~"Under 1 year",
                       AGE%in%1:4~"1-4 years",
                       AGE%in%5:14~"5-14 years",
                       AGE%in%15:24~"15-24 years",
                       AGE%in%25:34~"25-34 years",
                       AGE%in%35:44~"35-44 years",
                       AGE%in%45:54~"45-54 years",
                       AGE%in%55:64~"55-64 years",
                       AGE%in%65:74~"65-74 years",
                       AGE%in%75:84~"75-84 years",
                       AGE%in%85:998~"85 years and over",
                       AGE==999~"All Ages"),
         SEX=case_when(SEX==0~"All Sexes",
                       SEX==1~"Male",
                       SEX==2~"Female")) %>% 
  rename(Region = NAME,
         Sex = SEX,
         Population_2019 = POPEST2019_CIV,
         Age = AGE,
         ) %>% 
  filter(Age %in% ages_10) %>% group_by(Region, Sex, Age) %>% 
  summarise(Population_2019 = sum(Population_2019)) %>% ungroup()

# combine NYC and new york in Covid data

joined2 <- joined2 %>% 
  mutate(Region = ifelse(Region == "New York City", "New York", Region)) %>%
  group_by(Region, Sex, Age, Date) %>% 
  summarise( Covid_deaths = sum(Covid_deaths, na.rm = T))

# join

joined2 <- left_join(joined2, uspop)

joined = joined2 %>% 
  filter(Region != "Puerto Rico")


# From joined, combine New York City and New York and remove Puerto Rico

joined <- joined %>% 
  mutate(Region = ifelse(Region=="All", "United States", Region), #Change "All" name to allow joining
         Region = ifelse(Region == "New York City", "New York", Region)) %>%  # Change New York City to New York to combine
  group_by(Region, Sex, Age, Date) %>% 
  summarise( Covid_deaths = sum(Deaths_linint, na.rm = T)) %>% 
  filter(Region != "Puerto Rico") 


ages <- c("30-34","35-39","40-44","45-49","50-54","55-59", "60-64","65-69","70-74","75-79", "80-84","85 +")



#'* Mortality ratios per state, by age group *
#  Total mortality rate

days_between = interval(min(joined$Date),max(joined$Date)) %/% days(1)

# There are 791 days for total deaths, so we need to divide the total deaths by 791
# then multiply by 365 to obtain yearly mortality rate

# With CDC Data
Cumulative_deaths <- joined %>% group_by(Region, Sex, Age, Population_2019) %>% 
  summarise(Covid_deaths = sum(Covid_deaths, na.rm = T)) %>% 
  mutate(yearly_death_rate = Covid_deaths/(Population_2019/days_between*365) * 1000)


# With Coverage Data

Cumulative_deaths <- mortality_function(joined %>% filter(Date == max(joined$Date))) %>% 
  mutate(yearly_death_rate = Covid_deaths/(Pop/days_between*365) * 1000) 

Cumulative_deaths <- Cumulative_deaths %>%              # Change age groups id
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


# Resume

order <- Cumulative_deaths %>% filter(Sex == "Male",
                                      Age == "85 years and over") %>%
  arrange(desc(yearly_death_rate)) %>% pull(Region) %>% as.character()

Cumulative_deaths <- Cumulative_deaths %>% mutate(Region = factor(Region, levels = order))

# For COVERAGE data
for(age_group in ages){
  
  x <- Cumulative_deaths %>% filter(Age== age_group) %>% ggplot(aes( x= Region, y = yearly_death_rate, color = Sex)) +
    geom_point() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+ labs(y = age_group) + 
    scale_y_log10(limits = c(0.05,150))
  
  assign(paste("plot_",age_group, sep = ""), x)
  
  
}

# For CDC data
for(age_group in ages_10){
  
  x <- Cumulative_deaths %>% filter(Age== age_group) %>% ggplot(aes( x= Region, y = yearly_death_rate, color = Sex)) +
    geom_point() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+ labs(y = age_group) + 
    scale_y_log10(limits = c(0.05,200))
  
  assign(paste("plot_",age_group, sep = ""), x)
  
}

axis = Cumulative_deaths %>% filter(Age=="1-4 years") %>% 
  ggplot(aes(x= Region, y = yearly_death_rate, color = Sex)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(panel.ontop = TRUE,
        axis.ticks.y=element_blank())+ labs(y = "   ")

tiff("./EPC 2022/Graphs/general_covidratios.tiff", width = 3300, height = 6000, res=500)

ggpubr::ggarrange(`plot_30-34`,
                  `plot_35-39`,
                  `plot_40-44`,
                  `plot_45-49`,
                  `plot_50-54`,
                  `plot_55-59`,
                  `plot_60-64`,
                  `plot_65-69`,
                  `plot_70-74`,
                  `plot_75-79`,
                  `plot_80-84`,
                  `plot_85 +`,
                  axis,
                  common.legend = T,
                  ncol = 1)

dev.off()

tiff("./EPC 2022/Graphs/general_covidratios_CDC.tiff", width = 3300, height = 6000, res=500)

ggpubr::ggarrange(`plot_25-34 years`,
                  `plot_35-44 years`,
                  `plot_45-54 years`,
                  `plot_55-64 years`,
                  `plot_65-74 years`,
                  `plot_75-84 years`,
                  `plot_85 years and over`,
                  axis,
                  common.legend = T,
                  ncol = 1)

dev.off()







#'* Mortality ratio trends per month*

df_monthly_deaths <- joined

df_monthly_deaths <- joined %>% 
  group_by(Region, Sex, Age) %>% 
  mutate(Deaths_per_month = dplyr::lead(Covid_deaths) - Covid_deaths)

# Mortality rate : attach population
df_mort <- mortality_function(df_monthly_deaths) %>% 
  mutate(Deaths_per_month = ifelse(Deaths_per_month <= 0, 0, Deaths_per_month))%>% 
  mutate(monthly_mort_rate = Deaths_per_month/(Pop/12)) # Deaths divided by estimated monthly population is the monthly mortality rate

# pivot wider for males and females on the same line to calculate
# male-excess mortality

excess_male_mort <- pivot_wider(df_mort %>%  
                                  select(Region, Sex, Age, Date, monthly_mort_rate), 
                                names_from = Sex, values_from = monthly_mort_rate) %>%
  mutate(excess_male = m/f)

# With CDC Data

excess_male_mort <- pivot_wider(df_monthly_deaths %>%  mutate(monthly_mort_rate = Covid_deaths/(Population_2019/12)) %>% 
                                  select(Region, Sex, Age, Date, monthly_mort_rate), 
                                names_from = Sex, values_from = monthly_mort_rate) %>%
  mutate(excess_male = Male/Female)




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








#'* GGRidges, for US as a whole *


order <- excess_male_mort %>% pull(Age) %>% unique()
order <- ages_10[5:11]

excess_male_mort <- excess_male_mort %>% mutate(Age = factor(Age, levels = order)) %>% 
  filter(Age %in% order)

tiff("./EPC 2022/Graphs/ridge_lines_CDC.tiff", width = 3300, height = 6000, res=500)

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
  geom_vline(xintercept = 1, color = "black")

dev.off()



#'* Spaghetti plot evolution with number of total deaths *


# Spaghetti plot tracking the evolution of the ratio
library(gganimate)
library(gifski)
library(transformr)

# install.packages("devtools")
devtools::install_github("thomasp85/transformr")


excess_male_mort %>% filter(Age %in% ages) %>% 
  ggplot(aes(x = Age, y = excess_male, group = Date)) +
  geom_point() +
  scale_y_log10(limits = c(0.5,4)) +
  scale_color_viridis(option="rocket") + 
  geom_smooth()+
  transition_time(Date) +
  labs(title = "Male-to-female mortality ratio for the month of : {frame_time}",
       y = "Male-to-female mortality ratio")


excess_male_mort %>% pull(Date) %>% unique()

anim_save("./EPC 2022/Graphs/spaghetti.gif")

x <- excess_male_mort %>% group_by(Date, Age) %>% summarise(n = n())


# All states

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


png(file = paste("./Graphs/EPC2022/sp_total.png"), res = 300, width = 3000, height = 9000)

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

dev.off()



#'* All deaths combined *

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



#'* Other causes of death *
#'
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
  
  







