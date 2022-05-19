#------------------------------------------------------------------------------#
# Name : mortality_functions.R                                                 #
# Description : Calculates mortality rate for each sex and their ratio         #
# Author : Pietro Violo                                                        #
# Date : May 15 2022                                                           #
#------------------------------------------------------------------------------#

library(covidAgeData)
library(lubridate)
library(stats)

#'* Calculate deaths for the first of every month.*

# All code is in a temporary function in order to not import everything
# when called by Source in the main script file

function(temporary){
  # Download data
  data <- download_covid(data = "Output_5")
  
  # filter for United States only
  df <- data %>% filter(Country == "USA")
  
  # Transform to date
  df <- df %>% mutate(Date = as.Date(Date, tryFormats = c("%d.%m.%Y")))
  
  # Sum up groups that are over 85+, because we only have 85+ population groups
  # in Census data
  
  df <- df %>% mutate(Age = ifelse(Age >=85, 85, Age)) %>% 
    group_by(Region, Date, Sex, Age) %>% 
    summarise(Deaths = sum(Deaths)) %>% 
    ungroup()
  
  # keep males and females
  
  df <- df %>% filter(Sex != "b")
  
  # Now, we have various dates (but not all) available, we need to find the
  # earliest and dates date available for each month and each state/age group
  
  df <- df %>%
    dplyr::mutate(year = lubridate::year(Date), 
                  month = lubridate::month(Date), 
                  day = lubridate::day(Date))
  
  # earliest day for each month
  df_min_dates <- df %>% 
    group_by(Region,Sex,Age,year,month) %>% 
    summarise(day = min(day))
  
  df_min_dates <- left_join(df_min_dates,df, by = c("Region", "Sex","Age","year","month","day"))
  
  # Difference in days and deaths between two dates
  # Number of days until first of each month
  
  df_min_dates <-df_min_dates %>% ungroup() %>% 
    group_by("Region","Sex","Age") %>% 
    mutate(days_between = (interval(Date, dplyr::lead(Date)) %/% days(1)),
           deaths_between = dplyr::lead(Deaths)-Deaths,
           days_before_first = (interval(Date, floor_date(dplyr::lead(Date),"month"))) %/% days(1),
           Deaths_first = Deaths + deaths_between/days_between * days_before_first) 
  
  # Deaths on the first of the month
  # First dates available for every group has taken the value of the last 85+, we
  # must eliminate that discrepancy.
  
  df_first_month <- df_min_dates %>% mutate(Deaths = dplyr::lag(Deaths_first),
                                            day = 1) %>% ungroup() %>% 
    select(Region, Sex, Age, year, month, day, Deaths) %>% 
    mutate(Deaths = ifelse((Age == 0) & (Deaths > 220), NA, Deaths)) %>% 
    na.omit() %>% 
    filter(Deaths >= 0) %>% 
    mutate(date = as.Date(paste(year, month, day, sep="/"), format = "%Y/%m/%d"))
  
  save(df_first_month, file = "./Data/df_first_month.RData")
  
  
}



#'* Function to calculate daily male and female mortality ratios, and their ratio.*

mortality_function <- function(df) {
  
  # POPULATION DATA
  # Combine single age into the same groups as our COVID-19 deaths age group
  # 2019 population
  
  df_pop <- read.csv("./Data/USpop.csv") %>% select(NAME, SEX, AGE,POPEST2019_CIV) %>% 
    mutate(Age=case_when(AGE%in%0:4~0L,
                         AGE%in%5:9~5L,
                         AGE%in%10:14~10L,
                         AGE%in%15:19~15L,
                         AGE%in%20:24~20L,
                         AGE%in%25:29~25L,
                         AGE%in%30:34~30L,
                         AGE%in%35:39~35L,
                         AGE%in%40:44~40L,
                         AGE%in%45:49~45L,
                         AGE%in%50:54~50L,
                         AGE%in%55:59~55L,
                         AGE%in%60:64~60L,
                         AGE%in%65:69~65L,
                         AGE%in%70:74~70L,
                         AGE%in%75:79~75L,
                         AGE%in%80:84~80L,
                         AGE%in%85:999~85L),
           Sex=case_when(SEX==0~"b",
                         SEX==1~"m",
                         SEX==2~"f")) %>% 
    group_by(NAME,Sex,Age) %>% 
    summarize(Pop=sum(POPEST2019_CIV),.groups="drop") %>% 
    select(Region=NAME,Sex,Age,Pop)
  
  df_mort <- left_join(df,df_pop, by = c("Region", "Sex", "Age")) 
  
  # Average population exposition per day
  
  return(df_mort)
  
}



