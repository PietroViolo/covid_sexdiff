#------------------------------------------------------------------------------#
# Name : mortality_functions.R                                                 #
# Description : Calculates mortality rate for each sex and their ratio         #
# Author : Pietro Violo                                                        #
# Date : May 15 2022                                                           #
#------------------------------------------------------------------------------#

library(tidyverse)
library(covidAgeData)
library(lubridate)
library(stats)

#'* Calculate deaths for the first of every month.*

# All code is in a temporary function in order to not import everything
# when called by Source in the main script file

function(temporary){
  
  df <- download_covid(data = "Output_5") %>%                       # Download data
    filter(Country == "USA",!is.na(Deaths)) %>%                     # Filter for United States only
    mutate(Age = ifelse(Age >=85, 85, Age)) %>%                     # Sum up groups that are over 85+
    group_by(Region, Date, Sex, Age) %>% 
    summarise(Deaths = sum(Deaths)) %>% 
    ungroup() %>% 
    filter(Sex != "b") %>%                                          # Keep males and females
    mutate(Date = as.Date(Date, tryFormats = c("%d.%m.%Y"))) %>%    # Transform to date
    dplyr::mutate(year = lubridate::year(Date), 
                  month = lubridate::month(Date), 
                  day = lubridate::day(Date))

  # We want to estimate the number of deaths on the 1st of each month
  regions <- unique(df$Region)
  sexes <- unique(df$Sex)
  ages <- unique(df$Age)
  
  # Establish the range of firsts of the month
  fst.month <- min(filter(df,year==2020)$month)
  lst.month <- max(filter(df,year==max(unique(df$year)))$month)
  
  fst.date <- as.Date(paste0("2020-",min(filter(df,year==2020)$month),"-1"))
  lst.date <- as.Date(paste(max(unique(df$year)),max(filter(df,year==max(unique(df$year)))$month),"1",sep="-"))
  
  days <- seq(fst.date,lst.date,by="month")
    
  # Desired structure, without the values
  first.days <- data.frame(expand.grid(regions,sexes,ages,days)) %>%
    `colnames<-`(c("Region","Sex","Age","Date")) %>%
    arrange(Region,Sex,Age,Date) %>%
    dplyr::mutate(year = lubridate::year(Date), 
                  month = lubridate::month(Date), 
                  day = lubridate::day(Date))
    
  # Firsts of the month already in the data
  df_firsts <- df %>% filter(day==1) %>% arrange(Region,Sex,Age,Date)
  
  df_first_month <- df %>% 
    bind_rows(.,anti_join(first.days,df_firsts,by=c("Region","Sex","Age","Date"))) %>%    # Add missing firsts to the data
    arrange(Region,Sex,Age,Date) %>%
    group_by(Region,Sex,Age) %>% 
    mutate(Deaths = ifelse(is.na(Deaths) & 0 %in% Deaths & 
                             Date < nth(Date,suppressWarnings(max(which(Deaths==0)))),    # Everything before 1st 0 is a 0
                           0,Deaths)) %>%                                                              
    filter(Date >= nth(Date,min(which(!is.na(Deaths))))) %>%                              # Remove remaining leading NAs
    mutate(lag = Deaths, 
           lead = Deaths,
           lag.date = as.Date(ifelse(is.na(Deaths),NA,Date),origin='1970-01-01'),
           lead.date = lag.date) %>% 
    fill(lag,.direction = "down") %>%                                                     # Get previous and next non NA values
    fill(lead,.direction = "up") %>% 
    fill(lag.date,.direction = "down") %>%                                                # Get dates of previous and next non NA values
    fill(lead.date,.direction = "up") %>% 
    mutate(Deaths = ifelse(is.na(Deaths) & day == 1,                                      # Linear interpolation
                           lag+(lead-lag)*as.numeric(Date-lag.date)/as.numeric(lead.date-lag.date),
                           Deaths)) %>%                                            
    filter(day == 1)                                                            

  save(df_first_month, file = "./Data/df_first_month.RData")
  
  # Monotonic splines by Tim Riffe
  regions <- df %>% pull(Region) %>% unique()
  ages <- df %>% pull(Age) %>% unique()
  spline_deaths <- c()
  
  for(state in state.name){
    for(sex in c("f","m")){
      for(age in ages){
        
        all_dates <- df %>% filter(Region == state & Sex == sex & Age == age)
        first_of_month <- floor_date(all_dates$Date, "month") %>% unique()
        monodeaths <- splinefun(as.integer(all_dates$Date), all_dates$Deaths, method = "monoH.FC")(as.integer(first_of_month))
        monodeaths <- cbind(state, as.character(first_of_month), sex, age,  monodeaths)
        monodeaths <- monodeaths[-1,]
        spline_deaths <- rbind(spline_deaths, monodeaths)
      }
    }
    print(state)
  }
  
  new <- spline_deaths %>% data.frame(.) 
  colnames(new) <- c("Region", "Date", "Sex", "Age", "Deaths_mono")
  new <- new %>% mutate(Age = as.integer(Age),
                        Deaths_mono = as.double(Deaths_mono),
                        Date = as.Date(Date))
  
  
  # Comparison of linear interpolation and spline estimations
  joined <- df_first_month %>% 
    rename(Deaths_linint = Deaths) %>% 
    left_join(.,new) %>% 
    select(Region,Sex,Age, Date, Deaths_linint, Deaths_mono) %>% 
    arrange(Region,Sex,Age,Date) %>% 
    mutate(diff=Deaths_linint-Deaths_mono)

  save(joined, file = "./Data/df_first_month.RData")
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

link_census_regions <- function(data,state_var = "State") {
  # US CENSUS REGIONS
  northeast <- c("Connecticut", "Maine", "Massachusetts", "New Hampshire",
                 "Rhode Island", "Vermont", "New Jersey", "New York", "Pennsylvania")
  midwest <- c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin", "Iowa", "Kansas", "Minnesota",
               "Missouri", "Nebraska", "North Dakota", "South Dakota")
  south <- c("Delaware", "District of Columbia", "Florida", "Georgia", "Maryland", "North Carolina",
             "South Carolina", "Virginia", "West Virginia", "Alabama", "Kentucky", "Mississippi",
             "Tennessee", "Arkansas", "Louisiana", "Oklahoma", "Texas")
  west <- c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah", "Wyoming",
            "Alaska", "California", "Hawaii", "Oregon", "Washington")
  
  data <- data %>% 
    mutate(census_region = case_when(get(state_var) %in% northeast ~ "Northeast",
                                     get(state_var) %in% midwest ~ "Midwest",
                                     get(state_var) %in% south ~ "South",
                                     get(state_var) %in% west ~ "West"))
  
  return(data)
}




