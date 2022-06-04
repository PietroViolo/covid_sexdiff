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
    mutate(Day = as.Date(Date, tryFormats = c("%d.%m.%Y"))) %>%    # Transform to date
    dplyr::mutate(year = lubridate::year(Day), 
                  month = lubridate::month(Day), 
                  day = lubridate::day(Day)) %>% 
    select(-Date)
  
  # Now, we have various dates (but not all) available, we need to find the
  # earliest and dates date available for each month and each state/age group
  
  # Earliest day for each month
  
  # We want to estimate the number of deaths on the 1st of each month
  regions <- unique(df$Region)
  sexes <- unique(df$Sex)
  ages <- unique(df$Age)
  
  fst.month <- min(filter(df,year==2020)$month)
  lst.month <- max(filter(df,year==max(unique(df$year)))$month)
  
  fst.date <- as.Date(paste0("2020-",min(filter(df,year==2020)$month),"-1"))
  lst.date <- as.Date(paste(max(unique(df$year)),max(filter(df,year==max(unique(df$year)))$month),"1",sep="-"))
  
  days <- seq(fst.date,lst.date,by="month")
    
  deaths.first <- array(data=NA,dim=c(length(regions),length(days)),dimnames=list(regions,format(days)))
  
  first.days <- data.frame(expand.grid(regions,sexes,ages,days)) %>%
    `colnames<-`(c("Region","Sex","Age","Day")) %>%
    arrange(Region,Sex,Age,Day)
    
  df_firsts <- df %>% filter(day==1) %>% arrange(Region,Sex,Age,Day)
  
  df_first_month <- df %>% 
    bind_rows(.,anti_join(first.days,df_firsts,by=c("Region","Sex","Age","Day"))) %>% 
    arrange(Region,Sex,Age,Day) %>% 
    mutate(year = lubridate::year(Day), 
           month = lubridate::month(Day), 
           day = lubridate::day(Day)) %>% 
    group_by(Region,Sex,Age) %>% 
    mutate(Deaths = ifelse(is.na(Deaths) & 0 %in% Deaths & Day < nth(Day,suppressWarnings(max(which(Deaths==0)))),
                           0,Deaths),
           lag = Deaths,
           lead = Deaths) %>% 
    fill(lag,.direction = "down") %>% 
    fill(lead,.direction = "up") %>% 
    mutate(Deaths = ifelse(is.na(Deaths) & !is.na(lag) & !is.na(lead) & lag == lead,
                           lag,Deaths)) %>% 
    filter(Day >= nth(Day,min(which(!is.na(Deaths))))) %>% 
    mutate(lag.day = as.Date(ifelse(is.na(Deaths),NA,Day),origin='1970-01-01'),
           lead.day = lag.day) %>% 
    fill(lag.day,.direction = "down") %>% 
    fill(lead.day,.direction = "up") %>% 
    mutate(Deaths = ifelse(is.na(Deaths) & day == 1,
                           lag+(lead-lag)*as.numeric(Day-lag.day)/as.numeric(lead.day-lag.day),
                           Deaths)) %>% 
    filter(day == 1)

  
  # df_min_dates <- df %>% 
  #   group_by(Region,Sex,Age,year,month) %>% 
  #   summarise(day = min(day)) %>% 
  #   left_join(.,df, by = c("Region","Sex","Age","year","month","day")) %>% 
  #   ungroup() %>% 
  #   group_by("Region","Sex","Age") %>% 
  #   mutate(days_between = (interval(Date, dplyr::lead(Date)) %/% days(1)),
  #          deaths_between = dplyr::lead(Deaths)-Deaths,
  #          days_before_first = (interval(Date, floor_date(dplyr::lead(Date),"month"))) %/% days(1), # Number of days until first of each month
  #          Deaths_first = Deaths + deaths_between/days_between * days_before_first) %>% # Difference in days and deaths between two dates
  #   mutate(Deaths = dplyr::lag(Deaths_first),day = 1) %>% 
  #   ungroup() %>% 
  #   select(Region, Sex, Age, year, month, day, Deaths) %>% 
  #   mutate(Deaths = ifelse((Age == 0) & (Deaths > 220), NA, Deaths)) %>% 
  #   na.omit() %>% 
  #   filter(Deaths >= 0) %>% 
  #   mutate(date = as.Date(paste(year, month, day, sep="/"), format = "%Y/%m/%d"))

  # Deaths on the first of the month
  
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
  colnames(new) <- c("Region", "date", "Sex", "Age", "Deaths_mono")
  new <- new %>% mutate(date = as.Date(as.character(date)),
                        Age = as.integer(Age),
                        Deaths_mono = as.double(Deaths_mono))
  
  
  # All values are essentially the same
  joined <- left_join(df_first_month,new) %>% select(Region,Sex,Age, date, Deaths, Deaths_mono)
  
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




