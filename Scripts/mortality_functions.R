#------------------------------------------------------------------------------#
# Name : mortality_functions.R                                                 #
# Description : Calculates mortality rate for each sex and their ratio         #
# Author : Pietro Violo                                                        #
# Date : May 15 2022                                                           #
#------------------------------------------------------------------------------#

mortality_function <- function(df) {
  df_ratios <- df %>% tidyr::pivot_wider(names_from = Sex, values_from = Deaths)
  
  # US population by age and by State from CDC
  Population.US<<-read.csv("USpop.csv")
  
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
 
  
  
  # attach populations of each age group
  
}
