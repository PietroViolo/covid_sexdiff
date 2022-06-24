#------------------------------------------------------------------------------#
# Name : NA_Decypherer.R                                                       #
# Description : Decypher NA's                                                  #
# Author : Pietro Violo and Marielle CG                                        #
#------------------------------------------------------------------------------#

rm(list=ls(all=TRUE))
library(tidyverse)

# Age groups of interest
age_groups <- c("All Ages","Under 1 year", "1-4 years", "5-14 years","15-24 years", "25-34 years",
                "35-44 years", "45-54 years", "55-64 years", "65-74 years", "75-84 years","85 years and over")

# Load data and select age groups of interest
# Remove excess deaths from 2022 for total
# us_covid_2022 <- read.csv("Provisional_COVID-19_Deaths_by_Sex_and_Age_2022.csv") %>%
#   filter(Group == "By Year",
#          Year == 2022,
#          State == "United States")
# 
# us_covid_replace <- read.csv("Provisional_COVID-19_Deaths_by_Sex_and_Age_2022.csv") %>%
#   filter(Group == "By Total",
#          State == "United States")
# 
# us_covid_replace$COVID.19.Deaths <- us_covid_replace$COVID.19.Deaths - us_covid_2022$COVID.19.Deaths
# 
# # COVID data
# 
# us_covid <- us_covid <- read.csv("Provisional_COVID-19_Deaths_by_Sex_and_Age_2022.csv")
# 
# # Left join
# 
# us_covid <- left_join(us_covid,us_covid_replace)

# Clean up

us_covid <- read.csv("Provisional_COVID-19_Deaths_by_Sex_and_Age.csv") %>% 
  rename(Age = Age.Group) %>% 
  select(Group, Year, Month, State, Sex, Age, COVID.19.Deaths) %>% 
  filter(Age %in% age_groups)

# Remove Puerto Rico and add together New York City and New York State

us_covid <- us_covid %>% filter(State != "Puerto Rico") %>% filter(Year != 2022)

# Proportions of deaths per age group for the United States population

us_covid_males_proportion <- us_covid %>% filter(Group == "By Total", 
                                                 State =="United States",
                                                 Sex == "Male",
                                                 Age != "All Ages") %>% 
  mutate(proportion = COVID.19.Deaths/sum(COVID.19.Deaths)) %>% select(Sex,Age,proportion)


us_covid_females_proportion <- us_covid %>% filter(Group == "By Total", 
                                                   State =="United States",
                                                   Sex == "Female",
                                                   Age != "All Ages") %>% 
  mutate(proportion = COVID.19.Deaths/sum(COVID.19.Deaths))%>% select(Sex,Age,proportion)

us_covid_allsexes_proportion <- us_covid %>% filter(Group == "By Total", 
                                                    State =="United States",
                                                    Sex == "All Sexes",
                                                    Age != "All Ages") %>% 
  mutate(proportion = COVID.19.Deaths/sum(COVID.19.Deaths)) %>% select(Sex,Age,proportion)

proportions <- rbind(us_covid_allsexes_proportion,us_covid_males_proportion,us_covid_females_proportion)


# Splits by Total, Male and Female. If only one is NA, finds it.

SexDeduction <- function(us_covid){
  us_covid <- us_covid %>% 
    pivot_wider(names_from = Sex, values_from = COVID.19.Deaths) %>%
    mutate(Male = ifelse(is.na(Male)&!is.na(Female),`All Sexes`-Female,Male),
           Female = ifelse(is.na(Female)&!is.na(Male), `All Sexes`-Male,Female)) %>% 
    mutate(across(c(Male,Female), ~ifelse(!is.na(`All Sexes`) & is.na(Male) & is.na(Female),ifelse(`All Sexes` == 18,9,.),.))) %>% 
    pivot_longer(`All Sexes`:Female, names_to = "Sex", values_to = "COVID.19.Deaths") %>%
    select(Group,Year,Month,State,Sex,Age,COVID.19.Deaths)
  
  steps <<- steps + 1
  
  Na_count <<- c(Na_count,SummariseNA(us_covid))
  
  return(us_covid)
}

# If only one NA, the difference of the total and the non-NA's equals the missing one
AgeDeduction <- function(us_covid){
  n_na <- us_covid %>% pivot_wider(names_from = Age, values_from = COVID.19.Deaths) %>%
    select(7:17) %>% 
    is.na %>% 
    rowSums
  
  n_sum_na <- us_covid %>% pivot_wider(names_from = Age, values_from = COVID.19.Deaths) %>%
    select(7:17) %>% rowSums(na.rm = TRUE)
  
  us_covid<-us_covid %>% 
    pivot_wider(names_from = Age, values_from = COVID.19.Deaths) %>% 
    cbind(.,n_na,n_sum_na)
  
  us_covid <- us_covid %>% 
    mutate(`Under 1 year` = ifelse(is.na(`Under 1 year`)&n_na==1,`All Ages`-n_sum_na,`Under 1 year`),
           `1-4 years` = ifelse(is.na(`1-4 years`)&n_na==1,`All Ages`-n_sum_na,`1-4 years`),
           `5-14 years` = ifelse(is.na(`5-14 years`)&n_na==1,`All Ages`-n_sum_na,`5-14 years`),
           `15-24 years` = ifelse(is.na(`15-24 years`)&n_na==1,`All Ages`-n_sum_na,`15-24 years`),
           `25-34 years` = ifelse(is.na(`25-34 years`)&n_na==1,`All Ages`-n_sum_na,`25-34 years`),
           `35-44 years` = ifelse(is.na(`35-44 years`)&n_na==1,`All Ages`-n_sum_na,`35-44 years`),
           `45-54 years` = ifelse(is.na(`45-54 years`)&n_na==1,`All Ages`-n_sum_na,`45-54 years`),
           `55-64 years` = ifelse(is.na(`55-64 years`)&n_na==1,`All Ages`-n_sum_na,`55-64 years`),
           `65-74 years` = ifelse(is.na(`65-74 years`)&n_na==1,`All Ages`-n_sum_na,`65-74 years`),
           `75-84 years` = ifelse(is.na(`75-84 years`)&n_na==1,`All Ages`-n_sum_na,`75-84 years`),
           `85 years and over` = ifelse(is.na(`85 years and over`)&n_na==1,`All Ages`-n_sum_na,`85 years and over`))
  
  us_covid <- us_covid %>% 
    select(-c("n_na","n_sum_na")) %>% 
    pivot_longer(`All Ages`:`85 years and over`, names_to = "Age", values_to = "COVID.19.Deaths") %>%
    select(Group,Year,Month,State,Sex,Age,COVID.19.Deaths)
  
  steps <<- steps + 1
  
  Na_count <<- c(Na_count,SummariseNA(us_covid))
  
  return(us_covid)
}

# If n differential is equal to n cases, then 1 in each case

SingleNA_age <- function(us_covid){
  us_covid<-us_covid %>% pivot_wider(names_from = Age, values_from = COVID.19.Deaths)
  
  us_covid$n_na <- us_covid %>%
    select(7:17) %>% 
    is.na %>% 
    rowSums
  
  us_covid$n_sum_na <- us_covid %>%
    select(7:17) %>% rowSums(na.rm = TRUE)
  
  #us_covid<-us_covid %>% pivot_wider(names_from = Age, values_from = COVID.19.Deaths)
  us_covid<-us_covid %>% 
    mutate(n_diff = ifelse(is.na(`All Ages`),-1,`All Ages`-n_sum_na),
           mult = n_diff/9) %>%
    mutate(across(7:17,~ifelse(n_na == n_diff, ifelse(is.na(.),1,.),.))) %>%
    mutate(across(7:17,~ifelse(mult == n_na, ifelse(is.na(.),9,.),.))) %>% 
    select(-c(n_na,n_sum_na,n_diff,mult))
  
  us_covid <- us_covid %>% 
    pivot_longer(`All Ages`:`85 years and over`, names_to = "Age", values_to = "COVID.19.Deaths") %>%
    select(Group,Year,Month,State,Sex,Age,COVID.19.Deaths)
  
  steps <<- steps + 1
  
  Na_count <<- c(Na_count,SummariseNA(us_covid))
  
  return(us_covid)
}

SingleNA_state <- function(us_covid){
  us_covid <- us_covid %>% pivot_wider(names_from = State, values_from = COVID.19.Deaths)
  
  us_covid$n_na <- us_covid %>%
    select(Alabama:Wyoming) %>% 
    is.na %>% 
    rowSums
  
  us_covid$n_sum_na <- us_covid %>%
    select(Alabama:Wyoming) %>% 
    rowSums(na.rm = TRUE)
  
  #us_covid<-us_covid %>% pivot_wider(names_from = Age, values_from = COVID.19.Deaths)
  us_covid<-us_covid %>% 
    mutate(n_diff = ifelse(is.na(`United States`),-1,`United States`-n_sum_na),
           mult = n_diff/9) %>%
    mutate(across(Alabama:Wyoming,~ifelse(n_na == n_diff, ifelse(is.na(.),1,.),.))) %>%
    mutate(across(Alabama:Wyoming,~ifelse(mult == n_na, ifelse(is.na(.),9,.),.))) %>%
    select(-c(n_na,n_sum_na,n_diff))
  
  us_covid <- us_covid %>% 
    pivot_longer(`United States`:Wyoming, names_to = "State", values_to = "COVID.19.Deaths") %>%
    select(Group,Year,Month,State,Sex,Age,COVID.19.Deaths)
  
  steps <<- steps + 1
  
  Na_count <<- c(Na_count,SummariseNA(us_covid))
  
  return(us_covid)
}

StateDeduction <- function(us_covid){
  us_covid <- us_covid %>% pivot_wider(names_from = State, values_from = COVID.19.Deaths)
  
  us_covid$n_na <- us_covid %>%
    select(Alabama:Wyoming) %>% 
    is.na %>% 
    rowSums
  
  us_covid$n_sum_na <- us_covid %>%
    select(Alabama:Wyoming) %>% 
    rowSums(na.rm = TRUE)
  
  #us_covid<-us_covid %>% pivot_wider(names_from = Age, values_from = COVID.19.Deaths)
  us_covid<-us_covid %>% 
    mutate(n_diff = ifelse(is.na(`United States`),-1,`United States`-n_sum_na)) %>%
    mutate(across(Alabama:Wyoming,~ifelse(n_na == 1, ifelse(is.na(.),n_diff,.),.))) %>%
    select(-c(n_na,n_sum_na,n_diff))
  
  us_covid <- us_covid %>% 
    pivot_longer(`United States`:Wyoming, names_to = "State", values_to = "COVID.19.Deaths") %>%
    select(Group,Year,Month,State,Sex,Age,COVID.19.Deaths)
  
  steps <<- steps + 1
  
  Na_count <<- c(Na_count,SummariseNA(us_covid))
  
  return(us_covid)
}

# Calculates how many NA's left in COVID-19Deaths
SummariseNA <- function(us_covid){
  return(us_covid %>% summarise(isna = sum(is.na(COVID.19.Deaths))))
}

# Will look at every year and compare them to the Total

YearsNA <- function(us_covid){
  
  input <- us_covid
  us_covid <- us_covid %>% 
    filter(Group == "By Year") %>% 
    select(-Group) %>% 
    pivot_wider(names_from = Year, values_from = COVID.19.Deaths) %>% 
    left_join(.,input %>% 
                filter(Group == "By Total") %>%
                select(State,Sex,Age,COVID.19.Deaths) %>% 
                rename(Total.deaths = COVID.19.Deaths)) %>% 
    mutate(`2020` = ifelse(is.na(`2020`),Total.deaths - `2021`,`2020`),`2021` = ifelse(is.na(`2021`),Total.deaths - `2020`,`2021`)) %>% 
    mutate(across(c(`2020`,`2021`), ~ifelse(!is.na(Total.deaths) & is.na(`2020`) & is.na(`2021`),ifelse(Total.deaths == 18,9,.),.))) %>% 
    pivot_longer(`2020`:`2021`,names_to = "Year",values_to = "COVID.19.Deaths") %>%
    select(-Total.deaths) %>% 
    mutate(Group ="By Year") %>% 
    rbind(input %>% filter(Group!="By Year")) %>%
    select(Group,Year,Month,State,Sex,Age,COVID.19.Deaths)
  
  steps <<- steps + 1
  
  Na_count <<- c(Na_count,SummariseNA(us_covid))
  
  return(us_covid)
}

# Will look at every month and compare them with the year.

MonthsNA <- function(us_covid){
  
  input <- us_covid
  
  non_na <- us_covid %>% 
    filter(Group == "By Month") %>% 
    select(-Group) %>%  
    pivot_wider( names_from = Month, values_from = COVID.19.Deaths) %>%
    select(5:16) %>% 
    rowSums(na.rm = TRUE)
  n_na <- us_covid %>% 
    filter(Group == "By Month") %>% 
    select(-Group) %>%  
    pivot_wider(names_from = Month, values_from = COVID.19.Deaths) %>%
    select(5:16) %>% 
    is.na %>% 
    rowSums
  
  us_covid <- us_covid %>% 
    filter(Group == "By Month")  %>%  
    pivot_wider( names_from = Month, values_from = COVID.19.Deaths) %>%
    left_join(.,us_covid %>% 
                filter(Group == "By Year") %>% 
                rename(Total.deaths=COVID.19.Deaths) %>% 
                select(-c(Group,Month)), by = c("Year","State","Age","Sex"))
  
  us_covid <- cbind(us_covid,non_na, n_na)
  
  us_covid <- us_covid %>% mutate(`1` = ifelse(is.na(`1`) & n_na ==1,Total.deaths-non_na,`1`),
                                  `2` = ifelse(is.na(`2`) & n_na ==1,Total.deaths-non_na,`2`),
                                  `3` = ifelse(is.na(`3`) & n_na ==1,Total.deaths-non_na,`3`),
                                  `4` = ifelse(is.na(`4`) & n_na ==1,Total.deaths-non_na,`4`),
                                  `5` = ifelse(is.na(`5`) & n_na ==1,Total.deaths-non_na,`5`),
                                  `6` = ifelse(is.na(`6`) & n_na ==1,Total.deaths-non_na,`6`),
                                  `7` = ifelse(is.na(`7`) & n_na ==1,Total.deaths-non_na,`7`),
                                  `8` = ifelse(is.na(`8`) & n_na ==1,Total.deaths-non_na,`8`),
                                  `9` = ifelse(is.na(`9`) & n_na ==1,Total.deaths-non_na,`9`),
                                  `10` = ifelse(is.na(`10`) & n_na ==1,Total.deaths-non_na,`10`),
                                  `11` = ifelse(is.na(`11`) & n_na ==1,Total.deaths-non_na,`11`),
                                  `12` = ifelse(is.na(`12`) & n_na ==1,Total.deaths-non_na,`12`)) %>% 
    pivot_longer(`1`:`12`, names_to = "Month", values_to = "COVID.19.Deaths") %>%
    select(-c(Total.deaths,non_na,n_na)) %>% 
    mutate(Month = as.integer(Month)) %>%
    select(Group,Year,Month,State,Sex,Age,COVID.19.Deaths)
  
  us_covid <- rbind(input %>% filter(Group!="By Month"),us_covid) 
  
  steps <<- steps + 1
  
  Na_count <<- c(Na_count,SummariseNA(us_covid))
  return(us_covid)
}

SingleNA_month <- function(us_covid){
  input <- us_covid
  
  non_na <- us_covid %>% 
    filter(Group == "By Month") %>% 
    select(-Group) %>%  
    pivot_wider( names_from = Month, values_from = COVID.19.Deaths) %>%
    select(5:16) %>% 
    rowSums(na.rm = TRUE)
  
  n_na <- us_covid %>% 
    filter(Group == "By Month") %>% 
    select(-Group) %>%  
    pivot_wider(names_from = Month, values_from = COVID.19.Deaths) %>%
    select(5:16) %>% 
    is.na %>% 
    rowSums
  
  us_covid <- us_covid %>% 
    filter(Group == "By Month")  %>%  
    pivot_wider(names_from = Month, values_from = COVID.19.Deaths) %>%
    left_join(.,input %>% 
                filter(Group == "By Year") %>% 
                rename(Total.deaths=COVID.19.Deaths) %>% 
                select(-c(Group,Month)), by = c("Year","State","Age","Sex"))
  
  us_covid <- cbind(us_covid,non_na, n_na)
  
  #us_covid<-us_covid %>% pivot_wider(names_from = Age, values_from = COVID.19.Deaths)
  
  us_covid<-us_covid %>% 
    mutate(n_diff = ifelse(is.na(Total.deaths),-1,Total.deaths-non_na),
           mult = n_diff/9) %>%
    mutate(across(6:17,~ifelse(n_na == n_diff, ifelse(is.na(.),1,.),.)),
           across(6:17,~ifelse(mult == n_na, ifelse(is.na(.),9,.),.))) %>%
    select(-c(n_na,non_na,n_diff,mult,Total.deaths))
  
  us_covid <- us_covid %>% 
    pivot_longer(`1`:`12`, names_to = "Month", values_to = "COVID.19.Deaths") %>%
    select(Group,Year,Month,State,Sex,Age,COVID.19.Deaths)
  
  us_covid <- rbind(input %>% filter(Group!="By Month"),us_covid)
  
  steps <<- steps + 1
  
  Na_count <<- c(Na_count,SummariseNA(us_covid))
  
  return(us_covid)
}



# We assume that the proportions of deaths per age group in a State are the same as the United State's as a whole. We will impute an NA of an age group by assigning it its corresponding proportion.

# From Under one year old to upwards, if NA impute

Imputation <- function(us_covid, AgeGroup){
  
  us_covid <- left_join(left_join(us_covid,us_covid %>% filter(Age == "All Ages") %>% rename(Total.deaths=COVID.19.Deaths) %>% select(-Age),by=c("Group","Year","Month","State","Sex")),proportions) %>%mutate(imputed.deaths = round(Total.deaths * proportion))
  
  us_covid<- us_covid %>% 
    mutate(COVID.19.Deaths = ifelse(is.na(COVID.19.Deaths) & Age == AgeGroup,ifelse(imputed.deaths==0,1,imputed.deaths),COVID.19.Deaths)) %>%
    select(-c(Total.deaths,proportion,imputed.deaths)) %>%
    select(Group,Year,Month,State,Sex,Age,COVID.19.Deaths)
  
  steps <<- steps + 1
  
  Na_count <<- c(Na_count,SummariseNA(us_covid))
  
  return(us_covid)
  
}
SummariseNA(us_covid)













# While loop

age_groups <- c("1-4 years","Under 1 year","5-14 years","15-24 years","25-34 years", "35-44 years","45-54 years", "55-64 years", "65-74 years", "75-84 years", "85 years and over", "All Ages")

steps <<- 1

n_disable = 0

Na_count <<- c(SummariseNA(us_covid))

Algorithms <- c("AgeDeduction","MonthsNA","SexDeduction","StateDeduction","SingleNA_age","SingleNA_month","SingleNA_state")


# While loop that selects at random the algorithms and applies them until there are no more NA's

for(age in age_groups){
  
  while(n_disable != 10){
    
    constant = SummariseNA(us_covid)
    us_covid <<- us_covid %>% 
      SexDeduction(.) %>% 
      AgeDeduction(.) %>% 
      SingleNA_age(.) %>% 
      YearsNA(.) %>% 
      MonthsNA(.) %>% 
      SingleNA_month(.) %>% 
      StateDeduction(.) %>% 
      SingleNA_state(.)
    
    if(SummariseNA(us_covid) == constant){
      n_disable <<- n_disable + 1
    }
  }
  n_disable <<- 0
  us_covid <<- Imputation(us_covid,age)
  
}

data.frame(1:steps,unlist(Na_count))%>%
  rename("Number of NA's"= unlist.Na_count.,
         "Number of steps" = X1.steps) %>% 
  ggplot(aes(x=`Number of steps`, y=`Number of NA's`))+
  geom_line()


SummariseNA(us_covid)

#us_covid %>% filter(Group == "By Month", Age != "All Ages", State != "United States", Sex != "All Sexes") %>% summarise(sum = sum(COVID.19.Deaths,na.rm = TRUE))

write.csv(us_covid,"Non_na_COVID19deaths.csv")




# Validation
us_covid$Month <- as.integer(us_covid$Month)
us_covid$Year <- as.integer(us_covid$Year)
us_covid_valid <- left_join(us_covid_orig, us_covid, by=c("Group","Year","Month","State","Sex","Age"))
comp <- us_covid_valid %>% filter(is.na(COVID.19.Deaths.x) & !is.na(COVID.19.Deaths.y))
table(comp$COVID.19.Deaths.y)