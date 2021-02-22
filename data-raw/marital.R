
library(synthpop)
library(dplyr)
library(demprep)

set.seed(0)
marital <- synthpop::SD2011 %>%
    mutate(year = 2011,
           month = 5,
           date_survey = impute_date(year = year, month = month)) %>%
    select(date_survey,
           age,
           ymarr,
           mmarr) %>%
    mutate(date_birth = impute_dob(date = date_survey, age_years = age)) %>%
    mutate(date_marry = impute_date(year = ymarr, month = mmarr)) %>%
    filter(is.na(date_marry) | as.integer(date_marry - date_birth) > (365.25 * 18)) %>%
    select(date_survey, date_birth, date_marry) %>%
    sample_n(size = 1000)


marital$age <- date_to_age_year(date = rep("2011-01-01", 1000),
                                dob = marital$date_birth,
                                break_min = NULL,
                                break_max = 90)
head(marital)
dtabs(marital, ~ age)


marital$age5 <- date_to_age_multi(date = rep("2011-01-01", 1000),
                                  dob = marital$date_birth,
                                  width = 5,
                                  break_min = NULL,
                                  break_max = 90)
head(marital)
dtabs(marital, ~ age5)


marital$period_birth <- date_to_period_multi(date = marital$date_birth,
                                             origin = 2011)
head(marital)
dtabs(marital, ~ period_birth)


    
marital$period_marry <- date_to_period_multi(date = marital$date_marry,
                                             origin = 2011)
head(marital)

marital$age <- date_to_age_year(date = marital$date_survey,
                                dob = marital$date_birth)

head(marital)






    filter(
    select(sex, age, date_born, date_marry) %>%
    mutate(age_at_survey = date_to_age_year(date = "2011-05-01", dob = date_born)) %>%
    mutate(period_marry = date_to_period_multi(date = date_marry))

    mutate(age_at_marriage = date_to_age_year(date = date_marry, dob = date_born))

           
           
