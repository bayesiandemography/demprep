
library(readr)
library(dplyr)
library(tidyr)
library(forcats)
library(demprep)
library(purrr)

set.seed(0)

age_recode <- c("14" = "Under 15 years", "65" = "65+")

repeat_row <- function(age_father, year, count)
    data.frame(age_father = rep(age_father, count),
               year = rep(year, count))

icebirths <- read_csv("data-raw/icebirths/MAN05102.csv",
                      skip = 2) %>%
    select(-Unit) %>%
    rename(age_father = Age) %>%
    pivot_longer(cols = -age_father, names_to = "year", values_to = "count") %>%
    pmap_dfr(repeat_row) %>%
    mutate(age_father = clean_age(age_father),
           age_father = fct_recode(age_father, !!!age_recode),
           age_father = as.character(age_father),
           age_father = as.integer(age_father)) %>%
    mutate(dob_child = impute_date(year = year),
           dob_father = impute_dob(date = dob_child, age_years = age_father)) %>%
    select(dob_child, dob_father) %>%
    arrange(dob_child)

save(icebirths,
     file = "data/icebirths.rda",
     compress = "xz")

