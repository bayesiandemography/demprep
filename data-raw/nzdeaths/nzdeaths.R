
library(readr)
library(dplyr)
library(tidyr)

nzdeaths <- read_csv("data-raw/nzdeaths/VSD349202_20210630_082144_75.csv",
                     skip = 1,
                     n_max = 69) %>%
    rename(age = X1, sex = X2) %>%
    fill(age) %>%
    pivot_longer(-c(age, sex), names_to = "year", values_to = "count") %>%
    mutate(year = as.integer(year),
           count = as.integer(count)) %>%
    as.data.frame()

save(nzdeaths,
     file = "data/nzdeaths.rda",
     compress = "bzip2")
