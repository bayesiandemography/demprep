

date_to_cohort <- function(date, dob,
                           first_day = "1 January", is_year_to = TRUE,
                           open_left = TRUE,
                           as_factor = TRUE) {
}

date_to_cohort_multi <- function(date, dob,
                                 width = 5, origin = 2000,
                                 open_left = TRUE,
                                 as_factor = TRUE) {
}


date_to_cohort_month <- function(date, dob,
                                 open_left = TRUE,
                                 as_factor = TRUE) {
    date <- err_tdy_date(x = date,
                         name = "date")
}

date_cohort_quarter <- function(date, dob,
                                open_left = TRUE,
                                as_factor = TRUE) {
    date <- err_tdy_date(x = date,
                         name = "date")
}

