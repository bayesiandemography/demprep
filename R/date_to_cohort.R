

date_to_cohort_year <- function(date, dob,
                                first_month = "Jan",
                                is_year_to = TRUE,
                                year_min = NULL,
                                open_left = TRUE,
                                as_factor = TRUE) {
}



date_to_cohort_multi <- function(date, dob,
                                 width = 5, origin = 2000,
                                 year_min = NULL,
                                 open_left = TRUE,
                                 as_factor = TRUE) {
}


date_to_cohort_month <- function(date, dob,
                                 month_min = NULL,
                                 open_left = TRUE,
                                 as_factor = TRUE) {
    date <- err_tdy_date(x = date,
                         name = "date")
}

date_cohort_quarter <- function(date, dob,
                                quarter_min = NULL,
                                open_left = TRUE,
                                as_factor = TRUE) {
    date <- err_tdy_date(x = date,
                         name = "date")
}

