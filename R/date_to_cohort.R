

date_to_cohort_year <- function(date,
                                year_min = NULL,
                                year_max = NULL,
                                open_left = TRUE,
                                first_month = "Jan",
                                year_to = TRUE,
                                as_factor = TRUE) {
    date_to_period_or_cohort_year(date = date,
                                  year_min = year_min,
                                  year_max = year_max,
                                  open_left = open_left,
                                  first_month = first_month,
                                  year_to = year_to,
                                  as_factor = as_factor)
}

                                  
date_to_cohort_multi <- function(date,
                                 year_min = NULL,
                                 year_max = NULL,
                                 open_left = TRUE,
                                 width = 5,
                                 origin = 2000,
                                 as_factor = TRUE) {
}


date_to_cohort_month <- function(date,
                                 month_min = NULL,
                                 open_left = TRUE,
                                 as_factor = TRUE) {
    date <- err_tdy_date(x = date,
                         name = "date")
}

date_cohort_quarter <- function(date,
                                quarter_min = NULL,
                                open_left = TRUE,
                                as_factor = TRUE) {
    date <- err_tdy_date(x = date,
                         name = "date")
}

