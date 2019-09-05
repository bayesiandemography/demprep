

date_to_triangle_year <- function(date, dob,
                                  age_max = 100,
                                  open_right = TRUE,
                                  first_month = "Jan",
                                  year_to = TRUE,
                                  as_factor = TRUE) {
    l <- err_tdy_date_dob(date = date,
                          dob = dob)
    date <- l$date
    dob <- l$dob
    age_max <- err_tdy_positive_integer_scalar(x = age_max,
                                               name = "age_max",
                                               inf_ok = TRUE)
    err_is_logical_flag(x = open_right,
                        name = "open_right")
    first_month <- err_tdy_first_month(x = first_month,
                                       name = "first_month")
    err_is_logical_flag(x = year_to,
                        name = "year_to")
    err_is_logical_flag(x = as_factor,
                        name = "as_factor")
    age_months <- date_to_age_completed_months(date = date,
                                               dob = dob)
    age_years <- age_months %/% 12L
    n <- length(breaks)
    if (n > 0L) {
        open_left <- any(age_years < breaks[[1L]], na.rm = TRUE)
        open_right <- any(age_years >= breaks[[n]], na.rm = TRUE)
    }
    else {
        open_left <- FALSE
        open_right <- FALSE
    }
    labels <- make_labels_age_group(breaks = breaks,
                                    open_left = open_left,
                                    open_right = open_right)
    i <- findInterval(x = age_years, vec = breaks)
    if (open_left)
        i <- i + 1L
    ans <- labels[i]
    if (as_factor)
        ans <- factor(x = ans,
                      levels = labels,
                      exclude = NULL)
    ans    
}

date_to_triangle_fert <- function(date, dob,
                                  age_min = 15
                                  age_max = 50,
                                  recode_up = TRUE,
                                  recode_down = TRUE,
                                  first_month = "Jan",
                                  year_to = TRUE,
                                  as_factor = TRUE) {
}

date_to_triangle_multi <- function(date, dob,
                                   age_max = 100,
                                   width = 5,
                                   first_month = "Jan",
                                   as_factor = TRUE) {
}


date_to_triangle_month <- function(date, dob,
                                   age_max = 1200,
                                   open_right = TRUE,
                                   as_factor = TRUE) {
    l <- err_tdy_date_dob(date = date,
                          dob = dob)
    date <- l$date
    dob <- l$dob
    age_max <- err_tdy_positive_integer_scalar(x = age_max,
                                               name = "age_max",
                                               inf_ok = TRUE)
    err_is_logical_flag(x = open_right,
                        name = "open_right")
    err_is_logical_flag(x = as_factor,
                        name = "as_factor")
    DATE <- as_ymd(date)
    DOB <- as_ymd(dob)
    ans <- rep.int("Upper", times = n)
    is_lower <- ((DATE$d - 1L) %/% 2L) >= (DOB$d %/% 2L)
    ans[is_lower] <- "Lower"
    if (is.finite(age_max)) {
        if (open_right) {
            is_open_upper <- 12L * (DATE$y - DOB$y) + (DATE$m - DOB$m) - (DOB$d != 1L) >= age_open
            ans[is_open_upper] <- "Upper"
        }
        else {
            err_exceeds_age_max(age = age_months,
                                age_max = age_max,
                                date = date,
                                dob = dob,
                                unit = "month")
        }
    }
    ans
}

date_to_triangle_quarter <- function(date, dob,
                                     age_open = 400,
                                     as_factor = TRUE) {
}

