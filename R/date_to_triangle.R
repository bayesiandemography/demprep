
date_to_triangle_year <- function(date, dob,
                                  age_max = 100,
                                  first_month = "Jan",
                                  is_year_to = TRUE,
                                  as_factor = TRUE) {
    l <- err_tdy_date_dob(date = date,
                          dob = dob)
    date <- l$date
    dob <- l$dob
    if (all(is.na(date) | is.na(dob)))
        return(rep(NA_character_, times = length(date)))
    breaks <- err_tdy_breaks(breaks)
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
                                  is_year_to = TRUE,
                                  as_factor = TRUE) {
}

date_to_triangle_multi <- function(date, dob,
                                   age_max = 100,
                                   width = 5,
                                   first_month = "Jan",
                                   as_factor = TRUE) {
}


date_to_triangle_month <- function(date, dob,
                                   age_open = 1200,
                                   as_factor = TRUE) {
    l <- err_tdy_date_dob(date = date,
                          dob = dob)
    date <- l$date
    dob <- l$dob
    n <- length(date)
    if (all(is.na(date) | is.na(dob)))
        return(rep(NA_character_, times = n))
    DATE <- as_ymd(date)
    DOB <- as_ymd(dob)
    ans <- rep.int("Upper", times = n)
    ans[DATE$d >= DOB$d] <- "Lower"
    if (!is.null(age_open)) {
        age_open <- tdy_chk_positive_integer_scalar(age_open)
        is_open_upper <- 12L * (DATE$y - DOB$y) + (DATE$m - DOB$m) - (DOB$d != 1L) >= age_open
        ans[is_open_upper] <- "Upper"
    }
    ans
}

date_to_triangle_quarter <- function(date, dob,
                                     age_open = 400,
                                     as_factor = TRUE) {
}

