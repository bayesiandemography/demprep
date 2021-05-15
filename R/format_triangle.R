
## NO_TESTS
#' Format labels for one-year Lexis triangles
#'
#' Create labels for one-year Lexis triangles
#' to be used with one-year age groups and periods.
#'
#' \code{age} gives the age group to which each triangle
#' in \code{x} belongs. All age groups in \code{age}
#' must be single-year, except for any open age groups.
#'
#' \code{open_last} determines whether the
#' allocation of triangles needs to
#' account for an open age group, and \code{break_max}
#' specifies the cut-off for the open age group.
#' See \code{\link{format_age_year}} for a description
#' of how \code{open_last} and \code{break_max}
#' control age groups.
#'
#' When \code{break_max} is \code{NULL},
#' the return value from \code{format_triangle_year}
#' is identical to \code{x}. When \code{break_max}
#' is non-\code{NULL}, the return value is as follows.
#'
#' \tabular{lll}{
#'   \code{x} \tab \code{age} \tab return value \cr
#'   \code{"Lower"} \tab \code{<= break_max} \tab \code{"Lower"} \cr
#'   \code{"Lower"} \tab \code{> break_max} \tab \code{"Upper"} \cr
#'   \code{"Lower"} \tab \code{NA} \tab \code{NA} \cr
#'   \code{"Upper"} \tab \code{<= break_max} \tab \code{"Upper"} \cr
#'   \code{"Upper"} \tab \code{> break_max} \tab \code{"Upper"} \cr
#'   \code{"Upper"} \tab \code{NA} \tab \code{"Upper"} \cr
#'   \code{NA} \tab \code{<= break_max} \tab \code{NA} \cr
#'   \code{NA} \tab \code{> break_max} \tab \code{"Upper"} \cr
#'   \code{NA} \tab \code{NA} \tab \code{NA} \cr
#' }
#' 
#' @param x A vector of Lexis triangle labels.
#' @param age A vector of age groups, the same length
#' as \code{x}.
#' @param break_max An integer or \code{NULL}.
#' Defaults to 100.
#' @param open_last Whether the final age group
#' has no upper limit. Defaults to \code{TRUE}.
#'
#' @return A factor with the same length as
#' \code{x}.
#'
#' @seealso Other functions for reformating
#' triangle labels are 
#' \code{\link{format_triangle_multi}},
#' \code{\link{format_triangle_births}},
#' \code{\link{format_triangle_quarter}},
#' and \code{\link{format_triangle_month}}.
#'
#' \code{\link{date_to_triangle_year}} creates
#' one-year Lexis triangles from dates.
#'
#' @examples
#' ## we construct 'x' and 'age' from
#' ## dates information ourselves before
#' ## calling 'format_triangle_year'
#' date_original <- c("2024-03-27", "2022-11-09")
#' dob_original <- "2020-01-01"
#' x <- date_to_triangle_year(date = date_original,
#'                            dob = dob_original,
#'                            month_start = "Jul")
#' age <- date_to_age_year(date = date_original,
#'                         dob = dob_original)
#' format_triangle_year(x = x,
#'                      age = age)
#'
#' ## someone else has constructed
#' ## 'x' and 'age' from
#' ## dates information
#' x_processed <- c("Lower", "Lower", "Lower")
#' age_processed <- c("10", "15+", "5")
#' format_triangle_year(x = x_processed,
#'                      age = age_processed)
#'
#' ## alternative value for 'break_max'
#' format_triangle_year(x = x_processed,
#'                      age = age_processed,
#'                      break_max = 10)
#' @export 
format_triangle_year <- function(x,
                                 age,
                                 break_max = 100,
                                 open_last = TRUE) {
    format_triangle_month_quarter_year(x = x,
                                       age = age,
                                       break_max = break_max,
                                       open_last = open_last)
}


## HAS_TESTS
#' Format labels for multi-year Lexis triangles
#'
#' Format labels for multi-year Lexis triangles to
#' be used with multi-year age groups and periods.
#' These age groups and periods (apart from a
#' possible open age group) all have the same width,
#' which is set by the \code{width} parameter.
#'
#' \code{age} and \code{period} define the
#' age groups and periods to which the
#' Lexis triangles in \code{x} belong. These age groups
#' and periods can be narrower than \code{width},
#' Age groups can be single-year (\code{"23"}),
#' multi-year (\code{"20-24"})
#' or open (\code{"100+"}), and periods 
#' can be single-year (\code{"2023"})
#' or multi-year (\code{"2020-2025"}).
#'
#' The values for \code{width}, \code{break_max}, \code{open_last},
#' and \code{origin} together define a new system
#' of Lexis triangles. \code{format_triangle_multi}
#' calculates where the triangles defined by
#' \code{x}, \code{age}, and \code{period} fall within
#' this new system. For instance, if an upper triangle
#' defined by \code{x}, \code{age}, and \code{period}
#' falls entirely within a lower triangle in the new
#' system, then \code{format_triangle_multi}
#' returns \code{"Lower"}.
#' 
#' \code{open_last} determines whether the
#' triangles need to account for an
#' open age group, and \code{break_max}
#' specifies the cut-off for the open age group.
#' See \code{\link{format_age_multi}} for a description
#' of how \code{open_last} and \code{break_max}
#' determine age groups.
#' 
#' \code{x} and \code{period} must be based on the same
#' starting month, so that if \code{x} uses years that
#' start in July and end in June,
#' then \code{period} must do so too. If
#' \code{x} was created using function
#' \code{\link{date_to_triangle_year}}
#' and \code{period} was created using function
#' \code{\link{date_to_period_year}},
#' then both should have used the
#' same value for \code{month_start}.
#' If \code{x} and \code{period} were not
#' calculated from raw dates data, 
#' then it may be necessary to check the
#' documentation for \code{x} and \code{period}
#' to see which
#' months of the year were used.
#'
#' @param x A vector of Lexis triangle labels.
#' @param age A vector of age groups, the same length
#' as \code{x}.
#' @param period A vector of periods, the same length
#' as \code{x}.
#' @param break_max An integer or \code{NULL}.
#' Defaults to 100.
#' @param open_last Whether the final age group
#' has no upper limit. Defaults to \code{TRUE}.
#'
#' @return A factor with the same length as
#' \code{x}.
#'
#' @seealso Other functions for reformating
#' triangle labels are 
#' \code{\link{format_triangle_year}},
#' \code{\link{format_triangle_births}},
#' \code{\link{format_triangle_quarter}},
#' and \code{\link{format_triangle_month}}.
#'
#' \code{\link{date_to_triangle_multi}} creates
#' multi-year Lexis triangles from dates.
#'
#' @examples
#' ## we construct 'x', 'age', and 'period'
#' ## from dates information ourselves before
#' ## calling 'format_triangle_multi'
#' date_original <- c("2024-03-27", "2022-11-09")
#' dob_original <- "2020-01-01"
#' x <- date_to_triangle_multi(date = date_original,
#'                             dob = dob_original,
#'                             month_start = "Jul")
#' age <- date_to_age_multi(date = date_original,
#'                          dob = dob_original)
#' period <- date_to_period_multi(date = date_original,
#'                                month_start = "Jul")
#' format_triangle_multi(x = x,
#'                       age = age,
#'                       period = period)
#'
#' ## someone else has constructed
#' ## 'x', 'age', and 'period' from
#' ## dates information
#' x_processed <- c("Lower", "Lower", "Lower")
#' age_processed <- c("10", "15+", "5")
#' period_processed <- c(2002, 2015, 2011)
#' format_triangle_multi(x = x_processed,
#'                       age = age_processed,
#'                       period = period_processed)
#'
#' ## alternative value for 'width'
#' format_triangle_multi(x = x_processed,
#'                       age = age_processed,
#'                       period = period_processed,
#'                       width = 10)
#'
#' ## alternative value for 'break_max'
#' format_triangle_multi(x = x_processed,
#'                       age = age_processed,
#'                       period = period_processed,
#'                       break_max = 10)
#' @export 
format_triangle_multi <- function(x,
                                  age,
                                  period,
                                  width = 5,
                                  break_max = 100,
                                  open_last = TRUE,
                                  month_start = "Jan",
                                  label_year_start = TRUE,
                                  origin = 2000) {
    valid_triangles <- c("Lower", "Upper", NA)
    ## see if arguments supplied
    has_break_max <- !is.null(break_max)
    ## check arguments
    demcheck::err_length_same(x1 = age,
                              x2 = x,
                              name1 = "age",
                              name2 = "x")
    demcheck::err_length_same(x1 = period,
                              x2 = x,
                              name1 = "period",
                              name2 = "x")
    width <- demcheck::err_tdy_positive_integer_scalar(x = width,
                                                       name = "width")
    if (has_break_max) {
        break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                               name = "break_max")
        demcheck::err_multiple_of(x1 = break_max,
                                  x2 = width,
                                  name1 = "break_max",
                                  name2 = "width")
    }
    demcheck::err_is_logical_flag(x = open_last,
                                  name = "open_last")
    month_start <- demcheck::err_tdy_month_start(x = month_start,
                                                 name = "month_start")
    demcheck::err_is_logical_flag(x = label_year_start,
                                  name = "label_year_start")
    origin <- demcheck::err_tdy_integer_scalar(x = origin,
                                               name = "origin")
    ## deal with "empty" case where 'x' has no non-NA values
    n <- length(x)
    if (n == 0L) {
        ans <- factor(character(),
                      levels = c("Lower", "Upper"))
        return(ans)
    }
    ## put unique values in 'labels' vectors
    labels_x <- unique(x)
    labels_age <- unique(age)
    labels_period <- unique(period)
    ## check for invalid triangles
    is_valid_tri <- labels_x %in% valid_triangles
    i_invalid_tri <- match(FALSE, is_valid_tri, nomatch = 0L)
    if (i_invalid_tri > 0L)
        stop(gettextf("'%s' has invalid value for Lexis triangle [\"%s\"]",
                      "x", labels_x[[i_invalid_tri]]),
             call. = FALSE)
    ## parse 'age'
    parsed_age <- parse_quantities(x = labels_age,
                                   name = "age")
    low_age <- parsed_age$low # integer
    up_age <- parsed_age$up   # integer
    is_open_first_age <- parsed_age$is_open_first
    is_open_last_age <- parsed_age$is_open_last
    break_max_age <- parsed_age$break_max # integer
    i_open_first_age <- match(TRUE, is_open_first_age, nomatch = 0L)
    if (i_open_first_age > 0L) {
        stop(gettextf("'%s' has interval [\"%s\"] that is open on the left",
                      "age", labels_age[[i_open_first_age]]),
             call. = FALSE)
    }
    ## parse 'period'
    parsed_period <- parse_integers_intervals(x = labels_period,
                                              name = "period",
                                              month_start = month_start,
                                              label_year_start = label_year_start)
    low_period <- parsed_period$low # integer
    up_period <- parsed_period$up   # integer
    is_open_first_period <- parsed_period$is_open_first
    is_open_last_period <- parsed_period$is_open_last
    break_min_period <- parsed_period$break_min # integer
    break_max_period <- parsed_period$break_max # integer
    is_open_period <- is_open_first_period | is_open_last_period
    i_open_period <- match(TRUE, is_open_period, nomatch = 0L)
    if (i_open_period > 0L) {
        stop(gettextf("'%s' has open interval [\"%s\"]",
                      "period", labels_period[[i_open_period]]),
             call. = FALSE)
    }
    ## if 'open_last' is TRUE and 'break_max' is supplied, check that
    ## all open age groups start at or above 'break_max'
    if (open_last && has_break_max) {
        is_too_low_age <- is_open_last_age & (low_age < break_max)
        i_too_low_age <- match(TRUE, is_too_low_age, nomatch = 0L)
        if (i_too_low_age > 0L) {
            stop(gettextf("'%s' has open interval [\"%s\"] that starts below '%s' [%d]",
                          "age", labels_age[[i_too_low_age]], "break_max", break_max),
                 call. = FALSE)
        }
    }
    ## if 'open_last' is FALSE, check that there are no open intervals
    if (!open_last) {
        i_open_last_age <- match(TRUE, is_open_last_age, nomatch = 0L)
        if (i_open_last_age > 0L)
            stop(gettextf("'%s' is %s but '%s' has open interval [\"%s\"]",
                          "open_last", "FALSE", "age", labels_age[[i_open_last_age]]),
                 call. = FALSE)
    }
    ## if 'open_last' is FALSE, and 'break_max' is supplied,
    ## make sure that all intervals less than 'break_max'
    if (!open_last && has_break_max) {
        is_too_high_age <- up_age > break_max
        i_too_high_age <- match(TRUE, is_too_high_age, nomatch = 0L)
        if (i_too_high_age > 0L) {
            stop(gettextf("'%s' has interval [\"%s\"] that ends above '%s' [%d]",
                          "age", labels_age[[i_too_high_age]], "break_max", break_max),
                 call. = FALSE)
        }
    }
    ## make 'break_max' if not supplied
    if (!has_break_max) {
        remainder_max_age <- break_max_age %% width
        if (remainder_max_age == 0L)
            break_max <- break_max_age
        else
            break_max <- break_max_age - remainder_max_age + width
        message(gettextf("setting '%s' to %d",
                         "break_max", break_max))
    }
    ## make breaks for age
    breaks_age <- seq.int(from = 0L,
                          to = break_max,
                          by = width)
    ## Check that all age intervals fall within implied breaks.
    ## (Checking now gives more informative error messages
    ## then waiting for attempt to form Lexis triangles.)
    i_interval_age <- make_i_interval(low = low_age,
                                      up = up_age,
                                      breaks = breaks_age,
                                      open_first = FALSE,
                                      open_last = open_last)
    is_multiple_intervals_age <- i_interval_age == -1L
    i_multiple_intervals_age <- match(TRUE, is_multiple_intervals_age, nomatch = 0L)
    if (i_multiple_intervals_age > 0L)
        stop(gettextf("'%s' has interval [\"%s\"] that intersects two or more intervals formed using '%s = %d' and '%s = %d'",
                      "age",
                      labels_age[[i_multiple_intervals_age]],
                      "break_max",
                      break_max,
                      "width",
                      width),
             call. = FALSE)
    ## make breaks for period
    remainder_min_period <- (break_min_period - origin) %% width
    break_min_period <- break_min_period - remainder_min_period
    remainder_max_period <- (break_max_period - origin) %% width
    if (remainder_max_period == 0L)
        break_max_period <- break_max_period
    else
        break_max_period <- break_max_period - remainder_max_period + width
    ## Check that all intervals fall within implied breaks.
    ## (Checking now gives more informative error messages
    ## then waiting for attempt to form Lexis triangles.)
    breaks_period <- seq.int(from = break_min_period,
                             to = break_max_period,
                             by = width)
    i_interval_period <- make_i_interval(low = low_period,
                                         up = up_period,
                                         breaks = breaks_period,
                                         open_first = FALSE,
                                         open_last = FALSE)
    is_multiple_intervals_period <- i_interval_period == -1L
    i_multiple_intervals_period <- match(TRUE, is_multiple_intervals_period, nomatch = 0L)
    if (i_multiple_intervals_period > 0L)
        stop(gettextf("'%s' has interval [\"%s\"] that intersects two or more intervals formed using '%s = %d' and '%s = %d'",
                      "period",
                      labels_period[[i_multiple_intervals_period]],
                      "origin",
                      origin,
                      "width",
                      width),
             call. = FALSE)
    ## Construct and classify Lexis squares from existing labels
    i_labels_age <- match(age, labels_age)
    i_labels_period <- match(period, labels_period)
    low_age_all <- low_age[i_labels_age]
    low_period_all <- low_period[i_labels_period]
    up_age_all <- up_age[i_labels_age]
    up_period_all <- up_period[i_labels_period]
    height_all <- up_age_all - low_age_all
    width_all <- up_period_all - low_period_all
    is_square <- height_all == width_all
    i_not_square <- match(FALSE, is_square, nomatch = 0L)
    if (i_not_square > 0L)
        stop(gettextf("element %d of '%s' [\"%s\"] and element %d of '%s' [\"%s\"] have different widths, so do not form a Lexis square",
                      i_not_square,
                      "age",
                      age[[i_not_square]],
                      i_not_square,
                      "period",
                      period[[i_not_square]]),
             call. = FALSE)
    i_low_age <- findInterval(low_age_all, breaks_age)
    i_low_period <- findInterval(low_period_all, breaks_period)
    break_age <- breaks_age[i_low_age]
    break_period <- breaks_period[i_low_period]
    offset_low_age <- low_age_all - break_age
    offset_low_period <- low_period_all - break_period
    offset_up_age <- up_age_all - break_age
    offset_up_period <- up_period_all - break_period
    is_na_x <- is.na(x)
    is_na_age <- is.na(age)
    is_na_period <- is.na(period)
    is_na_age_period <- is_na_age | is_na_period
    is_na_any <- is_na_x | is_na_age | is_na_period
    is_lower <- !is_na_x & (x == "Lower")
    is_upper <- !is_na_x & (x == "Upper")
    lower_stays_lower <- !is_na_age_period & (offset_low_age <= offset_low_period)
    upper_stays_upper <- !is_na_age_period & (offset_low_age >= offset_low_period)
    lower_flips_to_upper <- !is_na_age_period & (offset_low_age >= offset_up_period)
    upper_flips_to_lower <- !is_na_age_period & (offset_up_age <= offset_low_period)
    is_ge_break_max_plus_width <- !is_na_age & (low_age_all >= break_max + width)
    is_ambig <- !(is_na_any
        | (lower_stays_lower & is_lower)
        | (upper_stays_upper & is_upper)
        | (lower_flips_to_upper & is_lower)
        | (upper_flips_to_lower & is_upper)
        | is_ge_break_max_plus_width)
    i_ambig <- match(TRUE, is_ambig, nomatch = 0L)
    if (i_ambig > 0L)
        stop(gettextf("element %d of '%s' [\"%s\"], for which '%s' is \"%s\" and '%s' is \"%s\", falls within two or more newly-created Lexis triangles",
                      i_ambig,
                      "x",
                      x[[i_ambig]],
                      "age",
                      age[[i_ambig]],
                      "period",
                      period[[i_ambig]]),
             call. = FALSE)
    ## allocate triangles
    ans <- rep(NA_character_, times = length(x))
    ans[lower_stays_lower & is_lower] <- "Lower"
    ans[upper_stays_upper & is_upper] <- "Upper"
    ans[lower_flips_to_upper & is_lower] <- "Upper"
    ans[upper_flips_to_lower & is_upper] <- "Lower"
    ans[is_ge_break_max_plus_width] <- "Upper"
    ## return result
    levels <- c("Lower", "Upper")
    if (anyNA(ans))
        levels <- c(levels, NA)
    ans <- factor(x = ans,
                  levels = levels,
                  exclude = NULL)
    ans
}


## HAS_TESTS
#' Format labels for Lexis triangles
#' used when tabulating births
#'
#' Format labels for Lexis triangles to be used with
#' age groups and periods for tabulating births.
#' These age groups and periods must all have the same
#' length, which is set by the \code{width} parameter.
#'
#' \code{age} and \code{period} define the
#' age groups and periods to which the
#' Lexis triangles
#' in \code{x} belong. These age groups and periods
#' can be narrower than \code{width}.
#' Age groups and periods can be single-year
#' (\code{"23"}, \code{"2023"}) or
#' multi-year (\code{"20-24"}, \code{"2020-2025"}).
#'
#' \code{break_min} and \code{break_max} specify
#' the range of ages over which reproduction
#' is assumed to occur, and \code{recode_up} and
#' \code{recode_down} control the way that reported
#' ages outside this range are handled. See
#' \code{\link{date_to_age_births}} for details.
#' 
#' The values for \code{width}, \code{break_min},
#' \code{break_max}, and \code{origin} together define a new system
#' of Lexis triangles. \code{format_triangle_births}
#' calculates where the triangles defined by
#' \code{x}, \code{age}, and \code{period} fall within
#' this new system. For instance, if an upper triangle
#' defined by \code{x}, \code{age}, and \code{period}
#' falls entirely within a lower triangle in the new
#' system, then \code{format_triangle_births}
#' returns \code{"Lower"}.
#'
#' If \code{recode_up} is \code{TRUE} and
#' an age is recoded upwards to fall within
#' the youngest reproductive age group,
#' then the corresponding Lexis triangle is set to
#' \code{"Lower"}. If \code{recode_down} is \code{FALSE}
#' and an age is recoded downwards to
#' fall within the lowest reproductive age group, then
#' the corresponding Lexis triangle is set to \code{"Upper"}.
#'
#' @inheritParams format_triangle_multi
#' @param break_min An integer or \code{NULL}.
#' Defaults to 15.
#' @param break_max An integer or \code{NULL}.
#' Defaults to 50.
#' @param recode_up If \code{TRUE}, births to parents
#' aged less than \code{break_min} are treated as occurring to
#' people in the youngest repoductive age group.
#' @param recode_down If \code{TRUE}, births to parents
#' aged \code{break_max} or more are treated as
#' occurring to people in the oldest reproductive
#' age group.
#'
#' @return A factor with the same length as \code{x}.
#'
#' @seealso Other functions for creating Lexis triangles are
#' \code{\link{format_triangle_year}},
#' \code{\link{format_triangle_multi}},
#' \code{\link{format_triangle_quarter}},
#' and \code{\link{format_triangle_month}}.
#'
#' \code{\link{date_to_triangle_births}} creates
#' Lexis triangles from dates.
#'
#' @examples
#' ## we construct 'x', 'age', and 'period'
#' ## from dates information ourselves before
#' ## calling 'format_triangle_multi'
#' date_birth <- c("2024-03-27", "2022-11-09")
#' dob_mother <- "2000-01-01"
#' x <- date_to_triangle_births(date = date_birth,
#'                             dob = dob_mother,
#'                             month_start = "Jul")
#' age <- date_to_age_births(date = date_birth,
#'                           dob = dob_mother)
#' period <- date_to_period_births(date = date_birth,
#'                                 month_start = "Jul")
#' format_triangle_births(x = x,
#'                        age = age,
#'                        period = period)
#'
#' ## someone else has constructed
#' ## 'x', 'age', and 'period' from
#' ## dates information
#' x_processed <- c("Lower", "Upper", "Upper")
#' age_processed <- c("20", "30-34", "25")
#' period_processed <- c("2002", "2015-2020", "2011)
#' format_triangle_multi(x = x_processed,
#'                       age = age_processed,
#'                       period = period_processed)
#'
#' ## recode up and down
#' x <- c("Upper", "Lower", "Upper")
#' age <- c("10", "50-54", "25")
#' period <- c("2002", "2015-2020", "2011)
#' format_triangle_multi(x = x,
#'                       age = age,
#'                       period = period,
#'                       recode_up = TRUE,
#'                       recode_down = TRUE)
#' @export
format_triangle_births <- function(x,
                                   age,
                                   period,
                                   width = 5,
                                   break_min = 15,
                                   break_max = 50,
                                   recode_up = FALSE,
                                   recode_down = FALSE,
                                   month_start = "Jan",
                                   label_year_start = TRUE,
                                   origin = 2000) {
    valid_triangles <- c("Lower", "Upper", NA)
    ## see if arguments supplied
    has_break_min <- !is.null(break_min)
    has_break_max <- !is.null(break_max)
    ## check arguments
    demcheck::err_length_same(x1 = age,
                              x2 = x,
                              name1 = "age",
                              name2 = "x")
    demcheck::err_length_same(x1 = period,
                              x2 = x,
                              name1 = "period",
                              name2 = "x")
    width <- demcheck::err_tdy_positive_integer_scalar(x = width,
                                                       name = "width")
    
    if (has_break_min) {
        break_min <- demcheck::err_tdy_positive_integer_scalar(x = break_min,
                                                               name = "break_min")
        demcheck::err_multiple_of(x1 = break_min,
                                  x2 = width,
                                  name1 = "break_min",
                                  name2 = "width")
    }
    if (has_break_max) {
        break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                               name = "break_max")
        demcheck::err_multiple_of(x1 = break_max,
                                  x2 = width,
                                  name1 = "break_max",
                                  name2 = "width")
    }
    if (has_break_min && has_break_max) {
        demcheck::err_lt_scalar(x1 = break_min,
                                x2 = break_max,
                                name1 = "break_min",
                                name2 = "break_max")
    }
    demcheck::err_is_logical_flag(x = recode_up,
                                  name = "recode_up")
    demcheck::err_is_logical_flag(x = recode_down,
                                  name = "recode_down")
    month_start <- demcheck::err_tdy_month_start(x = month_start,
                                                 name = "month_start")
    demcheck::err_is_logical_flag(x = label_year_start,
                                  name = "label_year_start")
    origin <- demcheck::err_tdy_integer_scalar(x = origin,
                                               name = "origin")
    ## deal with "empty" case where 'x' has no non-NA values
    n <- length(x)
    if (n == 0L) {
        ans <- factor(character(),
                      levels = c("Lower", "Upper"))
        return(ans)
    }
    ## put unique values in 'labels' vectors
    labels_x <- unique(x)
    labels_age <- unique(age)
    labels_period <- unique(period)
    ## check for invalid triangles
    is_valid_tri <- labels_x %in% valid_triangles
    i_invalid_tri <- match(FALSE, is_valid_tri, nomatch = 0L)
    if (i_invalid_tri > 0L)
        stop(gettextf("'%s' has invalid value for Lexis triangle [\"%s\"]",
                      "x", labels_x[[i_invalid_tri]]),
             call. = FALSE)
    ## parse 'age'
    parsed_age <- parse_quantities(x = labels_age,
                                   name = "age")
    low_age <- parsed_age$low # integer
    up_age <- parsed_age$up   # integer
    is_open_first_age <- parsed_age$is_open_first
    is_open_last_age <- parsed_age$is_open_last
    break_min_age <- parsed_age$break_min # integer
    break_max_age <- parsed_age$break_max # integer
    is_open_age <- is_open_first_age | is_open_last_age
    i_open_age <- match(TRUE, is_open_age, nomatch = 0L)
    if (i_open_age > 0L) {
        stop(gettextf("'%s' has open interval [\"%s\"]",
                      "age", labels_age[[i_open_age]]),
             call. = FALSE)
    }
    ## parse 'period'
    parsed_period <- parse_integers_intervals(x = labels_period,
                                              name = "period",
                                              month_start = month_start,
                                              label_year_start = label_year_start)
    low_period <- parsed_period$low # integer
    up_period <- parsed_period$up   # integer
    is_open_first_period <- parsed_period$is_open_first
    is_open_last_period <- parsed_period$is_open_last
    break_min_period <- parsed_period$break_min # integer
    break_max_period <- parsed_period$break_max # integer
    is_open_period <- is_open_first_period | is_open_last_period
    i_open_period <- match(TRUE, is_open_period, nomatch = 0L)
    if (i_open_period > 0L) {
        stop(gettextf("'%s' has open interval [\"%s\"]",
                      "period", labels_period[[i_open_period]]),
             call. = FALSE)
    }
    ## Check that ages lie within limits implied by 'break_min' and 'break_max',
    ## and recode where necessary. (Triangles recoded later.)
    if (has_break_min) {
        is_lt_min_age <- low_age < break_min
        i_lt_min_age <- match(TRUE, is_lt_min_age, nomatch = 0L)
        if (i_lt_min_age > 0L) {
            if (!recode_up) {
                stop(gettextf("'%s' has interval [\"%s\"] that starts below '%s' [%d] and '%s' is %s",
                              "age",
                              labels_age[[i_lt_min_age]],
                              "break_min",
                              break_min,
                              "recode_up",
                              "FALSE"),
                     call. = FALSE)
            }
        }
    }
    if (has_break_max) {
        is_gt_max_age <- up_age > break_max
        i_gt_max_age <- match(TRUE, is_gt_max_age, nomatch = 0L)
        if (i_gt_max_age > 0L) {
            if (!recode_down) {
                stop(gettextf("'%s' has interval [\"%s\"] that ends above '%s' [%d] and '%s' is %s",
                              "age",
                              labels_age[[i_gt_max_age]],
                              "break_max",
                              break_max,
                              "recode_down",
                              "FALSE"),
                     call. = FALSE)
            }
        }
    }
    ## make 'break_min', 'break_max' if not supplied
    if (!has_break_min) {
        remainder_min <- break_min_age %% width
        break_min <- break_min_age - remainder_min
        message(gettextf("setting '%s' to %d",
                         "break_min", break_min))
    }
    if (!has_break_max) {
        remainder_max <- break_max_age %% width
        if (remainder_max == 0L)
            break_max <- break_max_age
        else
            break_max <- break_max_age - remainder_max + width
        message(gettextf("setting '%s' to %d",
                         "break_max", break_max))
    }
    ## make breaks for age
    breaks_age <- seq.int(from = break_min,
                          to = break_max,
                          by = width)
    ## Check that all age intervals fall within implied breaks.
    ## (Checking now gives more informative error messages
    ## then waiting for attempt to form Lexis triangles.)
    i_interval_age <- make_i_interval(low = low_age,
                                      up = up_age,
                                      breaks = breaks_age,
                                      open_first = FALSE,
                                      open_last = FALSE)
    is_multiple_intervals_age <- i_interval_age == -1L
    i_multiple_intervals_age <- match(TRUE, is_multiple_intervals_age, nomatch = 0L)
    if (i_multiple_intervals_age > 0L)
        stop(gettextf("'%s' has interval [\"%s\"] that intersects two or more intervals formed using '%s = %d', '%s = %d', and '%s = %d'",
                      "age",
                      labels_age[[i_multiple_intervals_age]],
                      "break_min",
                      break_min,
                      "break_max",
                      break_max,
                      "width",
                      width),
             call. = FALSE)
    ## make breaks for period
    remainder_min_period <- (break_min_period - origin) %% width
    break_min_period <- break_min_period - remainder_min_period
    remainder_max_period <- (break_max_period - origin) %% width
    if (remainder_max_period == 0L)
        break_max_period <- break_max_period
    else
        break_max_period <- break_max_period - remainder_max_period + width
    ## Check that all intervals fall within implied period breaks.
    ## (Checking now gives more informative error messages
    ## then waiting for attempt to form Lexis triangles.)
    breaks_period <- seq.int(from = break_min_period,
                             to = break_max_period,
                             by = width)
    i_interval_period <- make_i_interval(low = low_period,
                                         up = up_period,
                                         breaks = breaks_period,
                                         open_first = FALSE,
                                         open_last = FALSE)
    is_multiple_intervals_period <- i_interval_period == -1L
    i_multiple_intervals_period <- match(TRUE, is_multiple_intervals_period, nomatch = 0L)
    if (i_multiple_intervals_period > 0L)
        stop(gettextf("'%s' has interval [\"%s\"] that intersects two or more intervals formed using '%s = %d' and '%s = %d'",
                      "period",
                      labels_period[[i_multiple_intervals_period]],
                      "origin",
                      origin,
                      "width",
                      width),
             call. = FALSE)
    ## Construct and classify Lexis squares from existing labels
    i_labels_age <- match(age, labels_age)
    i_labels_period <- match(period, labels_period)
    low_age_all <- low_age[i_labels_age]
    low_period_all <- low_period[i_labels_period]
    up_age_all <- up_age[i_labels_age]
    up_period_all <- up_period[i_labels_period]
    is_reclassified_up <- if (has_break_min) is_lt_min_age[i_labels_age] else FALSE
    is_reclassified_down <- if (has_break_max) is_gt_max_age[i_labels_age] else FALSE
    is_reclassified <- is_reclassified_up | is_reclassified_down
    height_all <- up_age_all - low_age_all
    width_all <- up_period_all - low_period_all
    is_not_square <- height_all != width_all
    i_not_square <- match(TRUE, is_not_square, nomatch = 0L)
    if (i_not_square > 0L)
        stop(gettextf("element %d of '%s' [\"%s\"] and element %d of '%s' [\"%s\"] have different widths, so do not form a Lexis square",
                      i_not_square,
                      "age",
                      age[[i_not_square]],
                      i_not_square,
                      "period",
                      period[[i_not_square]]),
             call. = FALSE)
    i_low_age <- findInterval(low_age_all, breaks_age)
    i_low_period <- findInterval(low_period_all, breaks_period)
    break_age <- breaks_age[i_low_age]
    break_period <- breaks_period[i_low_period]
    offset_low_age <- low_age_all - break_age
    offset_low_period <- low_period_all - break_period
    offset_up_age <- up_age_all - break_age
    offset_up_period <- up_period_all - break_period
    is_na_x <- is.na(x)
    is_na_age <- is.na(age)
    is_na_period <- is.na(period)
    is_na_age_period <- is_na_age | is_na_period
    is_na_any <- is_na_x | is_na_age | is_na_period
    is_lower <- !is_na_x & (x == "Lower")
    is_upper <- !is_na_x & (x == "Upper")
    lower_stays_lower <- !is_na_age_period & (offset_low_age <= offset_low_period)
    upper_stays_upper <- !is_na_age_period & (offset_low_age >= offset_low_period)
    lower_flips_to_upper <- !is_na_age_period & (offset_low_age >= offset_up_period)
    upper_flips_to_lower <- !is_na_age_period & (offset_up_age <= offset_low_period)
    is_ambig <- !(is_na_any
        | is_reclassified
        | (lower_stays_lower & is_lower)
        | (upper_stays_upper & is_upper)
        | (lower_flips_to_upper & is_lower)
        | (upper_flips_to_lower & is_upper))
    i_ambig <- match(TRUE, is_ambig, nomatch = 0L)
    if (i_ambig > 0L)
        stop(gettextf("element %d of '%s' [\"%s\"], for which '%s' is \"%s\" and '%s' is \"%s\", falls within two or more newly-created Lexis triangles",
                      i_ambig,
                      "x",
                      x[[i_ambig]],
                      "age",
                      age[[i_ambig]],
                      "period",
                      period[[i_ambig]]),
             call. = FALSE)
    ## allocate triangles
    ans <- rep(NA_character_, times = length(x))
    ans[lower_stays_lower & is_lower] <- "Lower"
    ans[upper_stays_upper & is_upper] <- "Upper"
    ans[lower_flips_to_upper & is_lower] <- "Upper"
    ans[upper_flips_to_lower & is_upper] <- "Lower"
    ans[is_reclassified_up] <- "Lower"
    ans[is_reclassified_down] <- "Upper"
    ## return result
    levels <- c("Lower", "Upper")
    if (anyNA(ans))
        levels <- c(levels, NA)
    ans <- factor(x = ans,
                  levels = levels,
                  exclude = NULL)
    ans
}


## NO_TESTS
#' Format labels for quarter Lexis triangles
#'
#' Create labels for one-quarter (three-month)
#' Lexis triangles to be used with one-quarter
#' age groups and periods.
#'
#' \code{age} gives the age group to which each triangle
#' in \code{x} belongs. All age groups in \code{age}
#' must have a width of one quarter,
#' except for any open age groups.
#' 
#' \code{break_max} and \code{open_first}
#' control the boundaries of Lexis triangles
#' for the oldest age group.
#' 
#' @param x A vector of Lexis triangle labels.
#' @param age A vector of age groups, the same length
#' as \code{x}.
#' @param break_max An integer or \code{NULL}.
#' Defaults to 400.
#' @param open_last Whether the final age group
#' has no upper limit. Defaults to \code{TRUE}.
#'
#' @return A factor with the same length as
#' \code{x}.
#'
#' @seealso Other functions for reformating
#' triangle labels are 
#' \code{\link{format_triangle_year}},
#' \code{\link{format_triangle_multi}},
#' \code{\link{format_triangle_births}},
#' and \code{\link{format_triangle_month}}.
#'
#' \code{\link{date_to_triangle_quarter}} creates
#' one-quarter Lexis triangles from dates.
#'
#' @examples
#' ## we construct 'x' and 'age' from
#' ## dates information ourselves before
#' ## calling 'format_triangle_quarter'
#' x <- date_to_triangle_quarter(date = c("2024-03-27",
#'                                     "2022-11-09"),
#'                               dob = "2020-01-01")
#' age <- date_to_age_quarter(date = c("2024-03-27",
#'                                     "2022-11-09"),
#'                            dob = "2020-01-01")
#' format_triangle_quarter(x = x,
#'                         age = age)
#' format_triangle_quarter(x = x,
#'                         age = age,
#'                         break_max = 5)
#'
#' ## someone else has constructed
#' ## 'x' and 'age' from
#' ## dates information
#' x <- c("Lower", "Lower", "Lower")
#' age <- c("10", "15+", "5")
#' format_triangle_quarter(x = x,
#'                         age = age,
#'                         break_max = 10)
#' @export 
format_triangle_quarter <- function(x,
                                    age,
                                    break_max = 400,
                                    open_last = TRUE) {
    format_triangle_month_quarter_year(x = x,
                                       age = age,
                                       break_max = break_max,
                                       open_last = open_last)
}



## NO_TESTS
#' Format labels for one-month Lexis triangles
#'
#' Create labels for one-month Lexis triangles
#' to be used with labels for one-month
#' age groups and periods.
#'
#' \code{age} gives the age group to which each triangle
#' in \code{x} belongs. All age groups in \code{age}
#' must be single-month, except for any open age groups.
#' The lower limit of open age groups in \code{age} must
#' be at least as high as \code{break_min}
#' (if \code{break_min} is non-\code{NULL}).
#' 
#' \code{break_max} and \code{open_first}
#' control the boundaries of Lexis triangles
#' for the oldest age group.
#' 
#' @param x A vector of Lexis triangle labels.
#' @param age A vector of age groups, the same length
#' as \code{x}.
#' @param break_max An integer or \code{NULL}.
#' Defaults to 1200.
#' @param open_last Whether the final age group
#' has no upper limit. Defaults to \code{TRUE}.
#'
#' @return A factor with the same length as
#' \code{x}.
#'
#' @seealso Other functions for reformating
#' triangle labels are 
#' \code{\link{format_triangle_year}},
#' \code{\link{format_triangle_multi}},
#' \code{\link{format_triangle_births}},
#' and \code{\link{format_triangle_quarter}}.
#'
#' \code{\link{date_to_triangle_month}} creates
#' one-month Lexis triangles from dates.
#'
#' @examples
#' ## we construct 'x' and 'age' from
#' ## dates information ourselves before
#' ## calling 'format_triangle_month'
#' x <- date_to_triangle_month(date = c("2024-03-27",
#'                                      "2022-11-09"),
#'                             dob = "2020-01-01",
#'                             month_start = "Jul")
#' age <- date_to_age_month(date = c("2024-03-27",
#'                                   "2022-11-09"),
#'                          dob = "2020-01-01")
#' format_triangle_month(x = x,
#'                       age = age)
#' format_triangle_month(x = x,
#'                       age = age,
#'                       break_max = 12)
#'
#' ## someone else has constructed
#' ## 'x' and 'age' from
#' ## dates information
#' x <- c("Lower", "Lower", "Lower")
#' age <- c("10", "12+", "6")
#' format_triangle_month(x = x,
#'                       age = age,
#'                       break_max = 12)
#' @export 
format_triangle_month <- function(x,
                                 age,
                                 break_max = 1200,
                                 open_last = TRUE) {
    format_triangle_month_quarter_year(x = x,
                                       age = age,
                                       break_max = break_max,
                                       open_last = open_last)
}

