
## derive_agetime_year <- function(age = NULL,
##                                 period = NULL,
##                                 cohort = NULL,
##                                 triangle = NULL) {
##     has_age <- !is.null(age)
##     has_period <- !is.null(period)
##     has_cohort <- !is.null(cohort)
##     has_triangle <- !is.null(triangle)
##     sum_has <- has_age + has_period + has_cohort + has_triangle
##     if (!identical(sum_has, 3L))
##         stop(gettextf("need to specify exactly 3 of '%s', '%s', '%s', '%s' [currently specify %d]"
##                       "age", "period", "cohort", "triangle", sum_has),
##              call. = FALSE)
##     if (has_age)
##         n_age <- length(age)
##     if (has_period)
##         n_period <- length(period)
##     if (has_cohort)
##         n_cohort <- length(cohort)
##     if (has_triangle)
##         n_triangle <- length(triangle)
##     if (has_age) {
##         if (has_period && !identical(n_age, n_period))
##             stop(gettextf("'%s' and '%s' have different lengths",
##                           "age", "period"),
##                  call. = FALSE)
##         if (has_cohort && !identical(n_age, n_cohort))
##             stop(gettextf("'%s' and '%s' have different lengths",
##                           "age", "cohort"),
##                  call. = FALSE)
##         if (has_triangle && !identical(n_age, n_triangle))
##             stop(gettextf("'%s' and '%s' have different lengths",
##                           "age", "triangle"),
##                  call. = FALSE)
##     }
##     else {
##         if (has_period && !identical(n_age, n_period))
##             stop(gettextf("'%s' and '%s' have different lengths",
##                           "age", "period"),
##                  call. = FALSE)
##         if (has_cohort && !identical(n_age, n_cohort))
##             stop(gettextf("'%s' and '%s' have different lengths",
##                           "age", "cohort"),
##                  call. = FALSE)
##         if (has_triangle && !identical(n_age, n_triangle))
##             stop(gettextf("'%s' and '%s' have different lengths",
##                           "age", "triangle"),
##                  call. = FALSE)
        
    


    
##     if (has_age) {

##         labels_age <- unique(age)
##         parsed_age <- parse_integers(x = labels_age,
##                                      name = "age")
##         low_age <- parsed_age$low
##         up_age <- parsed_age$up
##         is_open_first_age <- parsed_age$is_open_first
##         is_open_last_age <- parsed_age$is_open_last
##         break_max_age <- parsed_age$break_max # integer
##         i_open_first_age <- match(TRUE, is_open_first_age, nomatch = 0L)
##         if (i_open_first_age > 0L) {
##             stop(gettextf("'%s' has interval [\"%s\"] that is open on the left",
##                           "age", labels_age[[i_open_first_age]]),
##                  call. = FALSE)
##         }
##     }
##     if (has_period) {
##         n_period <- length(period)
##         labels_period <- unique(period)
##         parsed_period <- parse_integers(x = labels_period,
##                                         name = "period")
##         low_period <- parsed_period$low
##         up_period <- parsed_period$up
##         is_open_first_period <- parsed_period$is_open_first
##         is_open_last_period <- parsed_period$is_open_last
##         break_max_period <- parsed_period$break_max # integer
##         i_open_first_period <- match(TRUE, is_open_first_period, nomatch = 0L)
##         if (i_open_first_period > 0L) {
##             stop(gettextf("'%s' has interval [\"%s\"] that is open on the left",
##                           "period", labels_period[[i_open_first_period]]),
##                  call. = FALSE)
##         }
##         i_open_last_period <- match(TRUE, is_open_last_period, nomatch = 0L)
##         if (i_open_last_period > 0L) {
##             stop(gettextf("'%s' has interval [\"%s\"] that is open on the right",
##                           "period", labels_period[[i_open_last_period]]),
##                  call. = FALSE)
##         }
##     }
##     if (has_period) {
##         n_period <- length(period)
##         labels_period <- unique(period)
##         parsed_period <- parse_integers(x = labels_period,
##                                         name = "period")
##         low_period <- parsed_period$low
##         up_period <- parsed_period$up
##         is_open_first_period <- parsed_period$is_open_first
##         is_open_last_period <- parsed_period$is_open_last
##         break_max_period <- parsed_period$break_max # integer
##         i_open_first_period <- match(TRUE, is_open_first_period, nomatch = 0L)
##         if (i_open_first_period > 0L) {
##             stop(gettextf("'%s' has interval [\"%s\"] that is open on the left",
##                           "period", labels_period[[i_open_first_period]]),
##                  call. = FALSE)
##         }
##         i_open_last_period <- match(TRUE, is_open_last_period, nomatch = 0L)
##         if (i_open_last_period > 0L) {
##             stop(gettextf("'%s' has interval [\"%s\"] that is open on the right",
##                           "period", labels_period[[i_open_last_period]]),
##                  call. = FALSE)
##         }
##     }

        
