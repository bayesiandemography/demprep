
## derive_agetime <- function(age = NULL,
##                            period = NULL,
##                            cohort = NULL,
##                            triangle = NULL) {
##     has_age <- !is.null(age)
##     has_period <- !is.null(period)
##     has_cohort <- !is.null(cohort)
##     has_triangle <- !is.null(triangle)
##     sum_has <- has_age + has_period + has_cohort + has_triangle
##     if (!identical(sum_has, 3L))
##         stop(gettextf("need to specify exactly 3 of '%s', '%s', '%s', '%s' [currently specify %d]"
##                       "age", "period", "cohort", "triangle", sum_has),
##              call. = FALSE)
##     if (has_age) {
        
