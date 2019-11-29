
## DimScale -------------------------------------------------------------------

validity_DimScale <- function(object) {
    include_na <- object@include_na
    val <- demcheck::chk_is_logical_flag(x = include_na,
                                         name = "include_na")
    if (!isTRUE(val))
        return(val)
    TRUE
}

setClass("DimScale",
         contains = "VIRTUAL",
         slots = c(include_na = "logical"),
         validity = validity_DimScale)


## AgeGroup

validity_AgeGroup <- function(object) {
    open_last <- object@open_last
    val <- demcheck::chk_is_logical_flag(x = open_last,
                                         name = "open_last")
    if (!isTRUE(val))
        return(val)
    TRUE
}

setClass("AgeGroup",
         contains = c("DimScale",
                      "VIRTUAL"),
         slots = c(open_last = "logical"),
         validity = validity_AgeGroup)

validity_AgeGroupSingleQuarterMonth <- function(object) {
    break_min <- object@break_min
    break_max <- object@break_max
    val <- demcheck::chk_x_min_max_integer(x_min = break_min,
                                           x_max = break_max,
                                           name_min = "break_min",
                                           name_max = "break_max")
    if (!isTRUE(val))
        return(val)
    TRUE
}

## HAS_TESTS
setClass("AgeGroupSingle",
         contains = "AgeGroup",
         slots = c(break_min = "integer",
                   break_max = "integer",
                   open_last = "logical"),
         validity = validity_AgeGroupSingleQuarterMonth)


validity_AgeGroupMulti <- function(object) {
    break_min <- object@break_min
    break_max <- object@break_max
    width <- object@width
    val <- demcheck::chk_x_min_max_integer(x_min = break_min,
                                           x_max = break_max,
                                           name_min = "break_min",
                                           name_max = "break_max")
    if (!isTRUE(val))
        return(val)
    val <- demcheck::chk_is_positive_scalar(x = width,
                                            name = "width")
    if ((break_max - break_min) %% width != 0L)
        return(gettextf("difference between '%s' [%d] and '%s' [%d] not a multiple of '%s' [%d]",
                        "break_max", break_max, "break_min", break_min, "width", width))
    TRUE
}

## HAS_TESTS
setClass("AgeGroupMulti",
         contains = "AgeGroup",
         slots = c(break_min = "integer",
                   break_max = "integer",
                   width = "integer",
                   open_last = "logical"),
         validity = validity_AgeGroupMulti)


validity_AgeGroupCustom <- function(object) {
    breaks <- object@breaks
    open_last  <- object@open_last
    val <- demcheck::chk_x_integer(x = breaks,
                                   name = "breaks",
                                   open_first = FALSE,
                                   open_last = open_last)
    if (!isTRUE(val))
        return(val)
    TRUE
}

## HAS_TESTS
setClass("AgeGroupCustom",
         contains = "AgeGroup",
         slots = c(breaks = "integer",
                   open_last = "logical"),
         validity = validity_AgeGroupCustom)

## HAS_TESTS
setClass("AgeGroupQuarter",
         contains = "AgeGroup",
         slots = c(break_min = "integer",
                   break_max = "integer",
                   open_last = "logical"),
         validity = validity_AgeGroupSingleQuarterMonth)

## HAS_TESTS
setClass("AgeGroupMonth",
         contains = "AgeGroup",
         slots = c(break_min = "integer",
                   break_max = "integer",
                   open_last = "logical"),
         validity = validity_AgeGroupSingleQuarterMonth)


## Period

## We refer to 'year_min' and 'year_max', rather than
## 'break_min' and 'break_max' for periods
## and cohorts measured in years, because the labels
## for these periods and cohorts omit information
## on month, which is contained in the breaks

setClass("Period",
         contains = c("DimScale",
                      "VIRTUAL"))


validity_PeriodCohortSingle <- function(object) {
    year_min <- object@year_min
    year_max <- object@year_max
    val <- demcheck::chk_x_min_max_integer(x_min = year_min,
                                           x_max = year_max,
                                           name_min = "year_min",
                                           name_max = "year_max")
    if (!isTRUE(val))
        return(val)
    TRUE
}

setClass("PeriodSingle",
         contains = "Period",
         slots = c(year_min = "integer",
                   year_max = "integer"),
         validity = validity_PeriodCohortSingle)


validity_PeriodCohortMulti <- function(object) {
    year_min <- object@year_min
    year_max <- object@year_max
    width <- object@width
    val <- demcheck::chk_x_min_max_integer(x_min = year_min,
                                           x_max = year_max,
                                           name_min = "year_min",
                                           name_max = "year_max")
    if (!isTRUE(val))
        return(val)
    val <- demcheck::chk_is_positive_scalar(x = width,
                                            name = "width")
    if ((year_max - year_min) %% width != 0L)
        return(gettextf("difference between '%s' [%d] and '%s' [%d] not a multiple of '%s' [%d]",
                        "year_max", year_max, "year_min", year_min, "width", width))
    TRUE
}

setClass("PeriodMulti",
         contains = "Period",
         slots = c(year_min = "integer",
                   year_max = "integer",
                   width = "integer"),
         validity = validity_PeriodCohortMulti)

validity_PeriodCustom <- function(object) {
    years <- object@years
    val <- demcheck::chk_x_integer(x = years,
                                   name = "years",
                                   open_first = FALSE,
                                   open_last = FALSE)
    if (!isTRUE(val))
        return(val)
    TRUE
}

## HAS_TESTS
setClass("PeriodCustom",
         contains = "Period",
         slots = c(years = "integer"),
         validity = validity_PeriodCustom)


validity_PeriodCohortQuarter <- function(object) {
    break_min <- object@break_min
    break_max <- object@break_max
    val <- demcheck::chk_break_min_max_date(break_min = break_min,
                                            break_max = break_max,
                                            unit = "quarter")
    if (!isTRUE(val))
        return(val)
    TRUE
}

## HAS_TESTS
setClass("PeriodQuarter",
         contains = "Period",
         slots = c(break_min = "Date",
                   break_max = "Date"),
         validity = validity_PeriodCohortQuarter)


validity_PeriodCohortMonth <- function(object) {
    break_min <- object@break_min
    break_max <- object@break_max
    val <- demcheck::chk_break_min_max_date(break_min = break_min,
                                            break_max = break_max,
                                            unit = "month")
    if (!isTRUE(val))
        return(val)
    TRUE
}

## HAS_TESTS
setClass("PeriodMonth",
         contains = "Period",
         slots = c(break_min = "Date",
                   break_max = "Date"),
         validity = validity_PeriodCohortMonth)


## Cohort

validity_Cohort <- function(object) {
    open_first <- object@open_first
    val <- demcheck::chk_is_logical_flag(x = open_first,
                                         name = "open_first")
    if (!isTRUE(val))
        return(val)
    TRUE
}

setClass("Cohort",
         contains = c("DimScale",
                      "VIRTUAL"),
         slots = c(open_first = "logical"),
         validity = validity_Cohort)

## HAS_TESTS
setClass("CohortSingle",
         contains = "Cohort",
         slots = c(year_min = "integer",
                   year_max = "integer"),
         validity = validity_PeriodCohortSingle)

setClass("CohortMulti",
         contains = "Cohort",
         slots = c(year_min = "integer",
                   year_max = "integer",
                   width = "integer"),
         validity = validity_PeriodCohortMulti)

validity_CohortCustom <- function(object) {
    open_first <- object@open_first
    years <- object@years
    val <- demcheck::chk_x_integer(x = years,
                                   name = "years",
                                   open_first = open_first,
                                   open_last = FALSE)
    if (!isTRUE(val))
        return(val)
    TRUE
}

setClass("CohortCustom",
         contains = "Cohort",
         slots = c(years = "integer"),
         validity = validity_CohortCustom)


setClass("CohortQuarter",
         contains = "Cohort",
         slots = c(break_min = "Date",
                   break_max = "Date"),
         validity = validity_PeriodCohortQuarter)


setClass("CohortMonth",
         contains = "Cohort",
         slots = c(break_min = "Date",
                   break_max = "Date"),
         validity = validity_PeriodCohortMonth)


