
## Labels -------------------------------------------------------------------

validity_Labels <- function(object) {
    include_na <- object@include_na
    val <- demcheck::chk_is_logical_flag(x = include_na,
                                         name = "include_na")
    if (!isTRUE(val))
        return(val)
    TRUE
}

setClass("Labels",
         contains = "VIRTUAL",
         slots = c(include_na = "logical"),
         validity = validity_Labels)


## Categories -----------------------------------------------------------------

validity_LabCategories <- function(object) {
    labels <- object@labels
    val <- demcheck::chk_categories_complete(x = labels,
                                             name = "labels")
    if (!isTRUE(val))
        return(val)
    TRUE
}

setClass("LabCategories",
         contains = "Labels",
         slots = c(labels = "character"),
         validity = validity_LabCategories)
         

## Triangles -----------------------------------------------------------------

validity_LabTriangles <- function(object) {
    labels <- object@labels
    if (!identical(labels, c("Lower", "Upper")))
        return(gettextf("'%s' not identical to \"%s\", \"%s\"",
                        "Lower", "Upper"))
    TRUE
}

setClass("LabTriangles",
         contains = "Labels",
         prototype = prototype(labels = c("Lower", "Upper")),
         slots = c(labels = "character"),
         validity = validity_LabTriangles)


## Pool -----------------------------------------------------------------------

validity_LabPool <- function(object) {
    labels <- object@labels
    if (!identical(labels, c("Ins", "Outs")))
        return(gettextf("'%s' not identical to \"%s\", \"%s\"",
                        "Ins", "Outs"))
    TRUE
}

setClass("LabPool",
         contains = "Labels",
         prototype = prototype(labels = c("Ins", "Outs")),
         slots = c(labels = "character"),
         validity = validity_LabPool)


## Quantiles ------------------------------------------------------------------

validity_LabQuantiles <- function(object) {
    labels <- object@labels
    val <- demcheck::chk_categories_complete(x = labels,
                                             name = "labels")
    if (!isTRUE(val))
        return(val)
    val <- demcheck::chk_is_valid_quantile(x = labels,
                                           name = "labels")
    if (!isTRUE(val))
        return(val)
    TRUE
}

setClass("LabQuantiles",
         contains = "Labels",
         slots = c(labels = "character"),
         validity = validity_LabQuantiles)


## Integers -------------------------------------------------------------------

validity_LabIntegers <- function(object) {
    int_min <- object@int_min
    int_max <- object@int_max
    for (name in c("int_min", "int_max")) {
        x <- slot(object, name)
        val <- demcheck::chk_is_length_1(x = x,
                                         name = name)
        if (!isTRUE(val))
            return(val)
        val <- demcheck::chk_is_not_na_scalar(x = x,
                                              name = name)
        if (!isTRUE(val))
            return(val)
    }
    if (int_max < int_min)
        return(gettextf("'%s' is less than '%s'",
                        "int_max", "int_min"))
    TRUE
}

setClass("LabInt",
         contains = "Labels",
         slots = c(int_min = "integer",
                   int_max = "integer"),
         validity = validity_LabIntegers)



## GroupsIntegers ------------------------------------------------------------


validity_Groups_integers <- function(object) {
    breaks <- object@breaks
    open_first <- object@open_first
    open_last <- object@open_last
    n <- length(breaks)
    if (n == 0L) {
        if (open_first)
            return(gettextf("'%s' has length %d but '%s' is %s",
                            name, 0L, "open_first", "TRUE"))
        if (open_last)
            return(gettextf("'%s' has length %d but '%s' is %s",
                            name, 0L, "open_last", "TRUE"))
    }
    if (n == 1L) {
        if (!open_first && !open_last)
            return(gettextf("'%s' has length %d but '%s' and '%s' are both %s",
                            name, 1L, "open_first", "open_last", "FALSE"))
    }
    val <- chk_is_not_na_vector(x = breaks,
                                name = name)
    if (!isTRUE(val))
        return(val)
    val <- chk_is_strictly_increasing(x = breaks,
                                      name = name)
    if (!isTRUE(val))
        return(val)
    TRUE
}

setClass("LabIntGroups",
         contains = c("Labels",
                      "VIRTUAL"),
         slots = c(breaks = "integer",
                   open_first = "logical",
                   open_last = "logical"))


setClass("LabIntGroupsEnumerations",
         contains = "LabIntGroups")


setClass("LabIntGroupsEndpoints",
         contains = "LabIntGroups")


## Calendar -------------------------------------------------------------------

validity_LabCalendar <- function(object) {
    break_min <- object@break_min
    break_max <- object@break_max
    for (name in c("break_min", "break_max")) {
        x <- slot(object, name)
        val <- demcheck::chk_is_length_1(x = x,
                                         name = name)
        if (!isTRUE(val))
            return(val)
        val <- demcheck::chk_is_not_na_scalar(x = x,
                                              name = name)
        if (!isTRUE(val))
            return(val)
    }
    if (break_max < break_min)
        return(gettextf("'%s' is less than '%s'",
                        "int_max", "int_min"))
    TRUE
}

setClass("LabCalendar",
         contains = c("Labels",
                      "VIRTUAL"),
         slots = c(break_min = "Date",
                   break_max = "Date"))

validity_LabCalendarQuarters <- function(object) {
    for (name in c("break_min", "break_max")) {
        x <- slot(object, name)
        val <- chk_is_first_day_unit_scalar(x = x,
                                            name = name,
                                            unit = "quarter")
        if (!isTRUE(val))
            return(val)
    }
    TRUE
}

setClass("LabCalendarQuarters",
         contains = "LabCalendar",
         validity = validity_LabCalendarQuarters)

validity_LabCalendarMonths <- function(object) {
    for (name in c("break_min", "break_max")) {
        x <- slot(object, name)
        val <- chk_is_first_day_unit_scalar(x = x,
                                            name = name,
                                            unit = "month")
        if (!isTRUE(val))
            return(val)
    }
    TRUE
}

setClass("LabCalendarMonths",
         contains = "LabCalendar",
         validity = validity_LabCalendarMonths)


## Duration -------------------------------------------------------------------

setClass("LabDuration",
         contains = c("Labels",
                      "VIRTUAL"),
         slots = c(break_min = "integer",
                   break_max = "integer"))

setClass("LabDurationQuarters",
         contains = "LabDuration")

setClass("LabDurationMonths",
         contains = "LabDuration")



## DimScale -------------------------------------------------------------------

## validity_DimScale <- function(object) {
##     include_na <- object@include_na
##     val <- demcheck::chk_is_logical_flag(x = include_na,
##                                          name = "include_na")
##     if (!isTRUE(val))
##         return(val)
##     TRUE
## }

## setClass("DimScale",
##          contains = "VIRTUAL",
##          slots = c(include_na = "logical"),
##          validity = validity_DimScale)


## ## Intervals ------------------------------------------------------------------


## setClass("Intervals",
##          contains = c("DimScale",
##                       "VIRTUAL"))


## ## AgeGroup

## validity_AgeGroup <- function(object) {
##     open_last <- object@open_last
##     val <- demcheck::chk_is_logical_flag(x = open_last,
##                                          name = "open_last")
##     if (!isTRUE(val))
##         return(val)
##     TRUE
## }

## setClass("AgeGroup",
##          contains = c("Intervals",
##                       "VIRTUAL"),
##          slots = c(open_last = "logical"),
##          validity = validity_AgeGroup)

## validity_AgeGroupSingleQuarterMonth <- function(object) {
##     break_min <- object@break_min
##     break_max <- object@break_max
##     val <- demcheck::chk_x_min_max_integer(x_min = break_min,
##                                            x_max = break_max,
##                                            name_min = "break_min",
##                                            name_max = "break_max")
##     if (!isTRUE(val))
##         return(val)
##     TRUE
## }

## ## HAS_TESTS
## setClass("AgeGroupSingle",
##          contains = "AgeGroup",
##          slots = c(break_min = "integer",
##                    break_max = "integer",
##                    open_last = "logical"),
##          validity = validity_AgeGroupSingleQuarterMonth)


## validity_AgeGroupMulti <- function(object) {
##     break_min <- object@break_min
##     break_max <- object@break_max
##     width <- object@width
##     val <- demcheck::chk_x_min_max_integer(x_min = break_min,
##                                            x_max = break_max,
##                                            name_min = "break_min",
##                                            name_max = "break_max")
##     if (!isTRUE(val))
##         return(val)
##     val <- demcheck::chk_is_positive_scalar(x = width,
##                                             name = "width")
##     if ((break_max - break_min) %% width != 0L)
##         return(gettextf("difference between '%s' [%d] and '%s' [%d] not a multiple of '%s' [%d]",
##                         "break_max", break_max, "break_min", break_min, "width", width))
##     TRUE
## }

## ## HAS_TESTS
## setClass("AgeGroupMulti",
##          contains = "AgeGroup",
##          slots = c(break_min = "integer",
##                    break_max = "integer",
##                    width = "integer",
##                    open_last = "logical"),
##          validity = validity_AgeGroupMulti)


## validity_AgeGroupCustom <- function(object) {
##     breaks <- object@breaks
##     open_last  <- object@open_last
##     val <- demcheck::chk_x_integer(x = breaks,
##                                    name = "breaks",
##                                    open_first = FALSE,
##                                    open_last = open_last)
##     if (!isTRUE(val))
##         return(val)
##     TRUE
## }

## ## HAS_TESTS
## setClass("AgeGroupCustom",
##          contains = "AgeGroup",
##          slots = c(breaks = "integer",
##                    open_last = "logical"),
##          validity = validity_AgeGroupCustom)

## ## HAS_TESTS
## setClass("AgeGroupQuarter",
##          contains = "AgeGroup",
##          slots = c(break_min = "integer",
##                    break_max = "integer",
##                    open_last = "logical"),
##          validity = validity_AgeGroupSingleQuarterMonth)

## ## HAS_TESTS
## setClass("AgeGroupMonth",
##          contains = "AgeGroup",
##          slots = c(break_min = "integer",
##                    break_max = "integer",
##                    open_last = "logical"),
##          validity = validity_AgeGroupSingleQuarterMonth)


## ## Period

## ## We refer to 'year_min' and 'year_max', rather than
## ## 'break_min' and 'break_max' for periods
## ## and cohorts measured in years, because the labels
## ## for these periods and cohorts omit information
## ## on month, which is contained in the breaks

## setClass("Period",
##          contains = c("Intervals",
##                       "VIRTUAL"))


## validity_PeriodCohortSingle <- function(object) {
##     year_min <- object@year_min
##     year_max <- object@year_max
##     val <- demcheck::chk_x_min_max_integer(x_min = year_min,
##                                            x_max = year_max,
##                                            name_min = "year_min",
##                                            name_max = "year_max")
##     if (!isTRUE(val))
##         return(val)
##     TRUE
## }

## setClass("PeriodSingle",
##          contains = "Period",
##          slots = c(year_min = "integer",
##                    year_max = "integer"),
##          validity = validity_PeriodCohortSingle)


## validity_PeriodCohortMulti <- function(object) {
##     year_min <- object@year_min
##     year_max <- object@year_max
##     width <- object@width
##     val <- demcheck::chk_x_min_max_integer(x_min = year_min,
##                                            x_max = year_max,
##                                            name_min = "year_min",
##                                            name_max = "year_max")
##     if (!isTRUE(val))
##         return(val)
##     val <- demcheck::chk_is_positive_scalar(x = width,
##                                             name = "width")
##     if ((year_max - year_min) %% width != 0L)
##         return(gettextf("difference between '%s' [%d] and '%s' [%d] not a multiple of '%s' [%d]",
##                         "year_max", year_max, "year_min", year_min, "width", width))
##     TRUE
## }

## setClass("PeriodMulti",
##          contains = "Period",
##          slots = c(year_min = "integer",
##                    year_max = "integer",
##                    width = "integer"),
##          validity = validity_PeriodCohortMulti)

## validity_PeriodCustom <- function(object) {
##     years <- object@years
##     val <- demcheck::chk_x_integer(x = years,
##                                    name = "years",
##                                    open_first = FALSE,
##                                    open_last = FALSE)
##     if (!isTRUE(val))
##         return(val)
##     TRUE
## }

## ## HAS_TESTS
## setClass("PeriodCustom",
##          contains = "Period",
##          slots = c(years = "integer"),
##          validity = validity_PeriodCustom)


## validity_PeriodCohortQuarter <- function(object) {
##     break_min <- object@break_min
##     break_max <- object@break_max
##     val <- demcheck::chk_break_min_max_date(break_min = break_min,
##                                             break_max = break_max,
##                                             unit = "quarter")
##     if (!isTRUE(val))
##         return(val)
##     TRUE
## }

## ## HAS_TESTS
## setClass("PeriodQuarter",
##          contains = "Period",
##          slots = c(break_min = "Date",
##                    break_max = "Date"),
##          validity = validity_PeriodCohortQuarter)


## validity_PeriodCohortMonth <- function(object) {
##     break_min <- object@break_min
##     break_max <- object@break_max
##     val <- demcheck::chk_break_min_max_date(break_min = break_min,
##                                             break_max = break_max,
##                                             unit = "month")
##     if (!isTRUE(val))
##         return(val)
##     TRUE
## }

## ## HAS_TESTS
## setClass("PeriodMonth",
##          contains = "Period",
##          slots = c(break_min = "Date",
##                    break_max = "Date"),
##          validity = validity_PeriodCohortMonth)


## ## Cohort

## validity_Cohort <- function(object) {
##     open_first <- object@open_first
##     val <- demcheck::chk_is_logical_flag(x = open_first,
##                                          name = "open_first")
##     if (!isTRUE(val))
##         return(val)
##     TRUE
## }

## setClass("Cohort",
##          contains = c("Intervals",
##                       "VIRTUAL"),
##          slots = c(open_first = "logical"),
##          validity = validity_Cohort)

## ## HAS_TESTS
## setClass("CohortSingle",
##          contains = "Cohort",
##          slots = c(year_min = "integer",
##                    year_max = "integer"),
##          validity = validity_PeriodCohortSingle)

## setClass("CohortMulti",
##          contains = "Cohort",
##          slots = c(year_min = "integer",
##                    year_max = "integer",
##                    width = "integer"),
##          validity = validity_PeriodCohortMulti)

## validity_CohortCustom <- function(object) {
##     open_first <- object@open_first
##     years <- object@years
##     val <- demcheck::chk_x_integer(x = years,
##                                    name = "years",
##                                    open_first = open_first,
##                                    open_last = FALSE)
##     if (!isTRUE(val))
##         return(val)
##     TRUE
## }

## setClass("CohortCustom",
##          contains = "Cohort",
##          slots = c(years = "integer"),
##          validity = validity_CohortCustom)


## setClass("CohortQuarter",
##          contains = "Cohort",
##          slots = c(break_min = "Date",
##                    break_max = "Date"),
##          validity = validity_PeriodCohortQuarter)


## setClass("CohortMonth",
##          contains = "Cohort",
##          slots = c(break_min = "Date",
##                    break_max = "Date"),
##          validity = validity_PeriodCohortMonth)




## ## Points ---------------------------------------------------------------------

## setClass("Points",
##          contains = c("DimScale",
##                       "VIRTUAL"))

## ## ExactAge


## setClass("ExactAge",
##          contains = c("Intervals",
##                       "VIRTUAL"),
##          slots = c(open_last = "logical"),
##          validity = validity_AgeGroup)

## validity_AgeGroupSingleQuarterMonth <- function(object) {
##     break_min <- object@break_min
##     break_max <- object@break_max
##     val <- demcheck::chk_x_min_max_integer(x_min = break_min,
##                                            x_max = break_max,
##                                            name_min = "break_min",
##                                            name_max = "break_max")
##     if (!isTRUE(val))
##         return(val)
##     TRUE
## }

## ## HAS_TESTS
## setClass("AgeGroupSingle",
##          contains = "AgeGroup",
##          slots = c(break_min = "integer",
##                    break_max = "integer",
##                    open_last = "logical"),
##          validity = validity_AgeGroupSingleQuarterMonth)


## validity_AgeGroupMulti <- function(object) {
##     break_min <- object@break_min
##     break_max <- object@break_max
##     width <- object@width
##     val <- demcheck::chk_x_min_max_integer(x_min = break_min,
##                                            x_max = break_max,
##                                            name_min = "break_min",
##                                            name_max = "break_max")
##     if (!isTRUE(val))
##         return(val)
##     val <- demcheck::chk_is_positive_scalar(x = width,
##                                             name = "width")
##     if ((break_max - break_min) %% width != 0L)
##         return(gettextf("difference between '%s' [%d] and '%s' [%d] not a multiple of '%s' [%d]",
##                         "break_max", break_max, "break_min", break_min, "width", width))
##     TRUE
## }

## ## HAS_TESTS
## setClass("AgeGroupMulti",
##          contains = "AgeGroup",
##          slots = c(break_min = "integer",
##                    break_max = "integer",
##                    width = "integer",
##                    open_last = "logical"),
##          validity = validity_AgeGroupMulti)


## validity_AgeGroupCustom <- function(object) {
##     breaks <- object@breaks
##     open_last  <- object@open_last
##     val <- demcheck::chk_x_integer(x = breaks,
##                                    name = "breaks",
##                                    open_first = FALSE,
##                                    open_last = open_last)
##     if (!isTRUE(val))
##         return(val)
##     TRUE
## }

## ## HAS_TESTS
## setClass("AgeGroupCustom",
##          contains = "AgeGroup",
##          slots = c(breaks = "integer",
##                    open_last = "logical"),
##          validity = validity_AgeGroupCustom)

## ## HAS_TESTS
## setClass("AgeGroupQuarter",
##          contains = "AgeGroup",
##          slots = c(break_min = "integer",
##                    break_max = "integer",
##                    open_last = "logical"),
##          validity = validity_AgeGroupSingleQuarterMonth)

## ## HAS_TESTS
## setClass("AgeGroupMonth",
##          contains = "AgeGroup",
##          slots = c(break_min = "integer",
##                    break_max = "integer",
##                    open_last = "logical"),
##          validity = validity_AgeGroupSingleQuarterMonth)


## ## Period

## ## We refer to 'year_min' and 'year_max', rather than
## ## 'break_min' and 'break_max' for periods
## ## and cohorts measured in years, because the labels
## ## for these periods and cohorts omit information
## ## on month, which is contained in the breaks

## setClass("Period",
##          contains = c("Intervals",
##                       "VIRTUAL"))


## validity_PeriodCohortSingle <- function(object) {
##     year_min <- object@year_min
##     year_max <- object@year_max
##     val <- demcheck::chk_x_min_max_integer(x_min = year_min,
##                                            x_max = year_max,
##                                            name_min = "year_min",
##                                            name_max = "year_max")
##     if (!isTRUE(val))
##         return(val)
##     TRUE
## }

## setClass("PeriodSingle",
##          contains = "Period",
##          slots = c(year_min = "integer",
##                    year_max = "integer"),
##          validity = validity_PeriodCohortSingle)


## validity_PeriodCohortMulti <- function(object) {
##     year_min <- object@year_min
##     year_max <- object@year_max
##     width <- object@width
##     val <- demcheck::chk_x_min_max_integer(x_min = year_min,
##                                            x_max = year_max,
##                                            name_min = "year_min",
##                                            name_max = "year_max")
##     if (!isTRUE(val))
##         return(val)
##     val <- demcheck::chk_is_positive_scalar(x = width,
##                                             name = "width")
##     if ((year_max - year_min) %% width != 0L)
##         return(gettextf("difference between '%s' [%d] and '%s' [%d] not a multiple of '%s' [%d]",
##                         "year_max", year_max, "year_min", year_min, "width", width))
##     TRUE
## }

## setClass("PeriodMulti",
##          contains = "Period",
##          slots = c(year_min = "integer",
##                    year_max = "integer",
##                    width = "integer"),
##          validity = validity_PeriodCohortMulti)

## validity_PeriodCustom <- function(object) {
##     years <- object@years
##     val <- demcheck::chk_x_integer(x = years,
##                                    name = "years",
##                                    open_first = FALSE,
##                                    open_last = FALSE)
##     if (!isTRUE(val))
##         return(val)
##     TRUE
## }

## ## HAS_TESTS
## setClass("PeriodCustom",
##          contains = "Period",
##          slots = c(years = "integer"),
##          validity = validity_PeriodCustom)


## validity_PeriodCohortQuarter <- function(object) {
##     break_min <- object@break_min
##     break_max <- object@break_max
##     val <- demcheck::chk_break_min_max_date(break_min = break_min,
##                                             break_max = break_max,
##                                             unit = "quarter")
##     if (!isTRUE(val))
##         return(val)
##     TRUE
## }

## ## HAS_TESTS
## setClass("PeriodQuarter",
##          contains = "Period",
##          slots = c(break_min = "Date",
##                    break_max = "Date"),
##          validity = validity_PeriodCohortQuarter)


## validity_PeriodCohortMonth <- function(object) {
##     break_min <- object@break_min
##     break_max <- object@break_max
##     val <- demcheck::chk_break_min_max_date(break_min = break_min,
##                                             break_max = break_max,
##                                             unit = "month")
##     if (!isTRUE(val))
##         return(val)
##     TRUE
## }

## ## HAS_TESTS
## setClass("PeriodMonth",
##          contains = "Period",
##          slots = c(break_min = "Date",
##                    break_max = "Date"),
##          validity = validity_PeriodCohortMonth)


## ## Cohort

## validity_Cohort <- function(object) {
##     open_first <- object@open_first
##     val <- demcheck::chk_is_logical_flag(x = open_first,
##                                          name = "open_first")
##     if (!isTRUE(val))
##         return(val)
##     TRUE
## }

## setClass("Cohort",
##          contains = c("Intervals",
##                       "VIRTUAL"),
##          slots = c(open_first = "logical"),
##          validity = validity_Cohort)

## ## HAS_TESTS
## setClass("CohortSingle",
##          contains = "Cohort",
##          slots = c(year_min = "integer",
##                    year_max = "integer"),
##          validity = validity_PeriodCohortSingle)

## setClass("CohortMulti",
##          contains = "Cohort",
##          slots = c(year_min = "integer",
##                    year_max = "integer",
##                    width = "integer"),
##          validity = validity_PeriodCohortMulti)

## validity_CohortCustom <- function(object) {
##     open_first <- object@open_first
##     years <- object@years
##     val <- demcheck::chk_x_integer(x = years,
##                                    name = "years",
##                                    open_first = open_first,
##                                    open_last = FALSE)
##     if (!isTRUE(val))
##         return(val)
##     TRUE
## }

## setClass("CohortCustom",
##          contains = "Cohort",
##          slots = c(years = "integer"),
##          validity = validity_CohortCustom)


## setClass("CohortQuarter",
##          contains = "Cohort",
##          slots = c(break_min = "Date",
##                    break_max = "Date"),
##          validity = validity_PeriodCohortQuarter)


## setClass("CohortMonth",
##          contains = "Cohort",
##          slots = c(break_min = "Date",
##                    break_max = "Date"),
##          validity = validity_PeriodCohortMonth)
