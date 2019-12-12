
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
    if (!identical(labels, c("Lower", "Upper"))) {
        labels <- sprintf("\"%s\"", labels)
        labels <- paste(labels, collapse = ", ")
        return(gettextf("'%s' [%s] not identical to \"%s\", \"%s\"",
                        "labels", labels, "Lower", "Upper"))
    }
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
    if (!identical(labels, c("Ins", "Outs"))) {
        labels <- sprintf("\"%s\"", labels)
        labels <- paste(labels, collapse = ", ")
        return(gettextf("'%s' [%s] not identical to \"%s\", \"%s\"",
                        "labels", labels, "Ins", "Outs"))
    }
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

setClass("LabIntegers",
         contains = "Labels",
         slots = c(int_min = "integer",
                   int_max = "integer"),
         validity = validity_LabIntegers)



## GroupsIntegers ------------------------------------------------------------

validity_GroupedInt <- function(object) {
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

setClass("LabGroupedInt",
         contains = c("Labels",
                      "VIRTUAL"),
         slots = c(breaks = "integer",
                   open_first = "logical",
                   open_last = "logical"))


setClass("LabGroupedIntEnumerations",
         contains = "LabGroupedInt")


setClass("LabGroupedIntEndpoints",
         contains = "LabGroupedInt")


## Dates ----------------------------------------------------------------------

validity_LabCalendar <- function(object) {
    break_min <- object@break_min
    break_max <- object@break_max
    open_first <- object@open_first
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
    val <- demcheck::chk_is_logical_flag(x = open_first,
                                         name = "open_first")
    if (!isTRUE(val))
        return(val)
    if (break_max < break_min)
        return(gettextf("'%s' [\"%s\"] is less than '%s' [\"%s\"]",
                        "break_max", "break_min"))
    TRUE
}

setClass("LabCalendar",
         contains = c("Labels",
                      "VIRTUAL"),
         slots = c(break_min = "Date",
                   break_max = "Date",
                   open_first = "logical"),
         validity = validity_LabCalendar)


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


## Durations ------------------------------------------------------------------


validity_LabDurations <- function(object) {
    break_min <- object@break_min
    break_max <- object@break_max
    open_last <- object@open_last
    for (name in c("break_min", "break_max")) {
        x <- slot(object, name)
        val <- demcheck::chk_is_non_negative_scalar(x = x,
                                                    name = name)
        if (!isTRUE(val))
            return(val)
    }
    val <- demcheck::chk_is_logical_flag(x = open_last,
                                         name = "open_last")
    if (!isTRUE(val))
        return(val)
    if (break_max < break_min)
        return(gettextf("'%s' [\"%d\"] is less than '%s' [\"%d\"]",
                        "break_max", "break_min"))
    TRUE
}

setClass("LabDurations",
         contains = c("Labels",
                      "VIRTUAL"),
         slots = c(break_min = "integer",
                   break_max = "integer",
                   open_last = "logical"),
         validity = validity_LabDurations)

setClass("LabDurationsQuarters",
         contains = "LabDurations")

setClass("LabDurationsMonths",
         contains = "LabDurations")
