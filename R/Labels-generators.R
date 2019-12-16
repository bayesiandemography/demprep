
#' Create object of class "Label"
#'
#' Generator functions for objects of class
#' \code{\linkS4class{Label}}. End-users would not
#' normally call these functions directly.
#'
#' @param labels A chararacter vector with no
#' duplicates, blanks, or missing values,
#' listing all categories.
#' @param include_na Logical. Whether
#' to allow for an \code{NA} category.
#' @param breaks Integers or dates, defining
#' intervals.
#' @param break_min An integer or date, defining
#' interval limits.
#' @param break_max An integer or date, defining
#' interval limits.
#' @param int_min An integer, specifying the
#' bottom of the range of values.
#' @param int_max An integer, specifying the
#' top of the range of values.
#' @param open_first Logical. Whether the
#' first interval has a lower limit.
#' @param open_last Logical. Whether the
#' last interval has an upper limit.
#'
#' @return An object of class \code{\linkS4class{Label}}.
#' @name Label-generators
NULL

## HAS_TESTS
#' @export
#' @rdname Label-generators
LabCategories <- function(labels,
                          include_na) {
    labels <- as.character(labels)
    methods::new("LabCategories",
                 labels = labels,
                 include_na = include_na)
}

## HAS_TESTS
#' @export
#' @rdname Label-generators
LabTriangles <- function(include_na) {
    labels <- c("Lower", "Upper")
    methods::new("LabTriangles",
                 labels = labels,
                 include_na = include_na)
}

## HAS_TESTS
#' @export
#' @rdname Label-generators
LabPool <- function(include_na) {
    labels <- c("Ins", "Outs")
    methods::new("LabPool",
                 labels = labels,
                 include_na = include_na)
}

## HAS_TESTS
#' @export
#' @rdname Label-generators
LabQuantiles <- function(labels,
                         include_na) {
    methods::new("LabQuantiles",
                 labels = labels,
                 include_na = include_na)
}

## HAS_TESTS
#' @export
#' @rdname Label-generators
LabIntegers <- function(int_min,
                        int_max,
                        include_na) {
    methods::new("LabIntegers",
                 int_min = int_min,
                 int_max = int_max,
                 include_na = include_na)
}

## HAS_TESTS
#' @export
#' @rdname Label-generators
LabGroupedIntEnumerations <- function(breaks,
                                      open_first,
                                      open_last,
                                      include_na) {
    methods::new("LabGroupedIntEnumerations",
                 breaks = breaks,
                 open_first = open_first,
                 open_last = open_last,
                 include_na = include_na)
}

## HAS_TESTS
#' @export
#' @rdname Label-generators
LabGroupedIntEndpoints <- function(breaks,
                                   open_first,
                                   open_last,
                                   include_na) {
    methods::new("LabGroupedIntEndpoints",
                 breaks = breaks,
                 open_first = open_first,
                 open_last = open_last,
                 include_na = include_na)
}

## HAS_TESTS
#' @export
#' @rdname Label-generators
LabCalendarQuarters <- function(break_min,
                                break_max,
                                open_first,
                                open_last,
                                include_na) {
    methods::new("LabCalendarQuarters",
                 break_min = break_min,
                 break_max = break_max,
                 open_first = open_first,
                 open_last = open_last,
                 include_na = include_na)
}

## HAS_TESTS
#' @export
#' @rdname Label-generators
LabCalendarMonths <- function(break_min,
                              break_max,
                              open_first,
                              open_last,
                              include_na) {
    methods::new("LabCalendarMonths",
                 break_min = break_min,
                 break_max = break_max,
                 open_first = open_first,
                 open_last = open_last,
                 include_na = include_na)
}

## HAS_TESTS
#' @export
#' @rdname Label-generators
LabDurationsQuarters <- function(break_min,
                                 break_max,
                                 open_first,
                                 open_last,
                                 include_na) {
    methods::new("LabDurationsQuarters",
                 break_min = break_min,
                 break_max = break_max,
                 open_first = open_first,
                 open_last = open_last,
                 include_na = include_na)
}

## HAS_TESTS
#' @export
#' @rdname Label-generators
LabDurationsMonths <- function(break_min,
                               break_max,
                               open_first,
                               open_last,
                               include_na) {
    methods::new("LabDurationsMonths",
                 break_min = break_min,
                 break_max = break_max,
                 open_first = open_first,
                 open_last = open_last,
                 include_na = include_na)
}














