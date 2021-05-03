
## Specialised functions, used only by 'demprep' for making labels

## Characteristics:
##   - Use breaks rather than values
##   - No validity checking
##   - Create values for intermediate categories


## Age ------------------------------------------------------------------------

## HAS_TESTS
make_labels_age <- function(breaks, open_last, include_na) {
    x <- breaks_to_pairs(breaks = breaks,
                         open_first = FALSE,
                         open_last = open_last,
                         include_na = include_na)
    make_labels_quantities(x)
}


## Period ---------------------------------------------------------------------

## HAS_TESTS
make_labels_period_year <- function(break_min, break_max, include_na) {
    breaks <- seq.int(from = break_min,
                      to = break_max)
    x <- breaks_to_pairs(breaks = breaks,
                         open_first = FALSE,
                         open_last = FALSE,
                         include_na = include_na)
    make_labels_integers(x)
}

## HAS_TESTS
make_labels_period_custom <- function(breaks, include_na) {
    x <- breaks_to_pairs(breaks = breaks,
                         open_first = FALSE,
                         open_last = FALSE,
                         include_na = include_na)
    make_labels_intervals(x)
}

## HAS_TESTS
make_labels_period_quarter <- function(break_min, break_max, include_na) {
    breaks <- seq.Date(from = break_min,
                       to = break_max,
                       by = "quarter")
    x <- breaks_to_pairs(breaks = breaks,
                         open_first = FALSE,
                         open_last = FALSE,
                         include_na = include_na)
    make_labels_quarters(x)
}

## HAS_TESTS
make_labels_period_month <- function(break_min, break_max, include_na) {
    breaks <- seq.Date(from = break_min,
                       to = break_max,
                       by = "month")
    x <- breaks_to_pairs(breaks = breaks,
                         open_first = FALSE,
                         open_last = FALSE,
                         include_na = include_na)
    make_labels_months(x)
}


## Cohort ---------------------------------------------------------------------

## HAS_TESTS
make_labels_cohort_year <- function(break_min, break_max, open_first, include_na) {
    breaks <- seq.int(from = break_min,
                      to = break_max)
    x <- breaks_to_pairs(breaks = breaks,
                         open_first = open_first,
                         open_last = FALSE,
                         include_na = include_na)
    make_labels_integers(x)
}

## HAS_TESTS
make_labels_cohort_custom <- function(breaks, open_first, include_na) {
    x <- breaks_to_pairs(breaks = breaks,
                         open_first = open_first,
                         open_last = FALSE,
                         include_na = include_na)
    make_labels_intervals(x)
}

## HAS_TESTS
make_labels_cohort_quarter <- function(break_min, break_max, open_first, include_na) {
    breaks <- seq.Date(from = break_min,
                       to = break_max,
                       by = "quarter")
    x <- breaks_to_pairs(breaks = breaks,
                         open_first = open_first,
                         open_last = FALSE,
                         include_na = include_na)
    make_labels_quarters(x)
}

## HAS_TESTS
make_labels_cohort_month <- function(break_min, break_max, open_first, include_na) {
    breaks <- seq.Date(from = break_min,
                       to = break_max,
                       by = "month")
    x <- breaks_to_pairs(breaks = breaks,
                         open_first = open_first,
                         open_last = FALSE,
                         include_na = include_na)
    make_labels_months(x)
}


