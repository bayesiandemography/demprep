


LabCategories <- function(labels,
                          include_na) {
    methods::new("LabCategories",
                 labels = labels,
                 include_na = include_na)
}

LabTriangles <- function(include_na) {
    labels <- c("Lower", "Upper")
    methods::new("LabTriangles",
                 labels = labels,
                 include_na = include_na)
}

LabPool <- function(include_na) {
    labels <- c("Ins", "Out")
    methods::new("LabPool",
                 labels = labels,
                 include_na = include_na)
}

LabQuantiles <- function(labels,
                         include_na) {
    methods::new("LabQuantiles",
                 labels = labels,
                 include_na = include_na)
}

LabIntegers <- function(int_min,
                        int_max,
                        include_na) {
    methods::new("LabIntegers",
                 int_min = int_min
                 int_max = int_max,
                 include_na = include_na)
}

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

LabCalendarQuarters <- function(break_min,
                                break_max,
                                open_first,
                                include_na) {
    methods::new("LabCalendarQuarters",
                 break_min = break_min,
                 break_max = break_max,
                 open_first = open_first,
                 include_na = include_na)
}

LabCalendarMonths <- function(break_min,
                              break_max,
                              open_first,
                              include_na) {
    methods::new("LabCalendarMonths",
                 break_min = break_min,
                 break_max = break_max,
                 open_first = open_first,
                 include_na = include_na)
}

LabDurationsQuarters <- function(break_min,
                                break_max,
                                open_last,
                                include_na) {
    methods::new("LabDurationsQuarters",
                 break_min = break_min,
                 break_max = break_max,
                 open_last = open_last,
                 include_na = include_na)
}

LabDurationsMonths <- function(break_min,
                              break_max,
                              open_last,
                              include_na) {
    methods::new("LabDurationsMonths",
                 break_min = break_min,
                 break_max = break_max,
                 open_last = open_last,
                 include_na = include_na)
}














