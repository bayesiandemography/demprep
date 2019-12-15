

## make_labels ----------------------------------------------------------------


setMethod("make_labels",
          signature(object = "LabIntegers"),
          function(object) {
              int_min <- object@int_min
              int_max <- object@int_max
              include_na <- object@include_na
              make_labels_integers(int_min = int_min,
                                   int_max = int_max,
                                   include_na = include_na)
          })


setMethod("make_labels",
          signature(object = "LabGroupedIntEnumerations"),
          function(object) {
              breaks = object@breaks
              open_first <- object@open_first
              open_last <- object@open_last
              include_na <- object@include_na
              make_labels_grouped_int_enumerations(breaks = breaks,
                                                   open_first = open_first,
                                                   open_last = open_last,
                                                   include_na = include_na)
          })

setMethod("make_labels",
          signature(object = "LabGroupedIntEndpoints"),
          function(object) {
              breaks = object@breaks
              open_first <- object@open_first
              include_na <- object@include_na
              make_labels_grouped_int_endpoints(breaks = breaks,
                                                open_first = open_first,
                                                include_na = include_na)
          })


setMethod("make_labels",
          signature(object = "LabCalendarQuarters"),
          function(object) {
              break_min = object@break_min
              break_max = object@break_max
              open_first <- object@open_first
              include_na <- object@include_na
              make_labels_calendar_quarters_months(break_min = break_min,
                                                   break_max = break_max,
                                                   open_first = open_first,
                                                   include_na = include_na,
                                                   unit = "quarter")
          })

setMethod("make_labels",
          signature(object = "LabCalendarMonths"),
          function(object) {
              break_min = object@break_min
              break_max = object@break_max
              open_first <- object@open_first
              include_na <- object@include_na
              make_labels_calendar_quarters_months(break_min = break_min,
                                                   break_max = break_max,
                                                   open_first = open_first,
                                                   include_na = include_na,
                                                   unit = "month")
          })

setMethod("make_labels",
          signature(object = "LabDurationsQuarters"),
          function(object) {
              break_min = object@break_min
              break_max = object@break_max
              open_last <- object@open_last
              include_na <- object@include_na
              make_labels_duration_quarters_months(break_min = break_min,
                                                   break_max = break_max,
                                                   open_last = open_last,
                                                   include_na = include_na,
                                                   unit = "quarter")
          })

setMethod("make_labels",
          signature(object = "LabDurationsMonths"),
          function(object) {
              break_min = object@break_min
              break_max = object@break_max
              open_last <- object@open_last
              include_na <- object@include_na
              make_labels_duration_quarters_months(break_min = break_min,
                                                   break_max = break_max,
                                                   open_last = open_last,
                                                   include_na = include_na,
                                                   unit = "month")
          })

