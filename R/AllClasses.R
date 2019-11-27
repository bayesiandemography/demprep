

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


validity_Period <- function(object) {
    open_first <- object@open_first
    val <- demcheck::chk_is_logical_flag(x = open_first,
                                         name = "open_first")
    if (!isTRUE(val))
        return(val)
    TRUE
}

setClass("Period",
         contains = c("DimScale",
                      "VIRTUAL"),
         slots = c(open_first = "logical",
                   open_last = "logical"),
         validity = validity_Period)

setClass("PeriodSingle",
         contains = "Period",
         slots = c(year_min = "integer",
                   year_max = "integer"))

setClass("PeriodMulti",
         contains = "Period",
         slots = c(year_min = "integer",
                   year_max = "integer",
                   width = "integer"))

setClass("PeriodCustom",
         contains = "Period",
         slots = c(year = "integer"))

setClass("PeriodQuarter",
         slots = c(break_min = "Date",
                   break_max = "Date"))

setClass("PeriodMonth",
         slots = c(break_min = "Date",
                   break_max = "Date"))

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

setClass("AgeGroupSingle",
         slots = c(break_min = "integer",
                   break_max = "integer",
                   open_last = "logical"))

setClass("AgeGroupMulti",
         slots = c(break_min = "integer",
                   break_max = "integer",
                   width = "integer",
                   open_last = "logical"))

setClass("AgeGroupCustom",
         slots = c(breaks = "integer",
                   open_last = "logical"))

setClass("AgeGroupQuarter",
         slots = c(break_min = "integer",
                   break_max = "integer",
                   open_last = "logical"))

setClass("AgeGroupMonth",
         slots = c(break_min = "integer",
                   break_max = "integer",
                   open_last = "logical"))










                   
         
                  
         
