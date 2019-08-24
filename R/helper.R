
get_months <- function(abb = FALSE) {
    fmt <- if (abb) "%b" else "%B"
    format(ISOdate(2000, 1:12, 1), "%B")
}
