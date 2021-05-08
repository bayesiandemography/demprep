
## Regular expressions used in parsing labels

## Years for period and cohort; all units for age

CONST_P_SINGLE <- "^[0-9]+$"

CONST_P_LOW_UP <- "^([0-9]+)-([0-9]+)$"

CONST_P_OPEN_FIRST <- "^<[0-9]+$"

CONST_P_OPEN_LAST <- "^[0-9]+\\+$"


## Quarters for period and cohort

CONST_P_SINGLE_QUARTER <- "^[0-9]+ Q[1-4]$"

CONST_P_OPEN_FIRST_QUARTER <- "^<[0-9]+ Q[1-4]$"

CONST_P_OPEN_LAST_QUARTER <- "^[0-9]+ Q[1-4]\\+$"


## Months for period and cohort

CONST_P_SINGLE_MONTH <- paste(sprintf("^[0-9]+ %s$", month.abb), collapse = "|")

CONST_P_OPEN_FIRST_MONTH <- paste(sprintf("^<[0-9]+ %s$", month.abb), collapse = "|")

CONST_P_OPEN_LAST_MONTH <- paste(sprintf("^[0-9]+ %s\\+$", month.abb), collapse = "|")


