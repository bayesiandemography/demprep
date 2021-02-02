% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_date_to_age.R
\name{plot_date_to_age_quarter}
\alias{plot_date_to_age_quarter}
\title{Depict the intervals created by
function 'date_to_age_quarter'}
\usage{
plot_date_to_age_quarter(
  date,
  dob,
  break_max = 400,
  open_last = TRUE,
  show_months = FALSE
)
}
\arguments{
\item{date}{Dates of events or measurements.
A vector of class \code{\link[base]{Date}},
or a vector that can be coerced to class
\code{Date} using function \code{\link[base]{as.Date}}.}

\item{dob}{Dates of birth.
A vector of class \code{\link[base]{Date}},
or a vector that can be coerced to class
\code{Date} using function \code{\link[base]{as.Date}}.}

\item{break_max}{An integer or \code{NULL}.
Defaults to 400.}

\item{open_last}{Whether the final age group
has no upper limit. Defaults to \code{TRUE}.}

\item{show_months}{Whether to include vertical
lines showing boundaries between months.
Defaults to \code{FALSE}.}
}
\description{
Create a plot illustrating how function
\code{\link{date_to_age_quarter}} works.
}
\details{
\code{plot_date_to_age_quarter} is typically used for
learning or documentation, rather than for
actual data analysis.
}
\examples{
plot_date_to_age_quarter(date = c("2004-03-27",
                                  "2002-11-09"),
                         dob = c("2001-03-21",
                                 "2000-07-13"),
                         break_max = 12)

## alternative specifications for oldest age group
plot_date_to_age_quarter(date = "2019-09-22",
                         dob = "2018-01-01",
                         break_max = 4)
plot_date_to_age_quarter(date = "2019-09-22",
                         dob = "2018-01-01",
                         break_max = NULL)
plot_date_to_age_quarter(date = "2019-09-22",
                         dob = "2018-01-01",
                         break_max = NULL,
                         open_last = FALSE)
}