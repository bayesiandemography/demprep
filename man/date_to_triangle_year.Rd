% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/date_to_triangle.R
\name{date_to_triangle_year}
\alias{date_to_triangle_year}
\title{Convert dates to one-year Lexis triangles}
\usage{
date_to_triangle_year(date, dob, month_start = "Jan")
}
\arguments{
\item{date}{Dates of events.
A vector of class \code{\link[base]{Date}},
or a vector that can be coerced to class
\code{Date} using function \code{\link[base]{as.Date}}.}

\item{dob}{Dates of birth.
A vector of class \code{\link[base]{Date}},
or a vector that can be coerced to class
\code{Date} using function \code{\link[base]{as.Date}}.}

\item{month_start}{An element of \code{\link[base]{month.name}},
or \code{\link[base]{month.abb}}. The period starts on
the first day of this month.}
}
\value{
A character vector with the same length as \code{date}.
}
\description{
Use dates of events and dates of birth
to create one-year Lexis triangles.
}
\details{
See \code{vignette("demprep")} for the definition
of Lexis triangles.

\code{date} and \code{dob} must have the same length,
unless one of them has length 1, in which case the
length-1 argument is recycled.

Periods start on the first day of \code{month_start},
and end one-year-minus-one-day later.
The default value for \code{month_start} is \code{"Jan"},
so periods by default start on 1 January and
end on 31 December.
}
\examples{
date_to_triangle_year(date = c("2024-03-27",
                               "2022-11-09"),
                      dob = "2020-01-01")

## starts on 1 July rather than 1 January
date_to_triangle_year(date = c("2024-03-27",
                               "2022-11-09"),
                      dob = "2020-01-01",
                      month_start = "Jul")

## events occurring to people born during the
## period in question are always allocated to
## lower Lexis triangles
date_to_triangle_year(date = c("2020-03-19",
                               "2020-06-18"),
                      dob = c("2020-03-01",
                              "2020-06-18"))
}
\seealso{
The output from \code{date_to_triangle_year}
is often processed further using function
\code{\link{format_triangle_year}}.

Other functions for creating
triangles from dates are
\code{\link{date_to_triangle_quarter}}
and \code{\link{date_to_triangle_month}}.

Other functions for creating one-year
units from dates are
\code{\link{date_to_age_year}}.
\code{\link{date_to_period_year}}, and
\code{\link{date_to_cohort_year}}.
}
