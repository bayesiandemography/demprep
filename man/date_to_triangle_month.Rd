% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/date_to_triangle.R
\name{date_to_triangle_month}
\alias{date_to_triangle_month}
\title{Convert dates to month-length Lexis triangles}
\usage{
date_to_triangle_month(date, dob)
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
}
\value{
A character vector with the same length as \code{date}.
}
\description{
Based on an age-time plan where age groups and periods
both have widths of one month,
use dates of events and dates of birth
to allocate events to Lexis triangles.
}
\details{
The allocation of an event to a Lexis triangle depends
on the timing of the event and on the timing of changes
in age group. A person moves up into a new age group
once during each period. An event is allocated
to an upper Lexis triangle if the event occurs
before the move to the new age group.
An event is allocated to a lower Lexis triangle
if the event occurs with or after the move to the
new age group.

Consider, for instance, events occurring to a person
who was born on 11 March. \code{date_to_triangle_month}
allocates events to Lexis triangles as follows:
\tabular{ll}{
  \emph{Date of event} \tab \emph{Lexis triangle} \cr
  11 March to 31 March \tab \code{"Lower"} \cr
  1 April to 10 April \tab \code{"Upper"} \cr
  11 April to 30 April \tab \code{"Lower"} \cr
  1 May to 10 May \tab \code{"Upper"} \cr
  11 May to 31 May \tab \code{"Lower"} \cr
  etc \tab etc
}

\code{date} and \code{dob} must have the same length,
unless one of them has length 1, in which case the
length-1 argument is recycled.
}
\examples{
date_to_triangle_month(date = c("2024-03-27",
                                "2022-11-09"),
                       dob = "2020-01-01")
}
\seealso{
The output from \code{date_to_triangle_month}
is often processed further using function
\code{\link{format_triangle_month}}.

Other functions for creating
triangles from dates are
\code{\link{date_to_triangle_year}}
and \code{\link{date_to_triangle_quarter}}.

Other functions for creating one-month
units from dates are
\code{\link{date_to_age_month}}.
\code{\link{date_to_period_month}}, and
\code{\link{date_to_cohort_month}}.
}
