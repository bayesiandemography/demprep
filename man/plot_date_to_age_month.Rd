% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_date_to_age.R
\name{plot_date_to_age_month}
\alias{plot_date_to_age_month}
\title{Depict the intervals created by
function 'date_to_age_month'}
\usage{
plot_date_to_age_month(date, dob)
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
}
\description{
Create a plot illustrating how function
\code{\link{date_to_age_month}} works.
}
\details{
\code{plot_date_to_age_month} is typically used for
learning or documentation, rather than for
actual data analysis.
}
\examples{
plot_date_to_age_month(date = c("2004-03-27",
                                "2004-11-09"),
                       dob = c("2003-03-21",
                               "2003-07-13"))
}
\keyword{internal}
