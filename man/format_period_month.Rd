% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_period.R
\name{format_period_month}
\alias{format_period_month}
\title{Create consistent, complete one-month periods}
\usage{
format_period_month(x)
}
\arguments{
\item{x}{A vector of period labels.}
}
\value{
A factor with the same length as
\code{x}.
}
\description{
Given a vector of period labels, create a
\code{\link[base]{factor}} that contains
levels for the earliest and latest periods in \code{x},
and for all periods in between.
}
\details{
The elements of \code{x} must all be single months,
eg \code{"2001 Feb"} or \code{"2055 Mar"}.

If \code{x} contains \code{NA}, then the
levels of the factor created by \code{format_period_month}
also contain \code{NA}.
}
\examples{
format_period_month(x = c("2000 Sep", "2005 May", NA, "2004 Jan"))
}
\seealso{
Other functions for reformating
period labels are
\itemize{
  \item \code{\link{format_period_year}}
  \item \code{\link{format_period_multi}}
  \item \code{\link{format_period_custom}}
  \item \code{\link{format_period_quarter}}
}

\code{\link{date_to_period_month}} creates
month periods from dates.
}
