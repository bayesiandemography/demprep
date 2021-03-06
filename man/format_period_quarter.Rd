% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_period.R
\name{format_period_quarter}
\alias{format_period_quarter}
\title{Put period labels into the format required
for quarter (three-month) periods}
\usage{
format_period_quarter(x)
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
For instance, if the earliest period in \code{x}
is \code{"1990 Q1"}, and the latest is \code{"2010 Q4"},
then \code{format_period_quarter} creates a factor
with levels \code{"1990 Q1"}, \code{"1990 Q2"}, \dots,
\code{"2010 Q3"}, \code{"2010 Q4"}.
}
\details{
Quarters are defined as follows:
\tabular{lll}{
  \strong{Quarter} \tab \strong{Start} \tab \strong{End} \cr
  Q1 \tab 1 January \tab 31 March \cr
  Q2 \tab 1 April \tab 30 June \cr
  Q3 \tab 1 July \tab 30 September \cr
  Q4 \tab 1 October \tab 31 December
}

The elements of \code{x} must all be single quarters,
eg \code{"2001 Q2"} or \code{"2055 Q1"}.
Open intervals such as \code{"<2001 Q2"} are not
allowed.

If \code{x} contains \code{NA}, then the
levels of the factor created by \code{format_period_quarter}
also contain \code{NA}.
}
\examples{
format_period_quarter(x = c("2000 Q4", "2005 Q1", NA, "2004 Q3"))
}
\seealso{
Other functions for reformating
period labels are 
\code{\link{format_period_year}},
\code{\link{format_period_multi}},
\code{\link{format_period_custom}},
and \code{\link{format_period_month}}.

\code{\link{date_to_period_quarter}} creates
quarter periods from dates.
}
