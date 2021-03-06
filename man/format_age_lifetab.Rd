% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_age.R
\name{format_age_lifetab}
\alias{format_age_lifetab}
\title{Put age group labels into the format
required for abridged life table}
\usage{
format_age_lifetab(x, break_max = 100)
}
\arguments{
\item{x}{A vector of age group labels.}

\item{break_max}{An integer or \code{NULL}.
Defaults to 100.}
}
\value{
A factor with the same length as \code{x}.
}
\description{
Given a vector of age group labels, create a factor that contains
levels for all ages between \code{0} and \code{break_max},
plus an open age group (ie an age group with no upper limit.)
These age groups are the ones typically used in
"abridged" (ie not single-year) life tables: \code{"0"},
\code{"1-4"}, \code{"5-9"}, \code{"10-14"}, and so on up to the
highest age group, which is always open.
}
\details{
\code{break_max} is used to specify
the oldest age group.
If \code{break_max} is \code{NULL}, the oldest
age group is derived from the data.

All age groups in \code{x} must fall within the intervals
\code{"0"}, \code{"1-4"}, \code{"5-9"}, \dots,
except for age groups above \code{break_max}.

If \code{x} contains \code{NA}, then the
levels of the factor created by \code{format_age_lifetab}
also contain \code{NA}.
}
\examples{
format_age_lifetab(x = c("100+", "14", "1-4"))

## set oldest age group to 50+
format_age_lifetab(x = c("100+", "14", "1-4"),
                   break_max = 80)
}
\seealso{
Other functions for reformating
age group labels are 
\code{\link{format_age_year}},
\code{\link{format_age_multi}},
\code{\link{format_age_births}},
\code{\link{format_age_custom}},
\code{\link{format_age_quarter}},
and \code{\link{format_age_month}}.

\code{\link{date_to_age_year}} calculates
ages from dates.
}
