% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_age.R
\name{format_age_year}
\alias{format_age_year}
\title{Create consistent, complete single-year age groups}
\usage{
format_age_year(x, break_min = 0, break_max = 100, open_last = TRUE)
}
\arguments{
\item{x}{A vector of age group labels.}

\item{break_min}{An integer or \code{NULL}.
Defaults to 0.}

\item{break_max}{An integer or \code{NULL}.
Defaults to 100.}

\item{open_last}{Whether the final age group
has no upper limit. Defaults to \code{TRUE}.}
}
\value{
A factor with the same length as
\code{x}.
}
\description{
Given a vector of age group labels,
create a \code{\link[base]{factor}}
that contains levels for all ages between \code{break_min}
and \code{break_max}. The labels
may include an "open" age group with no upper limit. 
Apart from the open age
group, all the age groups have a width of one year.
}
\details{
Even when an age
group between \code{break_min} and \code{break_max}
is not included in \code{x}, \code{format_age_year}
still creates a level for it.

\code{x} must consist of labels for single-year
age groups, such as \code{"22"}, or
open age groups, such as \code{"100+"}.
\code{x} must not contain labels for
multi-year age groups such
as \code{"20-24"}.

If \code{break_min} or \code{break_max} is set to \code{NULL},
rather than to a specific value, then \code{format_age_year}
finds the narrowest range that accommodates the data.

All age groups in \code{x} must be single-year age groups,
except for any open age groups.

If \code{x} contains \code{NA}, then the
levels of the factor created by \code{format_age_year}
also contain \code{NA}.
}
\examples{
format_age_year(x = c("10", "3", "100+", "77"))

## allow 'break_min' and 'break_max' to be
## determined by the data
format_age_year(x = c(22, 7, 30),
                break_min = NULL,
                break_max = NULL)

## allow 'break_max' to be determined
## by the data, which includes an
## open age group
format_age_year(x = c("17", "10+"),
                break_max = NULL)

## oldest age group is closed
format_age_year(x = c(10, 3, 77),
                open_last = FALSE)
}
\seealso{
Other functions for reformating
age group labels are
\itemize{
  \item \code{\link{format_age_multi}}
  \item \code{\link{format_age_lifetab}}
  \item \code{\link{format_age_births}}
  \item \code{\link{format_age_custom}}
  \item \code{\link{format_age_quarter}}
  \item \code{\link{format_age_month}}
}

\code{\link{date_to_age_year}} creates
one-year age groups from dates.
}
