% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/impute.R
\name{impute_dob}
\alias{impute_dob}
\title{Impute a vector of dates of birth}
\usage{
impute_dob(date, age_years = NULL, age_months = NULL)
}
\arguments{
\item{date}{A vector of dates on which age is measured.}

\item{age_years}{Age measured in years.}

\item{age_months}{Age measured in months.}
}
\value{
A vector of class \code{\link[base]{Date}}
}
\description{
Given dates and ages, randomly generate 
dates of birth. Ages
are measured in complete years and/or completed
months. The dates of birth are generated from a 
uniform distribution: all dates that fall within 
the range implied by the \code{date},
\code{age_years}, and \code{age_months} 
arguments have the same chance of being selected.
For instance, if \code{date} is \code{"2000-12-31"}
and \code{age} is \code{0}, then every date from
\code{"2000-01-01"} to \code{"2000-12-31"}
has the same 1/366 chance of being selected.
}
\details{
The range of possible dates depends on whether
age is specified in years or months.
If age is specified in years only, then
the range is one year. If age is specified in months,
or in a combination of years and months,
then the range is one month. See below for an example.

ADD DISCUSSION OF SUBTLETIES OF MEASURING AGE

\code{date} must have class \code{\link[base]{Date}},
or can be coerced to class \code{Date}
via function \code{\link[base]{as.Date}}.

\code{date} and \code{age} must have the same length.
}
\examples{
impute_dob(date = c("2000-06-30", "2001-01-01"),
           age_years = c(4, 10))
impute_dob(date = c("2000-06-30", "2001-01-01"),
           age_months = c(3, 11))
impute_dob(date = c("2000-06-30", "2001-01-01"),
           age_years = c(0, 4),
           age_months = c(3, 11))
## if age is specified in months, then the range of
## possible dates of birth is narrower than if it
## is specified in years
ans_years <- impute_dob(date = rep("2020-01-01", 100),
                        age_years = 1)
ans_months <- impute_dob(date = rep("2020-01-01", 100),
                         age_months = 12)
range(ans_years) # up to one year
range(ans_months) # up to one month
}
\seealso{
\code{\link{impute_date}}
}
