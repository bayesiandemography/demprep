
#' demprep: Tools for preparing demographic data
#'
#' General-purpose tools for preparing demographic
#' data for further analysis.
#'
#' The raw data typically comes in the form of a data frame
#' with cross-classifying variables such as
#' age and sex. Data preparation typically requires two things:
#' \enumerate{
#'   \item reformatting the cross-classifying variables so that they
#' match the formats expected by packages such as \code{demarray}.
#'   \item cross-tabulating the data into a
#' multi-way \code{\link[base]{array}}.
#' }
#'
#' @section Reformatting Dates:
#'
#' If the raw data is at the individual level, ie if it has a record
#' for each person or each event, then information on ages and
#' times may be given as exact dates. The data may, for instance,
#' contain dates of birth or dates when events occurred.
#' Aggregate-level demographic analyses do not use exact dates
#' for events, but instead use intervals, such the age group "20-24",
#' or the period "2020-2030".
#' \code{demprep} has a large number of functions for converting
#' exact dates to intervals:
#' 
#' \tabular{llll}{
#'    \tab \strong{age} \tab \strong{time} \tab \strong{cohort} \cr
#'   Single year \tab \code{\link{date_to_age_group_year}} \tab \code{\link{date_to_period_year}} \tab \code{\link{date_to_cohort_year}} \cr
#'   Multi year, constant \tab \code{\link{date_to_age_group_multi}} \tab \code{\link{date_to_period_multi}} \tab \code{\link{date_to_cohort_multi}} \cr
#'   Multi year, varying \tab \code{\link{date_to_age_group_custom}} \tab \code{\link{date_to_period_custom}} \tab \code{\link{date_to_cohort_custom}} \cr
#'   Single quarter \tab \code{\link{date_to_age_group_quarter}} \tab \code{\link{date_to_period_quarter}} \tab \code{\link{date_to_cohort_quarter}} \cr
#'   Single month \tab \code{\link{date_to_age_group_month}} \tab \code{\link{date_to_period_month}} \tab \code{\link{date_to_cohort_month}} \cr
#'   Life table \tab \code{\link{date_to_age_group_lifetab}} \tab  \tab \cr
#'   Births \tab \code{\link{date_to_age_group_births}} \tab  \tab
#' }
#'
#' When doing demographic accounting, or when working with cohorts,
#' it may also be necessary to assign events with exact dates
#' to Lexis triangles, using the functions below:
#'
#' \tabular{ll}{
#'   Single year \tab \code{\link{date_to_triangle_year}} \cr
#'   Multi year, constant \tab \code{\link{date_to_triangle_multi}} \cr
#'   Single quarter \tab \code{\link{date_to_triangle_quarter}} \cr
#'   Single month \tab \code{\link{date_to_triangle_month}} \cr
#'   Births \tab \code{\link{date_to_triangle_births}}
#' }
#'
#' @section Reformating Labels:
#' 
#' If the raw data already uses intervals rather than dates
#' for events, it may be necessary to reformat the labels for
#' these intervals so that they match the conventions
#' used by packages such as \code{demarray}. Labels that match
#' these conventions can be generated using the following functions.
#' 
#' \tabular{llll}{
#'   \tab \strong{age} \tab \strong{time} \tab \strong{cohort} \cr
#'   Years \tab \code{\link{make_labels_age_group}} \tab \code{\link{make_labels_period}} \tab \code{\link{make_labels_cohort}} \cr
#'   Quarters \tab \code{\link{make_labels_age_group_quarter}} \tab \code{\link{make_labels_period_quarter}} \tab \code{\link{make_labels_cohort_quarter}} \cr
#'   Months \tab \code{\link{make_labels_age_group_month}} \tab \code{\link{make_labels_period_month}} \tab \code{\link{make_labels_cohort_month}}
#' }
#'
#' Some cross-classifying variables record quantitative variables other
#' than age, time, and cohort. For instance,
#' a population might be classified by current
#' income or number of children. Packages such as \code{demarray}
#' expect the labels for these sorts of variables to
#' follow conventions similar to those for age. Labels conforming
#' to these conventions can be generated using function
#' \code{\link{make_labels_enum}}.
#'
#' \emph{Describe the cleaning functions here when they are ready.}
#'
#' 
#' @section Tabulation:
#'
#' Once the cross-classifying variables are all in the required format,
#' the data can be tabulated into a multi-way
#' \code{\link[base]{array}}. When the data have already been aggregated
#' or when the data do not contain survey weights, the cross-tabulation
#' can be carried out using function \code{\link{dtabs}}. When
#' the data are at the individual level and come from a survey
#' that has a complex design, it may be better
#' to use function \code{\link{dtabs_survey}}.
#'
#' @docType package
#' @name demprep
NULL

