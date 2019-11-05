
#' demprep: Tools for preparing demographic data
#'
#' General-purpose tools preparing raw demographic
#' data for further analysis.
#'
#' The tools fall into several groups:
#'
#' @section Processing dates:
#' Constructing age groups, periods, and cohorts
#' from dates. Functions such as
#' \code{\link{date_to_age_group_year}},
#' \code{\link{date_to_period_multi}}, or
#' \code{\link{date_to_cohort_month}}.
#'
#' @section Making labels:
#' Constructing labels for age groups, periods, and cohorts.
#' Functions such as 
#' \code{\link{make_labels_age_group}} or
#' \code{\link{make_labels_period_quarter}}.
#'
#' @section Tabulation:
#' Constructing arrays from data frames.
#' Functions
#' \code{\link{dtabs}} and
#' \code{\link{dtabs_survey}}.
#'
#' @docType package
#' @name demprep
NULL

