
#' Births in Iceland
#'
#' Individual-level data on births in Iceland,
#' 1981-2019. The data is partly synthetic in that
#' functions \code{\link{impute_date}} and
#' \code{\link{impute_dob}} have been used to
#' randomly generate dates from the original data
#' on year of birth and age of father.
#'
#' @format A data frame with 167,830 rows and two columns:
#' \describe{
#'   \item{dob_child}{The (imputed) date of birth of the child.}
#'   \item{dob_father}{The (imputed) date of birth of the father.}
#' }
#'
#' @source A modified version of dataset MAN05102, downloaded from the Statistics Iceland
#' website on 22 February 2021.
"icebirths"
