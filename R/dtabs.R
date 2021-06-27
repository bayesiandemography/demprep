
#' Cross-tabulation
#'
#' A simplified version of function
#' \code{\link[stats]{xtabs}}, designed specifically for
#' constructing demographic arrays. Although \code{dtabs}
#' can add up cell values, it is also useful for converting
#' a dataset from a data frame format into an array format.
#'
#' In \code{dtabs}, unlike in \code{xtabs}, the first argument
#' is \code{data}, which is convenient when working with pipes.
#'
#' The right hand side of the \code{formula} argument
#' should list the cross-classifying
#' variables, separated by \code{+} signs. The left hand side
#' can be blank, or can have a single variable.
#' If the left hand side is blank, \code{dtabs} adds up
#' the number of cases for each combination of the
#' cross-classifying variables. If the left hand side
#' has a variable, \code{dtabs} adds up the values for
#' that variable, for each combination of the
#' cross-classifying variables. A dot on the right hand side
#' is shorthand for "all variables not included on the left hand side".
#' Unlike \code{xtabs}, \code{dtabs} does not permit
#' the use of matrices on the left hand side.
#' 
#' \code{data} sometimes does not contain all possible
#' combinations of the cross-classifying variables.
#' When this happens, it may be appropriate to set
#' the cell values for these missing combinations to
#' zero. This is the appropriate response, for instance,
#' when \code{dtabs} is being used to enumerate the
#' number of cases for each combination of cross-classifing
#' variables. However, in some
#' circumstances, it may instead be appropriate
#' to set the cell values to \code{NA}. This is the appropriate
#' response when, for instance, coverting a collection
#' of demographic rates from a data frame format to
#' an array format.
#'
#' The value to be used for missing combinations of
#' cross-classifying variables is specified via \code{fill}.
#' If some combinations of the cross-classifying variables
#' are absent from the data but
#' no value for \code{fill} has been supplied, then
#' \code{dtabs} will try to derive a value. It will
#' set \code{fill} to \code{0} if \code{formula} does
#' not include a response, or if the response consists
#' entirely of positive integers. It will
#' set \code{fill} to \code{NA} if there is a response,
#' and that response contains non-positive numbers. If
#' neither of these rules can be applied, it will raise
#' an error.
#'
#' If \code{dtabs} cannot find a variable from \code{formula}
#' in \code{data} it will look for the variable in
#' the enclosing environment. This can be useful
#' in interactive use, but in production code, it is almost
#' always better to make sure that all the required variables
#' are included in \code{data}.
#' 
#' \code{dtabs} is designed for use with unweighted data,
#' such as individual-level data from a census or an administrative
#' source, or for individual-level data that
#' have already been aggregated.
#' For individual-level data with survey weights,
#' such data from a household survey, \code{\link{dtabs_survey}} may
#' be more appropriate.
#'
#' @param data A data frame or matrix.
#' @param formula A \code{\link[stats]{formula}},
#' with or without a response. See below
#' for examples, and also the documentation for \code{\link{xtabs}}.
#' @param na_rm Whether to remove \code{NA}s from the
#' response before tabulating it.
#' Defaults to \code{FALSE}.
#' @param fill The value to use in cells representing
#' combinations of cross-classifying variables
#' that do not occur in \code{data}.
#'
#' @return An array.
#'
#' @seealso \code{\link{xtabs}}, \code{\link{dtabs_survey}} 
#' 
#' @examples
#' ## with response
#' df <- data.frame(age = c("young", "old", "young", "old", "old", "young"),
#'                  sex = c("F", "M", "M", "M", "M", "F"),
#'                  count = c(1, 7, 2, 3, 3, 5))
#' dtabs(df, count ~ age + sex)
#' dtabs(df, count ~ age)
#' dtabs(df, count ~ .)
#'
#' ## without response
#' df <- data.frame(age = c("young", "old", "young", "old", "old", "young"),
#'                  sex = c("F", "M", "M", "M", "M", "F"))
#' dtabs(df, ~ age + sex)
#' dtabs(df, ~ age)
#' dtabs(df, ~ .)
#'
#' ## data with missing combination of variables
#' df_incomplete <- data.frame(age = c("young", "old", "young"),
#'                            sex = c("F", "F", "M"),
#'                            count = 1:3)
#' ## default value of fill is 0
#' dtabs(df_incomplete, count ~ age + sex)
#' ## use alternative value
#' dtabs(df_incomplete, count ~ age + sex, fill = NA)
#'
#' ## reformat data from data frame format to array format
#' ## (with no adding-up of values)
#' df_vals <- data.frame(country = c("NZ", "Australia", "NZ", "Australia"),
#'                       sex = c("Female", "Female", "Male", "Male"),
#'                       rate = c(0.023, 0.021, 0.017, 0.019))
#' dtabs(df_vals, rate ~ country + sex)
#' @export
dtabs <- function (data = parent.frame(),
                   formula = ~.,
                   na_rm = FALSE,
                   fill = NULL) {
    formula <- stats::as.formula(formula)
    if (!inherits(formula, "formula")) 
        stop(gettextf("'%s' missing or incorrect",
                      "formula"),
             call. = FALSE)
    if (any(attr(stats::terms(formula, data = data), "order") > 1L)) 
        stop(gettextf("formula '%s' has interactions",
                      paste(deparse(formula), collapse = " ")),
             Call. = FALSE)
    demcheck::err_is_logical_flag(x = na_rm,
                                  name = "na_rm")
    call <- match.call()
    if (is.matrix(eval(call$data, parent.frame()))) 
        call$data <- as.data.frame(data)
    call$na.action  <- quote(na.pass)
    i_arg <- match(c("data", "formula", "na.action"), names(call))
    call <- call[c(1L, i_arg)]
    call[[1L]] <- quote(stats::model.frame)
    model_frame <- eval(call, parent.frame())
    has_response <- length(formula) > 2L
    if (has_response) {
        i_response <- attr(attr(model_frame, "terms"), "response")
        X <- model_frame[[i_response]]
        nc <- NCOL(X)
        if (nc > 1L)
            stop(gettextf("response has %d columns",
                          nc),
                 call. = FALSE)
        INDEX <- model_frame[-i_response]
    }
    else {
        X <- rep(1L, nrow(model_frame))
        INDEX <- model_frame
    }
    not_fac <- !vapply(INDEX, is.factor, TRUE)
    INDEX[not_fac] <- lapply(INDEX[not_fac], factor)
    fill <- make_fill(fill = fill,
                      X = X,
                      INDEX = INDEX)
    tapply(X = X,
           INDEX = INDEX,
           FUN = sum,
           na.rm = na_rm,
           default = fill,
           simplify = TRUE)
}


#' Cross-tabulation of binary outcomes
#' in weighted survey data
#'
#' Tabulate binary outcomes, for combinations of
#' cross-classifying variables, from individual-level
#' survey data.
#'
#' The survey has a complex
#' design, involving techniques such as clustering
#' and stratification. The complex design implies
#' that different members
#' of the target population have different probabilities
#' of being included in the survey. These differential
#' inclusion probabilities complicate subsequent analyses,
#' including the calculation of means and variances.
#'
#' However, by exploiting information contained in the survey
#' weights, it is possible to calculate the "effective"
#' number of trials and successes for each
#' combination of the cross-classifying variables.
#' These effective numbers can be analysed as if
#' they were generated by simple random sampling.
#'
#' The effective number of trials and successes equals the
#' actual number of trials and successes
#' divided by the "design effect".
#' The method that \code{dtab_survey} uses for calculating
#' design effects implies that they are always
#' greater than or equal to one. The larger the design
#' effect, the less information the survey contains,
#' releative the the information that would be contained
#' in a simple random sample.
#'
#' The right hand side of the \code{formula} argument
#' should give the cross-classifying
#' variables, separated by \code{+} signs. The left hand side
#' should give the response variable. This variable must be
#' a binary outcome: a mixture of \code{1}s and \code{0}s
#' or a mixture of \code{TRUE}s and \code{FALSE}s.
#' A dot on the right hand side
#' is shorthand for "all variables not included
#' on the left hand side".
#'
#' The \code{weights} vector is typically a column
#' within \code{data}. It should consist entirely
#' of non-negative values. If a record has a weight
#' of 0, the record is removed from the dataset
#' before the calculations are made.
#'
#' The method for calculating design effects,
#' and hence effective numbers of trials and
#' successes, is specified by the \code{method}
#' argument. The default method, \code{"gg_med"}
#' is based on Ghitza and Gelman (2013),
#' and proceeds as follows:
#' \enumerate{
#'   \item Calculate design effect. This is proportional
#'     to the variance of the weights within each
#'     cell of the tabulation. An overall design
#'     effect is calculated by taking the median of
#'     the cell-level design effects.
#'   \item Calculate effective number of trials.
#'     The effective number of trials within each
#'     cell is calculated by dividing the (unweighted) number of
#'     responses in each cell by the overall design effect.
#'   \item Calculate weighted proportion of successes in
#'     each cell. The proportion of successes is calculated
#'     by taking the weighted mean of the outcome variable,
#'     with the weights equal to the survey weights.
#'   \item Calculate effective number of succcesses.
#'     Multiply the weighted proportion of successes in each
#'     cell by the effective number of trials in that
#'     cell.
#' }
#'
#' Ghitza and Gelman (2013) in fact calcuate the overall
#' design effect by taking the mean, rather than
#' the median, of the cell-level design effects.
#' The mean is, however, sensitive to outliers, as
#' can occur when cells have few observations.
#' The original method can be obtained by setting
#' \code{method} to \code{"gg_mean"}.
#'
#' The effective numbers of success and trials obtained
#' in this way will not generally be integers. To
#' perform further calculations on these data it may
#' be necessary to \code{\link{round}} them.
#'
#' If there are no observations for a particular
#' combination of the cross-classifying variables,
#' then the corresponding cell is not included in the
#' calculation of design effects, and the number of
#' effective trials and successes for that cell
#' are set to 0. If there is one observation for a particular
#' combination of the cross-classifying variables,
#' then a design effect for that cell cannot be calculated,
#' and the cell is ignored in the calculation of the overall
#' design effect. However, estimates of the effective
#' number of trials and successes for that cell are provided.
#' 
#' @inheritParams dtabs
#' @param formula A \code{\link[stats]{formula}}.
#' The formula must contain a response, and that
#' response must be a binary varibale.
#' @param weights A vector of weights. Required.
#' @param method The method used to calculate
#' the overall design effect. Defaults to \code{"gg_med"}.
#'
#' @return A list of two arrays. The first
#' array is named \code{"successes"}
#' and the second is named \code{"trials"}.
#'
#' @seealso \code{dtabs_survey} is a modified version of function
#' \code{\link{dtabs}}, which is designed for unweighted data,
#' such as administrative or census data, or
#' for aggregate-level data. \code{dtabs} is in turn
#' based on function \code{\link[stats]{xtabs}}.
#'
#' @source Ghitza Y and Gelman A. 2013. Deep interactions with MRP:
#' Election turnout and voting patterns among small electoral subgroups.
#' \emph{American Journal of Political Science}, 57(3): 762-776.
#'
#' For an alternative way of calculating effective
#' numbers of responses, see Chen C, Wakefield J, and Lumley T.
#' 2014. The use of sampling weights
#' in Bayesian hierarchical models for small area estimation.
#' \emph{Spatial and Spatio-temporal Epidemiology}. 11: 33-43.
#'
#' @examples
#' ## create a small synthetic dataset
#' df <- data.frame(is_obese = rbinom(n = 25,
#'                                    size = 1,
#'                                    prob = 0.3),
#'                  age = sample(c("10-14", "15-19", "20-24"),
#'                               size = 25,
#'                               replace = TRUE),
#'                  sex = sample(c("F", "M"),
#'                               size = 25,
#'                               replace = TRUE),
#'                  wt = runif(n = 25,
#'                             min = 5,
#'                             max = 40))
#'
#' ## obtain tabulations
#' dtabs_survey(df, is_obese ~ age + sex, weights = wt)
#'
#' ## just for comparison, obtain tabulations without
#' ## accounting for survey design
#' dtabs(df, is_obese ~ age + sex)
#' @export
dtabs_survey <- function (data = parent.frame(),
                          formula,
                          weights,
                          na_rm = FALSE,
                          method = c("gg_med", "gg_mean")) {
    ## check and tidy inputs
    formula <- stats::as.formula(formula)
    if (!inherits(formula, "formula")) 
        stop(gettextf("'%s' missing or incorrect",
                      "formula"),
             call. = FALSE)
    if (length(formula) < 3L)
        stop(gettextf("formula '%s' does not have a response",
                      paste(deparse(formula), collapse = " ")),
             call. = FALSE)
    if (any(attr(stats::terms(formula, data = data), "order") > 1L)) 
        stop(gettextf("formula '%s' has interactions",
                      paste(deparse(formula), collapse = " ")),
             call. = FALSE)
    if (missing(weights))
        stop(gettextf("argument '%s' is missing with no default",
                      "weights"),
             call. = FALSE)
    demcheck::err_is_logical_flag(x = na_rm,
                                  name = "na_rm")
    method <- match.arg(method)
    call <- match.call()
    if (is.matrix(eval(call$data, parent.frame()))) 
        call$data <- as.data.frame(data)
    ## create model frame
    call$na.action  <- quote(na.pass)
    i_args <- match(c("formula", "data", "weights", "na.action"), names(call))
    call <- call[c(1L, i_args)]
    call[[1L]] <- quote(stats::model.frame)
    model_frame <- eval(call, parent.frame())
    ## check weights
    i_wt <- match("(weights)", names(model_frame))
    wt <- model_frame[[i_wt]]
    demcheck::err_not_na_vector(x = wt,
                                name = "weights")
    demcheck::err_non_negative_vector(x = wt,
                                      name = "weights")
    if (all(wt == 0L))
        stop(gettextf("'%s' all zero",
                      "weights"))
    ## check response
    i_response <- attr(attr(model_frame, "terms"), "response")
    response <- model_frame[[i_response]]
    nc <- NCOL(response)
    if (nc > 1L)
        stop(gettextf("response has %d columns",
                      nc),
             call. = FALSE)
    if (!(is.logical(response) || all(response %in% c(0L, 1L, NA))))
        stop(gettextf("response contains values other than %s and %s or %d and %d",
                      "TRUE", "FALSE", 1L, 0L),
             call. = FALSE)
    ## calculations
    if (method %in% c("gg_med", "gg_mean")) {
        INDEX <- model_frame[-c(i_response, i_wt)]
        not_fac <- !vapply(INDEX, is.factor, TRUE)
        INDEX[not_fac] <- lapply(INDEX[not_fac], factor)
        response <- response[wt > 0L]
        INDEX <- INDEX[wt > 0L, , drop = FALSE]
        wt <- wt[wt > 0L]
        weights_identical <- length(wt) == 1L || all(wt[-1L] == wt[[1L]])
        ones <- rep(1L, times = nrow(INDEX))
        trials_unweighted <- tapply(X = ones,
                                    INDEX = INDEX,
                                    FUN = sum,
                                    na.rm = FALSE,
                                    default = 0L,
                                    simplify = TRUE)
        if (weights_identical) {
            successes_unweighted <- tapply(X = response,
                                           INDEX = INDEX,
                                           FUN = sum,
                                           na.rm = na_rm,
                                           default = NA,
                                           simplify = TRUE)
            ans <- list(successes = successes_unweighted,
                        trials = trials_unweighted)
        }
        else {
            if (all(trials_unweighted <= 1L))
                stop(gettext("no cells have 2 or more observations, so no design effects can be calculated"),
                     call. = FALSE)
            successes_weighted <- tapply(X = response * wt,
                                         INDEX = INDEX,
                                         FUN = sum,
                                         na.rm = na_rm,
                                         default = NA,
                                         simplify = TRUE)
            trials_weighted <- tapply(X = wt,
                                      INDEX = INDEX,
                                      FUN = sum,
                                      na.rm = FALSE,
                                      default = NA,
                                      simplify = TRUE)
            mean_weights <- tapply(X = wt,
                                   INDEX = INDEX,
                                   FUN = mean,
                                   na.rm = FALSE,
                                   default = NA,
                                   simplify = TRUE)
            sd_weights <- tapply(X = wt,
                                 INDEX = INDEX,
                                 FUN = stats::sd,
                                 na.rm = FALSE,
                                 default = NA,
                                 simplify = TRUE)
            if (all(sd_weights == 0))
                mean_design_effect <- 1L
            else {
                design_effect <- 1 + (sd_weights / mean_weights)^2
                design_effect <- design_effect[trials_unweighted >= 2L]
                FUN <- switch(method,
                              gg_med = median,
                              gg_mean = mean,
                              stop(gettextf("unexpected value for '%s' [\"%s\"]",
                                            "method", method)))                          
                overall_design_effect <- FUN(design_effect, na.rm = TRUE)
            }
            trials_effective <- trials_unweighted / overall_design_effect
            propn_weighted <- successes_weighted / trials_weighted
            successes_effective <- propn_weighted * trials_effective
            no_obs <- trials_unweighted == 0L
            successes_effective[no_obs] <- 0L
            trials_effective[no_obs] <- 0L
            ans <- list(successes = successes_effective,
                        trials = trials_effective)
        }
    } # method %in% c("gg_med", "gg_mean")
    ans
}





    
