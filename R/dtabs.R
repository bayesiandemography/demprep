
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


