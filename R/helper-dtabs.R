
## Functions to help with tabulation that end users do not use directly.

## HAS_TESTS
make_fill <- function(fill, X, INDEX) {
    stopifnot(is.data.frame(INDEX))
    stopifnot(all(sapply(INDEX, is.factor)))
    stopifnot(identical(length(X), nrow(INDEX)))
    if (is.null(fill)) {
        if (!is.null(X)) { 
            if (identical(length(X), 0L))
                return(0L)
            ## try to infer value of 'fill'
            X_obs <- stats::na.omit(X)
            if (length(X_obs) > 0L) {
                is_pos <- X_obs > 0L
                is_int <- X_obs == round(X_obs)
                if (all(is_pos & is_int))
                    return(0L)
                if (any(!is_pos))
                    return(NA_integer_)
            }
        }
        n_possible_combn <- prod(sapply(INDEX, nlevels))
        n_actual_combn <- nrow(unique(INDEX))
        if (n_actual_combn < n_possible_combn)
            stop(gettextf(paste("some combinations of the cross-classifying variables are not",
                                "included in the data, but no value for '%s' has been supplied"),
                          "fill"),
                 call. = FALSE)
        return(0L)
    }
    else {
        demcheck::err_length_1(x = fill,
                               name = "fill")
        if (is.na(fill))
            return(NA_integer_)
        if (is.numeric(fill)) {
            if (fill == round(fill))
                fill <- as.integer(fill)
            return(fill)
        }
        stop(gettextf("invalid value for '%s'",
                      "fill"))
    }
}    
