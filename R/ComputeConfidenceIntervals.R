#' Compute Confidence Intervals
#'
#' Generates confidence intervals around the estimates given in boot.object.
#' @param boot.object boot class. The boot object. No default.
#' @param ... Additional arguments for boot::boot.ci. 
#' @export
ComputeConfidenceIntervals <- function(boot.object, ...) {
    ## Error handling
    if (class(boot.object) != "boot")
        stop("boot.object must be of class boot")
    ## Calculate confidence intervals for each estimates
    ci.vector <- sapply(seq_along(boot.object$t0), function(i){
        ci.object <- boot::boot.ci(boot.object, index = i, ...)
        rounded.ci <- round(ci.object[[ci.object$call$type]], digits = 3)
        lower.and.upper <- tail(rounded.ci[1, ], n = 2)
        ci <- paste0("(", paste(lower.and.upper, collapse = " to "), ")")
        return (ci)
    })
    ## Paste the confidence intervals with the corresponding estimate
    estimates.with.ci <- paste(round(boot.object$t0, digits = 3), ci.vector)
    names(estimates.with.ci) <- names(boot.object$t0)
    return (estimates.with.ci)
}
