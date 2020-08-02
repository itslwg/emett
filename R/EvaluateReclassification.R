#' EvaluateReclassification
#'
#' Calculates the NRI.
#' @param current.model.predictions Numeric vector. The risk groups that the current model predicted for subjects. No default.
#' @param new.model.predictions Numeric vector. The risk groups that the new model predicted for subjects. No default.
#' @param outcome.vector Numeric vector. The outcome of the subjects. No default. 
#' @param reverse Logical. If TRUE model risk group levels are reversed, i.e. levels 1,2,3,4 are reversed to 4,3,2,1 - respectively. Defaults to FALSE.
#' @param return.all Logical vector of length 1. If TRUE reclassification table, with frequency of movement upwards and downwards in categories, is returned. Defaults to FALSE. 
#' @export
EvaluateReclassification <- function(current.model.predictions, new.model.predictions, outcome.vector,
                                     reverse = FALSE, return.all = FALSE) {

    if (reverse)
        new.model <- forcats::fct_rev(new.model)
    rng <- range(current.model.predictions)
    msg <- ifelse(return.all, TRUE, FALSE)
    suppressMessages({capture.output(reclassification.estimates <-
        InvisReclass(event = outcome.vector,
                     p.std = current.model.predictions,
                     p.new = new.model.predictions,
                     cut = seq(rng[1] + 1, rng[2]),
                     niter = 0,
                     msg = msg)  
        )})
    return.object <- reclassification.estimates$nri
    if (return.all)
        return.object <- reclassification.estimates
    return (return.object)
}
#' InvisReclass
#'
#' Run nricens::nribin but prevent it from plotting.
#' @param ... Arguments for nricens::nribin.
#' @export
InvisReclass<- function(...) {
    ff <- tempfile()
    png(filename = ff)
    n <- nricens::nribin(...) 
    dev.off()
    unlink(ff)
    return (n)
}
