#' Only informed consent
#'
#' Keeps only the patients that did provide informed consent.
#' @param study.sample Data frame. The study sample. No default.
#' @param informed.consent.variable Character vector of length 1. The name of the variable describing whether the patient did provide informed consent. Defaults to "ic".
#' @param remove.missing Logical vector of length 1. If TRUE all observations with missing information on informed consent, as detected by is.na, are removed from the sample. Defaults to TRUE.
#' @export
OnlyInformedConsent <- function(study.sample, informed.consent.variable = "ic",
                                remove.missing = TRUE) {
    ## Error handling
    if (!is.data.frame(study.sample))
        stop("study.sample has to be a data frame")
    if (!is.character(informed.consent.variable) | !bengaltiger::IsLength1(informed.consent.variable))
        stop("informed.consent.variable has to be a character vector of length 1")
    if (!is.logical(remove.missing) | !bengaltiger::IsLength1(remove.missing))
        stop("remove.missing has to be a logical vector of length 1")
    subsample <- study.sample
    ## Remove missing
    n.missing <- 0
    if (remove.missing) {
        subsample <- subsample[!is.na(subsample[, informed.consent.variable]), ]
        n.missing <- nrow(study.sample) - nrow(subsample)
    }
    ## Select those that did provide informed consent
    subsample <- subsample[subsample[, informed.consent.variable] == "Yes", ]
    n.excluded <- nrow(study.sample) - nrow(subsample) - n.missing
    ## Collate return list
    total.n.excluded <- n.excluded
    if (remove.missing)
        total.n.excluded <- total.n.excluded + n.missing
    exclusion.text <- paste0(total.n.excluded, " did not provide informed consent")
    if (remove.missing) {
        exclusion.text <- paste0(total.n.excluded, " excluded: \n\n",
                                 "- ", n.missing, " had missing information on consent \n\n",
                                 "- ", n.excluded, " did not provide informed consent \n\n")
    }
    return.list <- list(exclusion.text = exclusion.text,
                        subsample = subsample)
    return(return.list)
}
