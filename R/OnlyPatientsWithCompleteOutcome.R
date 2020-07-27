#' Only patients with complete outcome
#'
#' Keeps only the patients with complete information about outcome.
#' @param study.sample data.frame. The study sample. No default.
#' @param outome.variable.name Character vector of length 1. The name of the variable with data on outcome. Defaults to "s30d".
#' @param remove.missing Logical vector of length 1. If TRUE all observations with missing outcome, as detected by is.na, are removed from the sample. Defaults to TRUE.
#' @export
OnlyPatientsWithCompleteOutcome <- function(study.sample, outcome.variable.name = "s30d",
                                            remove.missing = TRUE) {
    ## Error handling
    if (!is.data.frame(study.sample))
        stop("study.sample has to be a data frame")
    if (!is.character(outcome.variable.name) | !bengaltiger::IsLength1(outcome.variable.name))
        stop("age.variable.name has to be a character vector of length 1")
    subsample <- study.sample
    ## Remove missing
    subsample <- subsample[!is.na(subsample[, outcome.variable.name]), ]
    n.excluded <- nrow(study.sample) - nrow(subsample)
    ## Collate return list
    total.n.excluded <- n.excluded
    exclusion.text <- paste0(total.n.excluded, " excluded: \n\n",
                             "- ",
                             total.n.excluded, " had missing information on the outcome of interest \n\n")
    return.list <- list(exclusion.text = exclusion.text,
                        subsample = subsample)
    return(return.list)
}
