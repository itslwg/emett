#' OnlyCompleteTriageCategory
#'
#' Keeps only the patients with complete information on their triage category.
#' @param study.sample data.frame. The study sample. No default.
#' @param triage.category.label Character vector of length 1. Column name of triage category variable. Defaults to "tc" 
#' @param remove.missing Logical vector of length 1. If TRUE all observations with missing outcome, as detected by is.na, are removed from the sample. Defaults to TRUE.
#' @export
OnlyCompleteTriageCategory <- function(study.sample, triage.category.label = "tc",
                                       remove.missing = TRUE) {
    ## Error handling
    if (!is.data.frame(study.sample))
        stop("study.sample has to be a data frame")
    if (!is.character(triage.category.label) | !bengaltiger::IsLength1(triage.category.label))
        stop("age.variable.name has to be a character vector of length 1")
    subsample <- study.sample
    ## Remove missing
    subsample <- subsample[!is.na(subsample[, triage.category.label]), ]
    n.excluded <- nrow(study.sample) - nrow(subsample)
    ## Collate return list
    total.n.excluded <- n.excluded
    exclusion.text <- paste0(total.n.excluded, " excluded: \n\n",
                             "- ",
                             total.n.excluded, " had missing information on triage category \n\n")
    return.list <- list(exclusion.text = exclusion.text,
                        subsample = subsample)
    return(return.list)
}
