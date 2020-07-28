#' Set To Outcome
#'
#' This function changes patient outcome data if certain conditions are met. The outcome variable is set to dead if patients were dead on discharge after 30 days or if dead after 24 hours. Moreover, s30d is set to alive if coded alive and admitted to other hospital.
#' @param study.sample Data frame. The study data. No default
#' @export
SetToOutcome <- function(study.sample) {
    ## Test that study data if a data frame, stop if not
    if (!is.data.frame(study.sample))
        stop("study.sample must be a data frame")
    ## Set s30d to dead if we know that patients were dead on discharge or at 24
    ## hours
    study.sample[study.sample$hd == 1 & !is.na(study.sample$hd) | study.sample$s24h == 1 & !is.na(study.sample$s24h), "s30d"] <- "Yes"
    ## Finally remove  hd from the dataset
    study.sample <- study.sample[, -grep("hd", colnames(study.sample))]
    return (study.sample)
}
