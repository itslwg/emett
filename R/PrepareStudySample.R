#' PrepareStudySample
#'
#' Prepares the study data using the data dictionary.
#' @param study.sample data.frame. The study data frame. No default.
#' @param data.dictionary List. The data dictionary object. No default.
#' @export
PrepareStudySample <- function(study.sample, data.dictionary) {
    ## Error handling
    if (!is.data.frame(study.sample))
        stop("Study sample has to be type data frame.")
    ## If seqn in dataset, remove seqn to later bind it to the dataframe
    seqn.in.data <- FALSE
    if ("seqn" %in% names(study.sample)) {
        seqn <- study.sample$seqn 
        seqn[is.na(seqn)] <- 999 # seqn = 999 should not be NA
        study.sample$seqn <- NULL 
        seqn.in.data <- TRUE
    }
    ## Assign 999 as missing
    study.sample[study.sample == 999] <- NA
    ## Prepare study data using the data dictionary
    study.sample[] <- lapply(names(study.sample), function(n) {
        vdd <- data.dictionary[[n]] # Get variable specific data dictionary and assign that to vdd
        data <- study.sample[, n]
        if (vdd$t == "qual") {
            values <- vdd$vs # Get values
            if (values != ""){
                data <- as.factor(data)
                value.labels <- SplitLabels(vdd$vls) # Get value labels
                value.label.list <- lapply(strsplit(value.labels, "="), trimws, "both") # Make a list of value labels without white space
                labels <- unlist(lapply(value.label.list, function(x) x[2])) # Get labels
                levels(data) <- labels # Assign those to data
            }
        }
        return(data)
    })
    ## If seqn is in study data, add seqn again
    if (seqn.in.data)
        study.sample <- data.frame(study.sample, seqn = seqn)
    return(study.sample)
}
#' SplitLabels
#'
#' Helper function for PrepareStudySample
#' @param labels Character vector. Strings in which to remove commas commas and swap out backslashes. No default.
SplitLabels <- function(labels) {
    ## Remove quotation marks and split on commas
    labels <- trimws(gsub("\\\"", "", unlist(strsplit(labels, ","))))
    return (labels)
}
