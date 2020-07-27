#' Partition Sample
#'
#' Partitions the sample using stratified random sampling of the outcome of interest. Attempts to maintain class distribution as observed in the study sample. Accepted number of partitions are 2 or 3. If n.partitions is 3, 1 - train.size) / 2 is partitioned into both the validation and test sets. 
#' @param study.sample Data frame. The study sample. No default
#' @param outcome.variable.name Character vector of length 1. The name of the outcome variable of interest. Defaults to "s30d"
#' @param n.partitions Numeric vector of length 1. The number of partitions to create. Either 2 of 3. If  Defaults to 2.
#' @param train.size Numeric vector of length 1. The proportion of the sample that goes into training set. Defaults to 0.6
#' @export
PartitionSample <- function(study.sample, outcome.variable.name = "s30d",
                            n.partitions = 2, train.size = 0.6) {
    ## Error handling
    if (!is.character(outcome.variable.name) | !bengaltiger::IsLength1(outcome.variable.name))
        stop("outcome.variable.name has to be a character vector of length 1")
    if (!(n.partitions %in% c(2, 3)))
        stop("n.partitions must be either 2 or 3")
    if (!is.factor(study.sample[, outcome.variable.name]))
        stop("The outcome of interest must be a factor for the partitioning to work properly.")
    ## Get the indices for the training set
    train.indices <- caret::createDataPartition(y = study.sample[, outcome.variable.name],
                                                p = train.size, list = FALSE)
    set.list <- list(train = study.sample[train.indices, ],
                     test = study.sample[-train.indices, ])
    if (n.partitions == 3) {
        validation.and.train.set <- set.list$test
        ## Get new indices for the validation set
        validation.indices <- caret::createDataPartition(y = validation.and.train.set[, outcome.variable.name],
                                                         p = 0.5, list = FALSE)
        set.list$validation <- validation.and.train.set[validation.indices, ]
        ## Change the test size accordingly
        set.list$test <- validation.and.train.set[-validation.indices, ]
    }
    set.list <- lapply(set.list, function(set) {
        ## Subset tc and outcome from the samples
        tc <- set$tc
        outcome <- set[, outcome.variable.name]
        ## Change factor levels of the outcome to 0s and 1s, and make numeric
        levels(outcome) <- c(0, 1)
        outcome <- as.numeric(as.character(outcome))   
        ## Remove the outcome of interest and clincians triage from
        set <- set[, !(names(set) %in% c(outcome.variable.name, "tc"))]
        return (list(x = set, y = outcome, tc = tc))
    })
    return (set.list)
}
