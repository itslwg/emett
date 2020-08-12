#' Gridsearch breaks function
#'
#' Performs a gridsearch to identify breaks to optimise a provided loss function
#' @param predictions Numeric vector. Predicted probabilites. No default.
#' @param outcome.vector Numeric vector. The outcome of interest. No default.
#' @param grid Numeric vector. All values to be combined and searched. Defaults to seq(0.01, 0.99, 0.01).
#' @param n.breaks Numeric vector of length 1. The number of breaks in the predicted probabilites, i.e. cut-points. Defaults to 3.
#' @param loss.function Character vector of length 1. The name of the loss metric, e.g. auc. For now any performance measure included in ROCR is accepted. Defaults to "auc".
#' @param sample Logical vector of length 1. If TRUE loss is only calculated for 10% of possible breaks. Defaults to TRUE.
#' @param parallel Logical vector of length 1. If TRUE the gridsearch is done in parallel. Defaults to FALSE.
#' @param n.cores Numeric vector of length 1. The number of cores to use for parallel computing. Defaults to NULL.
#' @param save.plot Logical vector of length 1. If TRUE a wireframe plot is saved to disk. Defaults to FALSE.
#' @param verbose Logical vector of length 1. If TRUE then information is printed as the gridsearch runs. Defaults to FALSE. Currently ignored.
#' @param maximise Logical vector of length 1. If TRUE, grid search maximizes performance metric. Else the gridsearch minimizes the performance metric. Defaults to TRUE.
#' @importFrom foreach %dopar%
#' @export
GridsearchBreaks <- function(predictions, outcome.vector,
                             grid = seq(0.01, 0.99, 0.01),
                             n.breaks = 3,
                             loss.function="auc",
                             sample = TRUE, parallel = FALSE,
                             n.cores = NULL, save.plot = FALSE,
                             verbose = FALSE, maximise = TRUE) {
    ## Verify that outcomes is a vector of binary data with 0 and 1
    if (!all(levels(as.factor(outcome.vector)) %in% c("0", "1")))
        stop("outcome.vector is not a vector of binary data with only 0 and 1")
    ## Verify that there are no missing values in predictions or outcomes
    if (!all(!is.na(predictions)) | !all(!is.na(outcome.vector)))
        stop("There is missing data in predictions or outcomes")
    ## Define grid
    breaks.to.search <- combn(grid, n.breaks, simplify = FALSE)
    ## Draw a random sample if sample is TRUE
    if (sample)
        breaks.to.search <- breaks.to.search[sample(1:length(breaks.to.search), ceiling(length(breaks.to.search) * 0.1))]
    if (verbose)
        message("Estimating loss for each of ", length(breaks.to.search), " possible cutpoints combinations")
    ## Do gridsearch
    if (parallel) {
        if (is.null(n.cores)) {
            message("You have not specified the number of cores so 2 will be used")
            n.cores <- 2
        }
        parallel.breaks <- split(breaks.to.search,
                                 cut(1:length(breaks.to.search),
                                     breaks = n.cores,
                                     include.lowest = TRUE))
        cl <- parallel::makeCluster(n.cores)
        doParallel::registerDoParallel(cl)
        message("Running gridsearch for optimal cutpoints in parallel on ", n.cores, " cores")
        parallel.list <- foreach::foreach(breaks.to.search = parallel.breaks) %dopar%
            lapply(breaks.to.search, EstimateLoss,
                   predictions=predictions,
                   outcome.vector=outcome.vector,
                   measure=loss.function)
        parallel::stopCluster(cl)
        loss.list <- do.call(c, parallel.list)
    } else {
        loss.list <- lapply(
            breaks.to.search,
            EstimateLoss,
            predictions=predictions,
            outcome.vector=outcome.vector,
            measure=loss.function
            )   
    }
    loss.data <- data.frame(breaks = do.call(rbind, breaks.to.search), auc = unlist(loss.list))
    ## Make and save plot
    if (save.plot)
        create.and.save.gridsearch.plot(loss.data)
    ## Find max or min of auc
    decreasing <- ifelse(maximise, TRUE, FALSE)
    optimise.order <- order(
        loss.data$auc,
        decreasing=decreasing
    )
    ## Get optimal cutoffs
    optimal.cutpoints <- unlist(loss.data[optimise.order, 1:n.breaks][1, ])

    return (optimal.cutpoints)
}
#' EstimateLoss
#'
#' Define function to estimate loss for each set of cut points (breaks).
#' @param predictions Numeric vector. Predicted probabilites. No default.
#' @param outcome.vector Numeric vector. The outcome of interest. No default.
#' @param breaks Numeric vector. Breaks to test for the model scroe. No default.
#' @param measure Character vector of length 1. The name of the loss metric, e.g. auc. For now any performance measure included in ROCR is accepted. No default.
EstimateLoss <- function(predictions, outcome.vector,
                         breaks, measure) {
    breaks <- c(-Inf, breaks, Inf)
    groupings <- as.numeric(cut(predictions, breaks = breaks))
    loss <- EvaluateWithRocr(
        predictions = groupings,
        outcome.vector = outcome.vector,
        measure = measure
    )

    return(loss)
}

