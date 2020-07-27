#' Evaluate With Rocr
#'
#' Evaluate the classifier performance using ROCR.
#' @param predictions Numeric vector. Model predictions. No default.
#' @param outcome.vector Numeric vector. The vector containing the outcome of interest. No default.
#' @param measures Character vector of length 1 or list. If the former, then the measure will be used as the measure paramter in the ROCR::perfmance method. If the latter, then the list should have with two params for ROCR::performance: measure and x.measure. Defaults to "auc"
#' @param only.return.estimate Logical vector of length 1. If TRUE only the performance estimate is returned, else the full ROCR::performance instance is returned. Defaults to TRUE.
#' @export
EvaluateWithRocr <- function(predictions, outcome.vector, measures = "auc",
                             only.return.estimate = TRUE) {
    
    pred.rocr <- ROCR::prediction(predictions = predictions, labels = outcome.vector)

    rocr.perf <- ""
    if (!is.list(measures))
        rocr.perf <- ROCR::performance(pred.rocr, measure = measures)
    else
        rocr.perf <- ROCR::performance(pred.rocr, measure = measures$measure, x.measure = measures$x.measure)

    return.object <- rocr.perf
    if (only.return.estimate)
        return.object <- rocr.perf@y.values[[1]]

    return (return.object)
 }
