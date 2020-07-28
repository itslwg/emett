#' Boot Wrapper
#'
#' Wraps the boot function with a progress bar display
#' @param n.bootstrap.samples Numeric vector of length 1. Number of bootstrap samples to create. No default.
#' @param ... Arguments for the boot function
#' @export
BootWrapper <- function(n.bootstrap.samples, ...) {
    ComputeAucAndNri <- function(data, indices, show.progress, ...) {
        d <- data[indices, ]     ## Allow the boot function to pick indices
        ## Partition the sample, fit the model and predict on out-of-sample
        predictions.outcome.and.tc <- PartitionTrainAndPredict(study.sample = d, ...)
        ## Evaluate AUC on the test set for both continuous and binned predictions
        model.aucs <- with(predictions.outcome.and.tc, sapply(list(con.auc = con.model.test,
                                                                   cut.auc = cut.model.test),
                                                              EvaluateWithRocr, outcome.vector = y.test))
        clinicians.auc <- setNames(EvaluateWithRocr(predictions = predictions.outcome.and.tc$tc.test,
                                                    outcome.vector = predictions.outcome.and.tc$y.test),
                                   nm = "clinician.auc")
        ## Compare model auc to clinician auc
        model.clinician.difference <- setNames(model.aucs - clinicians.auc,
                                               nm = c("con.clinician.diff.auc", "cut.clinician.diff.auc"))
        ## Check if model performance is worse with binning
        con.cat.auc.difference <- model.aucs["con.auc"] - model.aucs["cut.auc"]
        con.cat.auc.difference <- setNames(c(con.cat.auc.difference, -con.cat.auc.difference),
                                           nm = c("con.cat.diff.auc", "cat.con.diff.auc"))
        ## Compile aucs to one vector
        auc.vector <- c(model.aucs, clinicians.auc, model.clinician.difference, con.cat.auc.difference)
        ## Evaluate nri on the test set
        nri <- invisible(with(predictions.outcome.and.tc,
                              EvaluateReclassification(current.model.predictions = tc.test,
                                                       new.model.predictions = cut.model.test,
                                                       outcome.vector = y.test)))
        nri <- unlist(list(nri.plus = nri["NRI+", ], nri.minus = nri["NRI-", ]))
        ## As vector of estimates for the boot package
        relevant.estimates <- c(auc.vector, nri)
        return (relevant.estimates)
    }
    statistics  <- boot::boot(statistic = progressReporter(total = n.bootstrap.samples,
                                                           nBars = 100,
                                                           f = ComputeAucAndNri),
                              R = n.bootstrap.samples,
                              ...)
    return (statistics)
}
