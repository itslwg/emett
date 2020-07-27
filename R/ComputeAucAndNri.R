#' Compute Auc And Nri
#'
#' Fit the model to the training data, predict on the test data, and evaluate the predictions.
#' @param data Data frame. The study data. No default.
#' @param indices Allows the boot function to pick indices.
#' @param boot Logical. If TRUE the sample is treated as a bootstrap sample. Then, indices are used by the boot package to resample the sample, and progress is logged as a bootstrap. Defaults to FALSE
#' @param log Logical. If TRUE progress is logged to a logfile. Defaults to TRUE
#' @param clean.start Logical. If TRUE logfile is removed and new information is logged. If FALSE information is appended to the logfile. Defaults to FALSE.
#' @export
ComputeAucAndNri <- function(data, indices, boot = FALSE,
                             log = TRUE, clean.start = TRUE,
                             ...) {
    ## Error handling
    if (!is.data.frame(data))
        stop ("data must be of type data frame")
    if (!bengaltiger::IsLength1(boot) | !is.logical(boot))
        stop ("boot must be of a logical vector of length 1")
    d <- data
    if (boot)
        d <- data[indices, ]     ## Allow the boot function to pick indices
    ## Partition the sample, fit the model and predict on out-of-sample
    predictions.outcome.and.tc <- PartitionTrainAndPredict(study.sample = d, ...)
    ## Evaluate AUC on the test set for both continuous and binned predictions
    model.aucs <- with(predictions.outcome.and.tc, sapply(list(con.auc = con.model.test, cut.auc = cut.model.test),
                                                          EvaluateWithRocr, outcome.vector = y.test))
    clinicians.auc <- list(clinician.auc = EvaluateWithRocr(predictions = predictions.outcome.and.tc$tc.test,
                                                            outcome.vector = predictions.outcome.and.tc$y.test))
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
    nri <- with(predictions.outcome.and.tc, EvaluateReclassification(current.model.predictions = tc.test,
                                                                     new.model.predictions = cut.model.test,
                                                                     outcome.vector = y.test))
    nri <- unlist(list(nri.plus = nri["NRI+", ], nri.minus = nri["NRI-", ]))
    ## As vector of estimates for the boot package
    relevant.estimates <- c(auc.vector, nri)
    timestamp <- Sys.time()
    if (log) {
        analysis_name <- "Main"
        if (boot)
            analysis_name <- "Bootstrap"
        logline <- paste0(analysis_name, " analysis completed on ", timestamp)
        append <- ifelse(clean.start, FALSE, TRUE)
        write(logline, "logfile", append = append)
    }   
    return (relevant.estimates)
}
