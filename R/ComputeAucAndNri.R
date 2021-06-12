#' Compute Auc And Nri
#'
#' Fit the model to the training data, predict on the test data, and evaluate the predictions.
#' @param data Data frame. The study data. No default.
#' @param indices Allows the boot function to pick indices.
#' @param boot.sample Logical. If TRUE the sample is treated as a bootstrap sample. Then, indices are used by the boot package to resample the sample, and progress is logged as a bootstrap. Defaults to FALSE
#' @param log Logical. If TRUE progress is logged to a logfile. Defaults to TRUE
#' @param clean.start Logical. If TRUE logfile is removed and new information is logged. If FALSE information is appended to the logfile. Defaults to FALSE.
#' @export
ComputeAucAndNri <- function(data, indices, n.partitions = 3,
                             boot.sample = FALSE, log = TRUE,
                             clean.start = TRUE, ...) {
    ## Error handling
    if (!is.data.frame(data))
        stop ("data must be of type data frame")
    if (!bengaltiger::IsLength1(boot.sample) | !is.logical(boot.sample))
        stop ("boot must be of a logical vector of length 1")
    d <- data
    if (boot.sample)
        d <- data[indices, ]     ## Allow the boot function to pick indices
    ## Partition the sample, fit the model and predict on out-of-sample
    predictions.outcome.and.tc <- PartitionTrainAndPredict(study.sample = d,
                                                           boot.sample=boot.sample,
                                                           n.partitions = n.partitions,
                                                           ...)$predictions.list
    # predictions.outcome.and.tc <- results$s30d.results$predictions.list
    ## Evaluate AUC on the test set for both continuous and binned predictions
    l <- predictions.outcome.and.tc[grep("model", names(predictions.outcome.and.tc), value=TRUE)]
    model.aucs <- sapply(setNames(nm=names(l)), function(nm) {
        partition <- sub(".*\\.", "", nm)
        o <- predictions.outcome.and.tc[[paste0("y.", partition)]]
        e <- EvaluateWithRocr(
            predictions=l[[nm]],
            outcome.vector=o
        )
        return (e)
    })
    c <- predictions.outcome.and.tc[grep("tc", names(predictions.outcome.and.tc), value=TRUE)]
    clinician.aucs <- sapply(setNames(nm=names(c)), function(nm) {
        partition <- sub(".*\\.", "", nm)
        o <- predictions.outcome.and.tc[[paste0("y.", partition)]]
        e <- EvaluateWithRocr(
            predictions=c[[nm]],
            outcome.vector=o
        )
        return (e)
    })
    ## Compare model auc to clinician auc
    model.clinician.difference <- setNames(model.aucs - clinician.aucs,
                                           nm = paste0(names(model.aucs), ".clinician.diff"))
    ## Check if model performance is worse with binning
    partition.labels <- unique(sub(".*\\.", "", names(model.aucs)))
    con.cat.auc.difference <- unlist(lapply(partition.labels, function(l) {
        b <- grep(l, names(model.aucs))
        cut.con.diff = diff(model.aucs[b])
        diffs <- c(cut.con.diff, -cut.con.diff)
        names(diffs) <- paste0(c("cut.con.diff.", "con.cut.diff."), l)
        
        return (diffs)
    }))
    ## Compile aucs to one vector
    auc.vector <- c(model.aucs, clinician.aucs, model.clinician.difference, con.cat.auc.difference)
    ## Evaluate nri
    nri <- unlist(lapply(partition.labels, function(l) {
        b <- grep(sprintf("^(?!.*%s).*%s.*$", "con", l), names(predictions.outcome.and.tc), perl=TRUE)
        vec <- predictions.outcome.and.tc[b]
        nri <- EvaluateReclassification(
            current.model.predictions=vec[[grep("tc", names(vec))]],
            new.model.predictions=vec[[grep("model", names(vec))]],
            outcome.vector=vec[[grep("y", names(vec))]],
            )
        r <- c(nri["NRI+", ], nri["NRI-", ])
        names(r) <- paste0(c("nri.plus.", "nri.minus."), l)
        return(r)
    }))
    ## As vector of estimates for the boot package
    estimates <- c(auc.vector, nri)
    timestamp <- Sys.time()
    if (log) {
        analysis_name <- "Main"
        if (boot.sample)
            analysis_name <- "Bootstrap"
        logline <- paste0(analysis_name, " analysis completed on ", timestamp)
        append <- ifelse(clean.start, FALSE, TRUE)
        write(logline, "logfile", append = append)
    }   
    return (estimates)
}
