#' RunModelling
#'
#' Run modelling with the pre-specified outcome variable.
#' @param prepared.sample data.frame The study sample, in its prepared form. No default. 
#' @param outcome.variable.name Character vector of length 1. Outcome variable for modelling. No default.
#' @param n.partitions Integer vector of length 1. Number of partitions for the study sample. Defaults to 3.
#' @param n.bootstrap.samples Integer vector of length 1. Number of bootstrap samples for bootstrap analysis. No default.
#' @param save.statistics Boolean vector of length 1. If TRUE, the results from the bootstrap resampling is saved to disk. Defaults to FALSE.
#' @param verbose Boolean vector of length 1. If TRUE messages describing the analysis steps is printed to console. Defaults to TRUE 
#' @export
RunModelling <- function(study.sample, outcome.variable.name, n.partitions=3,
                         n.bootstrap.samples, save.statistics=FALSE,
                         verbose=TRUE) {
    if (verbose)
        message(paste0(
            "\n~~~~~~~~~~~~~~~~~~~~~", paste0(rep("~", nchar(outcome.variable.name)), collapse=""), "\n",
            "Running modelling on ",
            outcome.variable.name,
            "\n~~~~~~~~~~~~~~~~~~~~~", paste0(rep("~", nchar(outcome.variable.name)), collapse=""), "\n"
        )
        )
    ## Partition sample, train, tune cut-points, and predict on a hold-out sample
    predictions.list <- PartitionTrainAndPredict(
        study.sample=study.sample,
        outcome.variable.name=outcome.variable.name,
        save.sample.predictions=TRUE,
        n.partitions=n.partitions,
        boot.sample=FALSE,
        use.fitted.sl=FALSE,
        verbose=verbose,
    )
    ## Generate point estimates and bootstrap estimates; Save estimates to results
    ## separately
    statistics <- BootstrapStatistics(
        f = ComputeAucAndNri,
        data=study.sample,
        outcome.variable.name = outcome.variable.name,
        R=n.bootstrap.samples,
        parallel="multicore",
        ncpus=4,
        save.sample.predictions=FALSE,
        log=TRUE,
        boot.sample=TRUE,
        use.fitted.sl=FALSE,
        verbose=verbose,
        return.samples=FALSE,
        n.partitions=n.partitions,
    )
    if (save.statistics)
        bengaltiger::SaveToResults(output.object = statistics, object.name = "statistics")

    return (statistics)
}

