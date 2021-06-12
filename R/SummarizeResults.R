#' SummarizeResults
#'
#' Plots the performance metrics, creates tables, and compiles manuscript. If modelling.list is not provided, the results are read from the results.Rds file.
#' @param modelling.list List. Output from the emett::RunModelling function. No default
#' @export
SummarizeResults<- function(modelling.list=NULL) {
    if (is.null(modelling.list)) {
        results <- readRDS("results.Rds")
        partitions <- readRDS("partitions.Rds")
        modelling.list <- results[c("s30d.results", "composite.results")]
        modelling.list$s30d.results$statistics <- results$s30d.statistics
        modelling.list$composite.results$statistics <- results$composite.statistics
        modelling.list$s30d.results$test.sample <- partitions$partitions.s30d$test$
        modelling.list$composite.results$test.sample <- partitions$partitions.composite$test$x
    }
    settings <- list(
        s30d.results=list(
            triage.comparison.labels=c("Survived", "Died"),
            superlearner.object.path="./SuperLearner_s30d.rds"
        ),
        composite.results=list(
            triage.comparison.labels=c("Non-event", "Event"),
            superlearner.object.path="./SuperLearner_composite.rds"
        )
    )
    nm <- names(modelling.list)[1]
    for (nm in names(modelling.list)) {
        suppressMessages({
            outcome.label <- sub("\\..*", "", nm)
            CreateRocPlot(
                modelling.list[[nm]]$predictions.list,
                file.name=paste0("roc.plot.", outcome.label),
                device = "pdf"
            )
            ## Compare the event levels in triage categories with clinicians
            CreateTriageComparisonPlot(
                modelling.list[[nm]]$predictions.list,
                file.name=paste0("triage.comparison.plot.", outcome.label),
                pretty.labels=settings[[nm]]$triage.comparison.labels,
                device = "pdf"
            )
            ## Create coefficients table, i.e. measures for each individual model in SuperLearner
            coeff.risk.table <- CreateCoefficientsTable(
                sample=modelling.list[[nm]]$test.sample,
                outcomes=modelling.list[[nm]]$predictions.list$y.test,
                )
            bengaltiger::SaveToResults(coeff.risk.table,
                                       paste0("coeff.risk.table.",  outcome.label))
            ## Create table of estimates
            estimates.with.ci <- ComputeConfidenceIntervals(
                boot.object = modelling.list[[nm]]$statistics,
                type = "basic"
            )
            bengaltiger::SaveToResults(estimates.with.ci, object.name=paste0("estimates.",  outcome.label))
            estimates.table <- CreateEstimatesTable(
                estimates.with.ci
            )
            ## Create classification tables, i.e. reclassification tables
            classification.tables <- CreateClassificationTables(modelling.list$s30d.results$predictions.list)
            bengaltiger::SaveToResults(classification.tables, paste0("classification.tables.",  outcome.label))
        })
    }
    predictions.lists <- lapply(modelling.list, "[[", "predictions.list")
    ## Create classifier plots, i.e. roc plots for all different classifiers
    CreateClassifierPlots(
        samples=lapply(modelling.list, "[[", "test.sample"),
        outcomes=lapply(predictions.lists, "[[", "y.test"),
        superlearner.object.paths=lapply(settings, "[[", "superlearner.object.path")
    )
}
