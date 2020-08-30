#' SummarizeResults
#'
#' Plots the performance metrics, creates tables, and compiles manuscript. If modelling.list is not provided, the results are read from the results.Rds file.
#' @param modelling.list List. Output from the emett::RunModelling function. No default
#' @export
SummarizeResults<- function(modelling.list=NULL) {
    if (is.null(modelling.list)) {
        results <- readRDS("results.Rds")
        test.partitions <- readRDS("test.partitions.Rds")
        modelling.list <- results[c("s30d.results", "composite.results")]
        modelling.list$s30d.results$statistics <- results$s30d.statistics
        modelling.list$composite.results$statistics <- results$composite.statistics
        modelling.list$s30d.results$test.sample <- test.partitions$test.partition.composite
        modelling.list$composite.results$test.sample <- test.partitions$test.partition.s30d
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
    for (nm in names(modelling.list)) {
        CreateRocPlot(modelling.list[[nm]]$predictions.list,
                      file.name=paste0("roc.plot.", sub("\\..*", "", nm)),
                      device = "pdf")
        CreateTriageComparisonPlot(
            modelling.list[[nm]]$predictions.list,
            file.name=paste0("triage.comparison.plot.",  sub("\\..*", "", nm)),
            pretty.labels=settings[[nm]]$triage.comparison.labels,
            device = "pdf"
        )
        ## Create coefficients table, i.e. measures for each individual model in SuperLearner
        CreateCoefficientsTable(
            sample=modelling.list[[nm]]$test.sample,
            outcomes=modelling.list[[nm]]$predictions.list$y.test,
            booktabs = TRUE,
            row.names=FALSE
        )
    }
    ## Create table of estimates
    estimates.with.ci <- ComputeConfidenceIntervals(
        boot.object = modelling.list$composite.results$statistics,
        type = "basic"
    )
    CreateEstimatesTable(
        estimates.with.ci,
        table.name = "estimates.table",
        footnote = "latex",
        escape = FALSE,
        booktabs = TRUE,
        file.format = "pdf"
    )
    ## Create classification tables, i.e. reclassification tables
    tables <- CreateClassificationTables(modelling.list$s30d.results$predictions.list)
    predictions.lists <- lapply(modelling.list, "[[", "predictions.list")
    ## Create classifier plots, i.e. roc plots for all different classifiers
    CreateClassifierPlots(
        samples=lapply(modelling.list, "[[", "test.sample"),
        outcomes=lapply(predictions.lists, "[[", "y.test"),
        superlearner.object.paths=lapply(settings, "[[", "superlearner.object.path")
    )
}
