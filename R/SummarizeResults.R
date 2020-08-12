#' SummarizeResults
#'
#' Plots the performance metrics, creates tables, and compiles manuscript.
#' @param modelling.list List. Output from the emett::RunModelling function. No default
#' @export
SummarizeResults<- function(modelling.list) {
    results <- readRDS("results.Rds")
    settings <- list(
        s30d.results=list(
            triage.comparison.labels=c("Survived", "Died")
        ),
        composite.results=list(
            triage.comparison.labels=c("Non-event", "Event")
        )
    )
    for (nm in names(modelling.list)) {
        CreateRocPlot(modelling.list[[nm]]$predictions.list,
                      file.name=paste0("roc.plot.", sub("\\..*", "", nm)),
                      device = "pdf")
        CreateTriageComparisonPlot(modelling.list[[nm]]$predictions.list,
                                   file.name=paste0("triage.comparison.plot.",  sub("\\..*", "", nm)),
                                   pretty.labels=settings[[nm]]$triage.comparison.labels,
                                   device = "pdf")
    }
    ## Create table of estimates
    estimates.with.ci <- ComputeConfidenceIntervals(boot.object = modelling.list$s30d.results$statistics,
                                                    type = "basic")
    CreateEstimatesTable(estimates.with.ci, table.name = "estimates.table",
                         footnote = "latex", escape = FALSE, booktabs = TRUE,
                         file.format = "rmd")
    ## Create classification tables, i.e. reclassification tables
    tables <- CreateClassificationTables(modelling.list$s30d.results$predictions.list)
    ## Create coefficients table, i.e. measures for each individual model in SuperLearner
    CreateCoefficientsTable(sample=modelling.list$samples, outcomes=results$predictions.list$y.test,
                            pretty.model.nms = pretty.names,
                            booktabs = TRUE)
    ## Create classifier plots, i.e. roc plots for all different classifiers
    CreateClassifierPlots(sample=results$samples$test,
                          outcomes=results$predictions.list$y.test,
                          pretty.model.nms = pretty.names,
                          device = "pdf")
}
