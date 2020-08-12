#' CreateTriageComparsionPlot
#'
#' Creates a comparison plot of clinicians and models in predicting the outcome.
#' @param predictions.outcome.and.tc List. Predictions, outcome and clinicians triage as list. No default.
#' @param model.labels Character vector of length 1. Element names for model predictions in predictions.outcome.and.tc. If NULL those model predictions whose list label contain "test" but also "cut", "CUT", or "tc". Defaults to NULL.
#' @param file.name Character vector of length 1. File name to use for saving the plot. Defaults to NULL, and no plot is saved.
#' @param device Character vector of length 1. The device to use for saving ggplot. Defaults to "pdf".
#' @param outcome.label Character vector of length 1. List label of the outcome.variable. Defaults to "y.test". 
#' @param pretty.names Character vector of length 1. Pretty names for the models to use in the plot. If NULL, Defaults to c("SuperLearner", "Clinicians")
#' @param pretty.labels Character vector. Number of Default/No default. 
#' @param save.plot.data.to.results Logical. If TRUE the plot data is saved to results. Defaults to TRUE.
#' @param ... Additional arguments for SavePlot.
#' @export
CreateTriageComparisonPlot <- function(predictions.outcome.and.tc, file.name = NULL,
                                       device = "pdf", model.labels = NULL,
                                       outcome.label = "y.test",
                                       pretty.names = c("SuperLearner", "Clinicians"),
                                       pretty.labels = c("Survived", "Died"),
                                       save.plot.data.to.results = TRUE,
                                       ...) {
    ## Error handling
    if (!is.list(predictions.outcome.and.tc))
        stop("predictions.outcome.and.tc must be list")
    if (length(pretty.labels) != length(unique(predictions.outcome.and.tc[[outcome.label]])))
        stop("Number of elements in argument pretty.labels must match the number of categories in the outcome")
    nms <- names(predictions.outcome.and.tc)
    if (is.null(model.labels))
        model.labels <- nms[grepl("CUT|cut|tc", nms) & grepl("test", nms)]
    ## Make plot data
    plot.data <- do.call(rbind, mapply(function(x, pretty.name) {
        pred <- predictions.outcome.and.tc[[x]]
        outcome <- predictions.outcome.and.tc[[outcome.label]]
        outcome.to.category <- table(pred, outcome)
        n.non.event <- outcome.to.category[, 1]
        n.event <- outcome.to.category[, 2]
        perc.event.y <- n.non.event + n.event
        perc.event <- paste0(round(prop.table(outcome.to.category, margin = 1)[, 2] * 100), "%")
        data <- data.frame(levels = rep(rownames(outcome.to.category), 2),
                           y = c(n.non.event, n.event),
                           strata = rep(pretty.labels, each = 4),
                           perc.event.y = c(perc.event.y, rep(NA, 4)),
                           perc.event = c(perc.event, rep(NA, 4)))
        data <- data.frame(data, x = rep(letters[1:4], 2), pretty.name = rep(pretty.name, nrow(data)))
        rownames(data) <- NULL
        return(data)
    }, model.labels, pretty.names, SIMPLIFY = FALSE))
    if (save.plot.data.to.results)
        bengaltiger::SaveToResults(plot.data, "triage.comparison.plot.data")
    ## Create plot and save plot
    plt <- PlotTriageComparison(plot.data)
    if (!is.null(file.name))
        SavePlot(plot.object = plt,
                 file.name = file.name,
                 device = device)
    return (plt)
}
