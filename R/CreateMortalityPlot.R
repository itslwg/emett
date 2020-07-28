#' CreateMortalityPlot
#'
#' Passes plo
#' @param predictions.outcome.and.tc List. Predictions, outcome and clinicians triage as list. No default.
#' @param model.labels Character vector of length 1. Element names for model predictions in predictions.outcome.and.tc. If NULL those model predictions whose list label contain "test" but also "cut", "CUT", or "tc". Defaults to NULL.
#' @param file.name Character vector of length 1. File name to use for saving the plot. Defaults to NULL, and no plot is saved.
#' @param device Character vector of length 1. The device to use for saving ggplot. Defaults to "pdf".
#' @param outcome.label Character vector of length 1. List label of the outcome.variable. Defaults to "y.test". 
#' @param pretty.names Character vector of length 1. Pretty names for the models to use in the plot. If NULL, Defaults to c("SuperLearner", "Clinicians") 
#' @param save.plot.data.to.results Logical. If TRUE the plot data is saved to results. Defaults to TRUE.
#' @param ... Additional arguments for SavePlot.
#' @export
CreateMortalityPlot <- function(predictions.outcome.and.tc, file.name = NULL,
                                device = "pdf", model.labels = NULL,
                                outcome.label = "y.test",
                                pretty.names = c("SuperLearner", "Clinicians"),
                                save.plot.data.to.results = TRUE,
                                ...) {
    ## Error handling
    if (!is.list(predictions.outcome.and.tc))
        stop("predictions.outcome.and.tc must be list")
    nms <- names(predictions.outcome.and.tc)
    if (is.null(model.labels))
        model.labels <- nms[grepl("CUT|cut|tc", nms) & grepl("test", nms)]
    ## Make plot data
    plot.data <- do.call(rbind, mapply(function(x, pretty.name) {
        pred <- predictions.outcome.and.tc[[x]]
        outcome <- predictions.outcome.and.tc[[outcome.label]]
        mortality.to.category <- table(pred, outcome)
        n.survived <- mortality.to.category[, 1]
        n.died <- mortality.to.category[, 2]
        perc.died.y <- n.survived + n.died
        perc.died <- paste0(round(prop.table(mortality.to.category, margin = 1)[, 2] * 100), "%")
        data <- data.frame(levels = rep(rownames(mortality.to.category), 2),
                           y = c(n.survived, n.died), strata = rep(c("Survived", "Died"), each = 4),
                           perc.died.y = c(perc.died.y, rep(NA, 4)), perc.died = c(perc.died, rep(NA, 4)))
        data <- data.frame(data, x = rep(letters[1:4], 2), pretty.name = rep(pretty.name, nrow(data)))
        rownames(data) <- NULL
        return(data)
    }, model.labels, pretty.names, SIMPLIFY = FALSE))
    if (save.plot.data.to.results)
        bengaltiger::SaveToResults(plot.data, "mortality.plot.data")
    ## Create plot and save plot
    plt <- PlotMortality(plot.data)
    if (!is.null(file.name))
        SavePlot(plot.object = plt,
                 file.name = file.name,
                 device = device)
    return (plt)
}
