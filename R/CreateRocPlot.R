#' Create Roc Plot
#'
#' Plots receiver operating charecteristiqs curves or precision/recall curves using ROCR.
#' @param predictions.outcome.and.tc List. Predicted probabilities, cut predicted probabilites, clinicians triage category and the outcome for both the training and test partitions. Assumes that the list elements have suffix "train" and "test" indicating study sample partition it belongs to. No default.
#' @param file.name Character vector of length 1. File name to use for saving the plot. Defaults to "roc.plot".
#' @param device Character vector of length 1. The device to use for saving ggplot. Defaults to "pdf".
#' @param plot.type Character vector of length 1. Type of ROCR plot. Must be either "roc" or "prec.rec". Defaults to "roc". 
#' @param model.labels Character vector. The model labels in the predictions.outcome.and.tc list. Defaults to c("con.model.train","cut.model.train","con.model.test","cut.model.test", "tc.train", "tc.test")
#' @param pretty.names Character vector. The pretty labels for the model labels. The labels that are used in the plot. Defaults to c("SuperLearner continuous prediction","SuperLearner priority levels","SuperLearner continuous prediction", "SuperLearner priority levels", "Clinicians priority levels", "Clinicians priority levels")
#' @param roc.or.precrec String. To perform ROC or precision/recall analysis. Accepted values are "ROC" or "prec.rec". No default.
#' @param ... Arguments for helper function PlotRoc.
#' @export
CreateRocPlot <- function(predictions.outcome.and.tc,
                          file.name = "roc.plot",
                          dir.name = './figures/',
                          device = "pdf", plot.type = "roc",
                          model.labels = c("con.model.train",
                                           "cut.model.train",
                                           "con.model.validation",
                                           "cut.model.validation",                                           
                                           "con.model.test",
                                           "cut.model.test",
                                           "tc.train",
                                           "tc.validation",
                                           "tc.test"),
                          pretty.names = c("SuperLearner continuous prediction",
                                           "SuperLearner priority levels",
                                           "SuperLearner continuous prediction",
                                           "SuperLearner priority levels",
                                           "SuperLearner continuous prediction",
                                           "SuperLearner priority levels",
                                           "Clinicians priority levels",
                                           "Clinicians priority levels",
                                           "Clinicians priority levels"),
                          ...) {
    ## Error handling
    if (!is.list(predictions.outcome.and.tc))
        stop("predictions.outcome.and.tc must be of type list")
    if (length(model.labels) != length(pretty.names))
        stop("pretty.names vector must be the same length as model.labels")
    if (!(plot.type %in% c("roc", "prec.rec")))
        stop("Accepted values for roc.or.precrec is roc and prec.rec")
    ## Define setting depending on type of plot
    if (plot.type == "roc")
        measures <- list(tpr = "tpr",
                         fpr = "fpr",
                         TPR = "True positive rate",
                         FPR = "False positive rate")
    if (plot.type == "prec.rec")
        measures <- list(prec = "prec",
                         rec = "rec",
                         PREC = "Precision",
                         REC = "True positive rate (recall)")
    ## Get list of ROCR performance objects. One for each model.label
    tpr.fpr <- lapply(setNames(nm = model.labels), function(model) {
        ## Get the model extension
        outcome.label <- "y.train"
        if (grepl("validation", model)) {
            outcome.label <- "y.validation"
        } else if (grepl("test", model)) {
            outcome.label <- "y.test"
        }
        pred <- ROCR::prediction(predictions = predictions.outcome.and.tc[[model]],
                                 labels = predictions.outcome.and.tc[[outcome.label]])
        perf <- ROCR::performance(pred, measure = measures[[1]], x.measure = measures[[2]])
        return(perf)
    })
    ## Create plot data
    plot.data <- do.call(rbind, lapply(setNames(nm = model.labels), function(model) {
        rocr.data <- tpr.fpr[[model]]
        pretty.name <- pretty.names[grep(model, model.labels)]
        set <- "A"
        if (grepl("validation", model)){
            set <- "B"
        } else if (grepl("test", model)) {
            set <- "C"
        }
        new.data <- cbind(rocr.data@y.values[[1]], rocr.data@x.values[[1]])
        new.data <- data.frame(new.data, rep(set, nrow(new.data)), rep(pretty.name, nrow(new.data)))
        colnames(new.data) <- c(measures[[1]], measures[[2]], "set", "pretty.name")
        return(new.data)
    }))
    ## Create and save plots
    plt <- PlotRoc(plot.data = plot.data, y.name = measures[[1]],
                   x.name = measures[[2]], ylab = measures[[3]],
                   xlab = measures[[4]], ...)
    file.path <- paste0(dir.name, file.name)
    if (!is.null(file.name))
        SavePlot(plot.object = plt,
                 file.name = file.path,
                 device = device)
    suppressWarnings({
        bengaltiger::SaveToResults(plt, file.name)
    })
    return (plt)
}
