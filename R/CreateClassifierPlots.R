#' Generate Precision/Recall and ROC-curves for classifiers
#'
#' Plots receiver operating characteristics and precision/recall curves of all learners included in SL.
#' @param sample data.frame Sample on which to base the predictions. No default.
#' @param outcomes Numeric vector. Outcome from the sample. No default.
#' @param superlearner.object.path The path to the SuperLearner object as generated from the SuperLearner::SuperLearner() method. Default: "./SuperLearner_s30d.rds"
#' @param pretty.model.nms Character vector. Pretty model names for plot. Defaults to c("SuperLearner", "GLMnet", "GLM", "Random Forest", "XGboost", "GAM")
#' @param ... Additional arguments for SavePlot. 
#' @export
CreateClassifierPlots <- function(sample, outcomes,
                                  superlearner.object.path = "./SuperLearner_s30d.rds",
                                  pretty.model.nms = c("SuperLearner",
                                                       "GLMnet",
                                                       "GLM",
                                                       "Random Forest",
                                                       "XGboost",
                                                       "GAM"),
                                  file.name = "roc.prec.rec", device = "pdf",
                                  ...) {
    ## Load model object
    superlearner.object <- readRDS(superlearner.object.path)
    ## Get predictions of SL learners from training set
    model.data <- data.frame(do.call(cbind, predict(superlearner.object, newdata = sample)))
    ## Initiate list to populate with dataframe columns and, then, fill
    predictions.list <- lapply(setNames(model.data, nm = pretty.model.nms), function(x) x)
    ## Get true positive and false positive rates
    measures <- list(measure = "tpr", x.measure = "fpr")
    tpr.fpr <- GetPerformanceList(predictions.list, measures, outcomes)
    roc.plot.data <- CreatePlotData(tpr.fpr, "A")
    ## Get recall and precision
    prec.rec <- GetPerformanceList(predictions.list, list(measure = "prec", x.measure = "rec"), outcomes)
    prec.plot.data <- CreatePlotData(prec.rec, "B")
    ## Create plots
    roc.plot <- PlotRoc(roc.plot.data)
    prec.rec.plot <- PlotRoc(prec.plot.data)
    ## Arrange plot grid
    combined.plot <- ggpubr::ggarrange(roc.plot, prec.rec.plot,
                                       ncol = 2,
                                       common.legend = TRUE,
                                       legend = "bottom",
                                       align = "hv")
    ## Save plot
    SavePlot(combined.plot,
             file.name = file.name,
             device = device,
             ...)
}
#' Creates a performance list for plotting
#' @param predictions.list List. No default.
#' @param outcomes Numeric vector. Outcome from the sample. No default.
GetPerformanceList <- function(predictions.list, measures, outcomes) {
    lapply(setNames(nm = names(predictions.list)), function(model) {
        EvaluateWithRocr(predictions.list[[model]], outcomes, measures, only.return.estimate = FALSE)
    })
}
#' Creates a performance list for plotting
#' @param perf.list List. Performance list created with GetPerformanceList. No default.
#' @param set Character vector of length 1. Identifier for plots. No default. 
CreatePlotData <- function(perf.list, set) {
    do.call(rbind, lapply(setNames(nm = names(perf.list)), function(model) {
        data <- perf.list[[model]]
        new.data <- cbind(data@y.values[[1]], data@x.values[[1]])
        y.name <- gsub(" ", ".", data@y.name)
        x.name <- gsub(" ", ".", data@x.name)
        if (x.name == "Recall")
            x.name <- paste0("True_positive_rate_BP", tolower(x.name), "EP")
        new.data <- data.frame(new.data,
                               rep(model, nrow(new.data)),
                               rep(set, nrow(new.data)))
        colnames(new.data) <- c(y.name, x.name, "pretty.name", "set")
        return(new.data)
    }))
}
