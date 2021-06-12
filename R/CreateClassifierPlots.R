#' CreateClassifierPlots
#'
#' Plots the ROC-curve for the learners included in the SuperLearner ensemble. If plotting multiple outcomes, use labelled lists using the data.frame outcome labels as the list labels suffixed with ".results". So, if using e.g. s30d and composite as outcomes, label the list with "s30d.results" and "composite.results".
#' @param samples data.frame or list of data.frames. The sample(s) upon which the predictions should be made. No default.
#' @param outcomes Numeric vector or list of numeric vectors. Outcome from the sample(s). No default.
#' @param superlearner.object.paths Character vector of length 1 or character vector. The path to the SuperLearner object(s) as generated from the SuperLearner::SuperLearner() method. Defaults to list(s30d.results="./SuperLearner_s30d.rds", composite.results="./SuperLearner_composite.rds")
#' @param pretty.model.nms Character vector. Pretty model names for plot. c("SuperLearner", "Random Forest", "Neural Net")
#' @param file.name Character vector of length 1. File name of the plot. Defaults to "learners.roc"
#' @param device Character vector of length 1. Device to use in SavePlot. Defaults to "pdf"
#' @param plot.labels Character vector of length 1 or character vector. Labels for the learners plots. E.g. if two outcomes, the labels for the first outcome will be A and for the second the plot label will be B. Defaults to list(s30="A", composite="B") 
#' @param ... Additional arguments for SavePlot. 
#' @export
CreateClassifierPlots <- function(samples, outcomes,
                                  superlearner.object.paths = list(
                                      s30d.results="./SuperLearner_s30d.rds",
                                      composite.results="./SuperLearner_composite.rds"
                                  ),
                                  pretty.model.nms = c("SuperLearner",
                                                       "XGBoost",
                                                       "GLMNet"),
                                  file.name = "learners.roc",
                                  dir.name = "./figures/", device = "pdf",
                                  plot.labels = list(s30d.results="A", composite.results="B"),
                                  ...) {
    plot.list <- lapply(names(samples), function(nm) {
        ## Load model object
        superlearner.object <- readRDS(superlearner.object.paths[[nm]])
        ## Get predictions of SL learners from training set
        model.data <- data.frame(do.call(cbind, predict(superlearner.object, newdata = samples[[nm]])))
        ## Initiate list to populate with dataframe columns and, then, fill
        predictions.list <- lapply(setNames(model.data, nm = pretty.model.nms), function(x) x)
        ## Get true positive and false positive rates
        measures <- list(measure = "tpr", x.measure = "fpr")
        tpr.fpr <- GetPerformanceList(predictions.list, measures, outcomes[[nm]])
        roc.plot.data <- CreatePlotData(tpr.fpr, plot.labels[[nm]])
        ## Create plots
        roc.plot <- PlotRoc(
            roc.plot.data,
            y,
            xlab = "False Positive Rate",
            ylab = "True Positive Rate",
        )
    })     
    ## Arrange plot grid
    combined.plot <- ggpubr::ggarrange(plotlist=plot.list,
                                       ncol = 2,
                                       common.legend = TRUE,
                                       legend = "bottom",
                                       align = "hv")
    file.path <- paste0(dir.name, file.name)
    ## Save plot
    SavePlot(combined.plot,
             file.name = file.path,
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
