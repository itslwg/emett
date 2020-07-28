#' CreateMortalityTable
#'
#' Table reporting number (%) of patients that died in each triage category.
#' @param predictions.outcome.and.tc List. The predictions, outcome and clinicians triage in each partition. No default.
#' @param digits Numeric vector of length 1. Number of digits to use in table. Defaults to 2.
#' @param model.labels Character vector. The labels of the models in the predictions.outcome.and.tc list. If NULL the labels are set to those not equal to the outcome.label. Defaults to NULL.
#' @param outcome.label Character vector of length 1. The list label of the outcome variable in predictions.outcome.and.tc. Defaults to "s30d".
#' @param pretty.names Character vector. Model pretty names for the table. If NULL they are set are set as the model labels in predictions.outcome.and.tc. Defaults to NULL.
#' @param scores.to.invert Character vector. Model scores to invert. Defaults to NULL.
#' @param save.to.disk Logical. If TRUE the table is saved to disk. Defaults to FALSE.
#' @param file.format Character vector of length 1.The format in which to save the table to disk. Has to be one of c("pdf", "rmd", "docx"). Defaults to "docx".
#' @param ... Additional arguments for MakeTable. 
#' @export
CreateMortalityTable <- function(predictions.outcome.and.tc,
                                 digits = 2, model.labels = NULL,
                                 outcome.label = "s30d", pretty.names = NULL,
                                 scores.to.invert = NULL, save.to.disk = FALSE,
                                 file.format = "docx", ...) {
    if (!is.null(scores.to.invert))
        predictions.outcome.and.tc <- invert.levels(predictions.outcome.and.tc,
                                                    scores.to.invert)
    nms <- names(predictions.outcome.and.tc)
    if (is.null(model.labels))
        model.labels <- nms[!(nms %in% outcome.label)]   
    tbl <- as.data.frame(do.call(cbind, lapply(model.labels, function(model.label) {
        freq.tbl <- table(predictions.outcome.and.tc[[model.label]],
                          predictions.outcome.and.tc[[outcome.label]])
        prop <- paste0("(", round(prop.table(as.matrix(freq.tbl), 1) * 100, digits)[, 2], ")")
        mortality.tbl <- paste(freq.tbl[, 2], prop)
        return (mortality.tbl)
    })))
    rownames(tbl) <- c("Green", "Yellow", "Orange", "Red")
    if (!is.null(pretty.names)) {
        colnames(tbl) <- pretty.names   
    } else {
        colnames(tbl) <- model.labels
    }
    tbl <- as.data.frame(t(tbl))
    if (save.to.disk)
        MakeTable(df = tbl, "mortality.table",
                  save.to.disk = save.to.disk,
                  file.format = file.format,
                  ...)
    return (tbl)
}
