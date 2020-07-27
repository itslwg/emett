#' Make learner weight, risk, and AUROCC table function
#'
#' This function generates a risk, learner weight and learner-AUROCC table.
#' @param sample data.frame. Data for SuperLearner predictions. No default.
#' @param outcomes Numeric vector. Outcome from the sample. No default.
#' @param table.name Character vector of length 1. Used for the file name and label. Defaults to "coeff.risk"
#' @param superlearner.object.path The path to the SuperLearner object as generated from the SuperLearner::SuperLearner() method. Default: "./superlearner.rds"
#' @param caption Character vector of length 1. Used as caption for caption in rendered table. Defaults to  "10 fold cross validated risk and area under the receiver operating curve characteristics (AUROCC) in the training sample, weight, and AUROCC in the test sample for SuperLearner and each included learner",
#' @param pretty.colnames Character vector. Column names for the rendered table. Defaults to c("AUROCC (95\\% CI)", "Model-Clinicians AUROCC difference (95\\% CI)", paste0("Model-Model", kableExtra::footnote_marker_symbol(1, "latex"), "AUROCC difference (95\\% CI)"))
#' @param pretty.model.nms Character vector. Rownames for rendered table. Defaults to c("SuperLearner", "GLMnet", "GLM", "Random Forest", "XGboost", "GAM")
#' @param footnote Character vector of length 1. Footnote for the table. Defaults to "The risk is equal to 1 - cross validated AUROCC. The weight is the weight given to each learner in the final SuperLearner. A weight of 0 means that the learner was not included. Abbreviations: GAM, Generalized Additive Model; GLM, Generalized Linear Model; XGboost, Extreme Gradient Boosting Machine"
#' @param return.table Logical vector of length 1. If TRUE the function returns the table. Defaults to FALSE.
#' @param save.to.disk Logical vector of length 1. If TRUE the table is saved to disk. Defaults to TRUE.
#' @param file.format Character vector of length 1. File format used for the saved table. Defaults to "pdf"
#' @param ... Additional arguments passes to MakeTable. 
#' @export
CreateCoefficientsTable <- function(sample, outcomes, table.name = "coeff.risk",
                                    superlearner.object.path = "./SuperLearner_s30d.rds",
                                    caption = "10 fold cross validated risk and area under
                                               the receiver operating curve characteristics
                                               (AUROCC) in the training sample, weight, and
                                               AUROCC in the test sample for SuperLearner and
                                               each included learner",
                                    pretty.colnames = c("Learner",
                                                        "Cross validated risk",
                                                        "Cross validated AUROCC",
                                                        "Weight",
                                                        "AUROCC in test sample"),
                                    pretty.model.nms = c("SuperLearner", "GLMnet", "GLM",
                                                         "Random Forest", "XGboost", "GAM"),
                                    footnote = "The risk is equal to 1 - cross validated AUROCC.
                                                The weight is the weight given to each learner in
                                                the final SuperLearner. A weight of 0 means that the
                                                learner was not included. Abbreviations: GAM Generalized
                                                Additive Model, GLM Generalized Linear Model, XGboost
                                                Extreme Gradient Boosting Machine.",
                                    return.table = FALSE, save.to.disk = TRUE, file.format = "pdf", ...) {
    ## Load SuperLearner object
    superlearner.object <- readRDS(superlearner.object.path)
    ## Error handling
    if (!is.list(superlearner.object))
        stop("Superlearner object is not a list.")
    ## Set learner names
    learner.names <- unlist(lapply(superlearner.object$SL.library$library$predAlgorithm,
                                   function(name) strsplit(name, ".", fixed = TRUE)))
    learner.names <- learner.names[!(learner.names %in% "SL")]
    ## Get predictons on training set from each model
    preds <- do.call(cbind, predict(superlearner.object, newdata = sample))
    colnames(preds) <- pretty.model.nms
    ## Initiate list and save preds columns, i.e. model predictions, to list
    list.of.predictions <- lapply(setNames(nm = colnames(preds)), function(nm) preds[, nm])
    ## Calculate AUC of learners
    auroccs <- lapply(list.of.predictions, function(predictions) EvaluateWithRocr(predictions, outcomes))
    ## Set table
    t.coeff.risk <- data.frame(Learner = pretty.model.nms,
                               cvRisk = c(NA, superlearner.object$cvRisk),
                               cvAUROCC = 1 - c(NA, superlearner.object$cvRisk),
                               Weight = c(NA, superlearner.object$coef),
                               AUROCC = unlist(auroccs),
                               stringsAsFactors = FALSE)
    t.coeff.risk[is.na(t.coeff.risk)] <- "Not applicable"
    t.coeff.risk[] <- lapply(t.coeff.risk, function(x) if (!is.character(x)) sprintf("%.3f", x) else x)
    colnames(t.coeff.risk) <- pretty.colnames
    MakeTable(t.coeff.risk,
              table.name = table.name,
              save.to.disk = save.to.disk,
              file.format = file.format,
              return.table = return.table,
              footnote = footnote, ...)
    if (return.table)
        return (t.coeff.risk)
}
#' RemoveWords
#'
#' Remove
#' @param str Character vector of length 1. The string in which words are to be removed. No default.
#' @param stopwords Character vector of length 1 or list of character vectors. The words to remove in str. No default.
RemoveWords <- function(str, stopwords) {
    ## User Mikkos answer at
    ## https://stackoverflow.com/questions/35790652/removing-words-featured-in-character-vector-from-string
    x <- unlist(strsplit(str, " "))
    paste(x[!x %in% stopwords], collapse = " ")
}
