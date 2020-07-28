#' Create Estimates Table
#'
#' Generates the data frame for
#' @param estimates.with.ci Character vector. The performance estimates with correponding ci. No default.
#' @param caption Type. Description. Default/No default. 
#' @param pretty.colnames Character vector. The colnames of your choice. Defaults to c("AUROCC (95% CI)", "Model-Clinicians AUROCC difference (95% CI)", "Model-Model AUROCC difference (95% CI)")
#' @param pretty.rownames Character vector. The rownames of your choice. Defaults to c("SuperLearner_CON", "SuperLearner_CAT", "Clinicians")
#' @param return.table Logical. If TRUE the estimates table is returned. Defaults to FALSE.
#' @param save.to.disk Logical. If TRUE the table is saved to disk. Defaults to TRUE.
#' @param file.format Character vector of length 1.The format in which to save the table to disk. Has to be one of c("pdf", "rmd", "docx"). Defaults to "docx".
#' @param ... Additional arguments for MakeTable.
#' @export
CreateEstimatesTable <- function(estimates.with.ci, table.name = "estimates.table",
                                 caption = "AUROCC (95\\% CI), Model-model AUROCC difference (95\\% CI),
                                            and Model-Clincians AUROCC difference (95\\% CI).",
                                 pretty.colnames = c("AUROCC (95\\% CI)",
                                                     "Model-Clinicians AUROCC difference (95\\% CI)",
                                                     paste0("Model-Model",
                                                            kableExtra::footnote_marker_symbol(1, "latex"),
                                                            "AUROCC difference (95\\% CI)")),
                                 pretty.rownames = c("SuperLearner\\textsubscript{CON}",
                                                     "SuperLearner\\textsubscript{CAT}",
                                                     "Clinicians"),
                                 return.table = FALSE, save.to.disk = TRUE, file.format = "pdf",
                                 ...) {
    ## Extract model-model and model-clinicians AUC differences
    diff.aucs <- estimates.with.ci[grep("diff", names(estimates.with.ci))]
    ## Get indices with model-model AUC differences
    model.clinicians.indexes <- grepl("clinician", names(diff.aucs))
    ## Extract the AUC point estimates
    point.estimates = estimates.with.ci[!(names(estimates.with.ci) %in% names(diff.aucs)) &
                                        !grepl("nri", names(estimates.with.ci))]
    # Make estimate table
    df <- data.frame(point.estimates = point.estimates,
                     model.clinicians.auc.difference = c(diff.aucs[model.clinicians.indexes], NA),
                     model.model.auc.difference = c(diff.aucs[!model.clinicians.indexes], NA),
                     row.names = pretty.rownames, stringsAsFactors = FALSE)
    colnames(df) <- pretty.colnames
    df[is.na(df)] <- "Not applicable"
    ## Make estimates data frame with xtable
    tbl <- MakeTable(df, return.table = return.table,
                     table.name = table.name,
                     save.to.disk = save.to.disk,
                     file.format = file.format, ...)
    if (return.table)
        return (df)
}
