#' Create Estimates Table
#'
#' Generates the data frame to be fed into kableExtra::kbl to make a table of model and clinician performance estimates. 
#' @param estimates.with.ci Character vector. The performance estimates with correponding ci. No default.
#' @param pretty.colnames Character vector. The colnames of your choice. Defaults to c("AUROCC (95% CI)", "Model-Clinicians AUROCC difference (95% CI)", "Model-Model AUROCC difference (95% CI)")
#' @param pretty.rownames Character vector. The rownames of your choice. Defaults to c("SuperLearner_CON", "SuperLearner_CAT", "Clinicians")
#' @export
CreateEstimatesTable <- function(estimates.with.ci,
                                 pretty.colnames = c("AUROCC (95\\% CI)",
                                                     "Model-Clinicians AUROCC difference (95\\% CI)",
                                                     paste0("Model-Model",
                                                            kableExtra::footnote_marker_symbol(1, "latex"),
                                                            "AUROCC difference (95\\% CI)")),
                                 pretty.rownames = c("SuperLearner\\textsubscript{CON}",
                                                     "SuperLearner\\textsubscript{CAT}",
                                                     "Clinicians")) {
    ## Extract only the test statistics
    test.estimates.with.ci <- estimates.with.ci[grep("test", names(estimates.with.ci))]
    ## Extract model-model and model-clinicians AUC differences
    diff.aucs <- test.estimates.with.ci[grep("diff", names(test.estimates.with.ci))]
    ## Get indices with model-model AUC differences
    model.clinicians.indexes <- grepl("clinician", names(diff.aucs))
    ## Extract the AUC point estimates
    point.estimates <- test.estimates.with.ci[!(names(test.estimates.with.ci) %in% names(diff.aucs)) &
                                              !grepl("nri", names(test.estimates.with.ci))]
    # Make estimate table
    df <- data.frame(point.estimates = point.estimates,
                     model.clinicians.auc.difference = c(diff.aucs[model.clinicians.indexes], NA),
                     model.model.auc.difference = c(diff.aucs[!model.clinicians.indexes], NA),
                     row.names = pretty.rownames,
                     stringsAsFactors = FALSE)
    colnames(df) <- pretty.colnames
    df[is.na(df)] <- "Not applicable"

    return (df)
}
