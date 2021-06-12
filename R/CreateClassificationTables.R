#' Create classification tables function
#'
#' Function to create classification tables
#' @param predictions.outcome.and.tc The study sample object. No default.
#' @param return.raw Logical vector of length 1. If TRUE then returns the matrix of each classification table. If not, returns the xtable version. Defaults to TRUE. 
#' @export
CreateClassificationTables <- function(predictions.outcome.and.tc, return.raw = TRUE) {
    ## Error handling
    if (!is.list(predictions.outcome.and.tc))
        stop("Study sample has to be a list")
    ## Make list to hold tables
    table.list <- list()
    ## Settings for classification tables
    settings.classification.tables <- list(
        superlearner.train.table = list(outcome = predictions.outcome.and.tc$y.train,
                                        priorities = predictions.outcome.and.tc$cut.model.train,
                                        caption = paste0("Priority levels assigned by the binned SuperLearner prediction in the training sample (n = ", length(predictions.outcome.and.tc$y.train), ")"),
                                        label = "tab:superlearner.priorities.train"),
        superlearner.test.table = list(outcome = predictions.outcome.and.tc$y.test,
                                       priorities = predictions.outcome.and.tc$cut.model.test,
                                       caption = paste0("Priority levels assigned by the binned SuperLearner prediction in the test sample (n = ", length(predictions.outcome.and.tc$y.test), ")"),
                                       label = "tab:superlearner_priorities_test"),
        clinicians.test.table = list(outcome = predictions.outcome.and.tc$y.test,
                                     priorities = predictions.outcome.and.tc$tc.test,
                                     caption = paste0("Priority levels assigned by clinicians in the test sample (n = ", length(predictions.outcome.and.tc$y.test), ")"),
                                     label = "tab:clinicians_priorities_test")
    )
    ## Settings for reclassification tables
    settings.reclassification.tables <- list(
        reclass.all = list(superlearner.priorities = predictions.outcome.and.tc$cut.model.test,
                           clinicians.priorities = predictions.outcome.and.tc$tc.test,
                           caption = paste0("Priority levels assigned by SuperLearner and clinicians in complete test sample (n = ", length(predictions.outcome.and.tc$tc.test), ")"),
                           label = "tab:reclass_all"),
        reclass.events = list(superlearner.priorities = with(predictions.outcome.and.tc, cut.model.test[y.test == 1]),
                              clinicians.priorities = with(predictions.outcome.and.tc, tc.test[y.test == 1]),
                              caption = paste0("Priority levels assigned by SuperLearner and clinicians in test sample events (n = ", length(with(predictions.outcome.and.tc, tc.test[y.test == 1])), ")"),
                              label = "tab:reclass_events"),
        reclass.nonevents = list(superlearner.priorities = with(predictions.outcome.and.tc, cut.model.test[y.test == 0]),
                                 clinicians.priorities = with(predictions.outcome.and.tc, tc.test[y.test == 0]),
                                 caption = paste0("Priority levels assigned by SuperLearner and clinicians in test sample non-events (n = ", length(with(predictions.outcome.and.tc, tc.test[y.test == 0])), ")"),
                                 label = "tab:reclass_nonevents")
    )
    ## Make classification tables
    class.tables <- lapply(settings.classification.tables, function(setting) {
        with(setting, MakeSimpleTable(outcome,
                                      priorities,
                                      caption,
                                      label,
                                      return.raw))
    })
    ## Make reclassification tables
    reclass.tables <- lapply(settings.reclassification.tables, function(setting) {
        with(setting, MakeReclassificationTable(superlearner.priorities,
                                                clinicians.priorities,
                                                caption,
                                                label,
                                                return.raw))
    })
    ## Put them in the same list
    table.list <- c(class.tables, reclass.tables)
    return(table.list)
}
MakeSimpleTable <- function(outcome, priorities,
                            caption, label,
                            return.raw) {
    simple.table <- cbind(table(outcome, priorities), table(outcome))
    colnames <- colnames(simple.table)
    colnames[5] <- "Overall"
    simple_table <- matrix(paste0(simple.table, " (", round(prop.table(simple.table, margin = 2) * 100), ")"), nrow = 2, dimnames = list(NULL, paste0(colnames, " (\\%)")))
    simple.table <- cbind(c("No", "Yes"), simple.table)
    colnames(simple.table)[1] <- "All cause 30-day mortality"
    if (!return.raw)
        simple.table <- xtable::print.xtable(xtable::xtable(simple.table,
                                                            caption = paste0("\\bf ", caption),
                                                            label = label),
                                             table.placement = "!ht",
                                             include.rownames = FALSE,
                                             sanitize.text.function = function(x) x,
                                             print.results = FALSE,
                                             caption.placement = "top")
    return (simple.table)
}
MakeReclassificationTable <- function(superlearner.priorities,
                                      clinicians.priorities,
                                      caption, label,
                                      return.raw) {
    reclass.table <- table(clinicians.priorities, superlearner.priorities)
    mat <- as.matrix(reclass.table)
    reclass <- sapply(1:nrow(mat), function(i) round((1 - mat[i, i]/sum(mat[i, ])) * 100))
    reclass.up <- sapply(1:nrow(mat), function(i) {
        if(i < ncol(mat))
            round(sum(mat[i, (i + 1):ncol(mat)]) / sum(mat[i, ]) * 100)
        else
            "NA"
    })
    reclass.down <- sapply(1:nrow(mat), function(i) {
        if(i > 1)
            round(sum(mat[i, 1:(i - 1)]) / sum(mat[i, ]) * 100)
        else
            "NA"
    })
    reclass.table <- cbind(reclass.table, reclass, reclass.up, reclass.down)
    reclass.table <- cbind(rownames(reclass.table), reclass.table)
    rownames(reclass.table) <- NULL
    reclass.table[reclass.table == NaN | reclass.table == "NA"] <- ""
    if (return.raw) {
        column.names <- c(
            "Clinicians",
            "Green",
            "Yellow",
            "Orange",
            "Red",
            "Rec. %",
            "Rec. up %",
            "Rec. down %"
        )
        class.labels <- c("Green", "Yellow", "Orange", "Red")
        colnames(reclass.table) <- column.names
        reclass.table[, "Clinicians"] <- class.labels
        
        return(reclass.table)
    } else {
        reclass.xtable <- xtable::xtable(reclass.table,
                                         caption = paste0("\\bf ", caption),
                                         label = label)
        addtorow <- list()
        addtorow$pos <- list(0, 0)
        addtorow$command <- c("& \\multicolumn{4}{c}{SuperLearner} \\\\\n",
                              "Clinicians & Green & Yellow & Orange & Red & Rec. \\% & Rec. up \\% & Rec. down \\% \\\\\n")
        reclass.xtable <- xtable::print.xtable(reclass.xtable,
                                               add.to.row = addtorow,
                                               include.rownames = FALSE,
                                               include.colnames = FALSE,
                                               print.results = FALSE,
                                               caption.placement = "top",
                                               table.placement = "!ht")
        star.caption <- "Reclassification (Rec.) figures refer to \\% of patients reclassified by the SuperLearner compared to clinicians. Rec. up and Rec. down indicates \\% of patients reclassified to a higher or lower priority level respectively."
        reclass.xtable <- AddStarCaption(reclass.xtable, star.caption)
        
        return(reclass.xtable)
    }
}
