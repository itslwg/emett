#' PlotReclassification
#'
#' Illustrates net reclassification improvement as heatmap.
#' @param predictions.list List. Predicted probabilities, cut probabilites, clinicians triage category and the outcome for both the training, (validattion) and test partitions. Assumes that the list elements have suffix "train" and "test" indicating study sample partition it belongs to. No default.
#' ... Arguments for EvaluateReclassification.
#' @export
PlotReclassification <- function(predictions.list,
                                 levels = c("Green", "Yellow", "Orange", "Red"),
                                 ...) {
    predictions.list <- results$predictions.list
    rc <- with(predictions.list, EvaluateReclassification(tc.test, cut.modebl.test, y.test,
                                                          return.all = TRUE))
    events.tbl <- unclass(rc$rtab.case) %>%
        `row.names<-`(levels) %>% `colnames<-`(levels)
    diag(events.tbl) <- 0
    multiples <- seq(3,1)
    m <- apply(seq_along(events.tbl), 1, function(n) {
        
    })
    plt.plus <- events.tbl %>%
        reshape2::melt() %>%
        ggplot2::ggplot(ggplot2::aes(x = Standard, y = New,
                                     fill = value, label = value)) +
        ggplot2::geom_tile() + 
        ggplot2::geom_text(ggplot2::aes(x = Standard, y = New, label = value),
                           color = "black") +
        ggplot2::scale_fill_gradient2(low = "blue", high = "red")
    
}
