#' PlotComparison
#'
#' Creates a stacked bar plot representing under-triage and over-triage in each triage category.
#' @param plot.data Data frame. The data for the mortality plot created with CreateTriageComparisonPlot. No default.
#' @param levels Character vector. Levels for the bars. Defaults to c("Green", "Yellow", "Orange", "Red")
#' @export
PlotTriageComparison <- function(plot.data, levels = c("Green", "Yellow", "Orange", "Red")) {
    if (!all(sapply(levels, is.character)))
        stop("levels should be a character vector.")
    colors <- RColorBrewer::brewer.pal(3, "Set2")
    # Create and return plot object
    plot.object <- ggplot2::ggplot(data = plot.data) +
        ggplot2::geom_col(ggplot2::aes(y = y, x = x, fill = strata), position = "stack") +
        ggplot2::geom_text(ggplot2::aes(y = perc.died.y + 1,
                                        x = x,
                                        label = perc.died), size = 2, vjust = "bottom") +
        ggplot2::xlab("Priority level") +
        ggplot2::ylab("Number of patients") +
        ggplot2::scale_x_discrete(labels = setNames(levels, letters[1:length(levels)])) + 
        ggplot2::scale_fill_manual(name = "", values = colors[2:3]) +
        ggplot2::theme(legend.position = "bottom",
                       strip.background = ggplot2::element_rect(fill="white"),
                       strip.text = ggplot2::element_text(size = 10, hjust = 0),
                       text = ggplot2::element_text(size = 8),
                       legend.key.size = ggplot2::unit(4, "mm"),
                       legend.key = ggplot2::element_rect(size = 1, colour = "white", linetype = "solid"),
                       plot.margin = ggplot2::unit(c(2,2,2,2),"pt")) +
        ggplot2::facet_wrap(~pretty.name) 
    return (plot.object)
}
