#' Plot Roc
#'
#' Helper function to CreateRocPlot that plots and optionally returns the ggplot object.
#' @param plot.data Data frame. The plot data object. No default.
#' @param device Character vector. Name of the image device to use. Passed to save.plot. Defaults to "eps".
#' @param y.name Character vector of length 1. The name of the y variable. Defaults to NULL in which case the first variable is used.
#' @param x.name Character vector of length 1. The name of the x variable. Defaults to NULL in which case the second variable is used.
#' @param ylab Character vector of length 1. The y label. Defaults to NULL in which case the variable name is used.
#' @param xlab Character vector of length 1. The x label. Defaults to NULL in which case the variable name is used.
#' @param file.name Character vector of length 1. If not NULL the plot object will be saved to disk using the name provided. Defaults to NULL.
#' @param subscript Logical. If TRUE, underscores in pretty names in converted to LaTex expression. Defaults to FALSE.
#' @export
PlotRoc <- function(plot.data, device = "eps", y.name = NULL, x.name = NULL,
                    ylab = NULL, xlab = NULL, file.name = NULL,
                    subscript = FALSE) {
    ## Number of colors and linetypes
    n <- length(levels(as.factor(plot.data$pretty.name)))
    ## Define colors and linetypes
    colors <- RColorBrewer::brewer.pal(n, "Set1")
    line.types <- c(1:n)
    ## Get y and x variables
    if (is.null(y.name))
        y.name <- names(plot.data)[1]
    if (is.null(x.name))
        x.name <- names(plot.data)[2]
    ## Define axis labels
    if (is.null(ylab))
        ylab <- gsub("EP", ")", gsub("BP", "(", gsub("_", " ", y.name)))
    if (is.null(xlab))
        xlab <- gsub("EP", ")", gsub("BP", "(", gsub("_", " ", x.name)))
    ## Get unique pretty names
    pretty.names <- as.list(as.character(unique(plot.data$pretty.name)))
    ## Detect underscores as subscripts in pretty names
    if (subscript) {
        pretty.names <- gsub(" ", "$ $", paste0("$", gsub("_", "_{", pretty.name), "}$"))
        ## Coerce to TeX format
        pretty.names <- unique(lapply(pretty.names, latex2exp::TeX))
    }
    axis.limits <- c(0,1)
    plot.object <- ggplot2::ggplot(data = plot.data) +
        ggplot2::geom_line(ggplot2::aes_string(x = x.name, y = y.name, col = "pretty.name",
                                               linetype = "pretty.name"), size = 0.5, alpha = 0.8) +
        ggplot2::xlab(xlab) +
        ggplot2::ylab(ylab) +
        ggplot2::xlim(axis.limits) +
        ggplot2::ylim(axis.limits) +
        ggplot2::scale_colour_manual(name = "", values = colors, labels = pretty.names) +
        ggplot2::scale_linetype_manual(name = "", values = line.types, labels = pretty.names) +
        ggplot2::theme(legend.position = "bottom",
                       strip.background = ggplot2::element_rect(fill="white"),
                       strip.text = ggplot2::element_text(size = 12, hjust = 0),
                       text = ggplot2::element_text(size = 8),
                       legend.key.size = ggplot2::unit(4, "mm"),
                       legend.key = ggplot2::element_rect(size = 1, colour = "white", linetype = "solid"),
                       plot.margin = ggplot2::unit(c(2,2,2,2),"pt")) +
        ggplot2::guides(colour = ggplot2::guide_legend(nrow = ceiling(n/3))) +
        ggplot2::facet_wrap(~set) +
        ggplot2::coord_equal(ratio=1)

    return (plot.object)
}
