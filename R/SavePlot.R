#' Save plot
#'
#' Wrapper for ggsave that also crops and converts to eps if necessary
#' @param plot.object The ggplot object. No default.
#' @param file.name Character vector of length 1. The plot file name. No default.
#' @param width Numeric vector of length 1. The width of the plot in cm. Defaults to 13.2.
#' @param height Numeric vector of length 1. The height of the plot in cm. Default to 9.
#' @param device Character vector of length 1. Name of the image device to use. Defaults to "eps".
#' @param convert.pdf.to.eps Logical. If TRUE and device = "pdf" then an attempt to convert that pdf using pdftops to eps is made. Defaults to TRUE.
#' @export
SavePlot <- function(plot.object, file.name, width = 13.2, height = width * 0.65,
                     device = "eps", convert.pdf.to.eps = TRUE) {
    ## Setup device
    font.device <- device
    if (device == "eps")
        font.device = "postscript"
    ## Generate full file name
    full.file.name <- paste0(file.name, ".", device)
    ## Setup ggplot arguments depending on device
    ggargs <- list(filename = full.file.name,
                   plot = plot.object,
                   device = font.device,
                   width = width,
                   height = height,
                   units = "cm",
                   family = "ArialMT")
    if (device == "eps") {
        ggargs$fallback_resolution <- 300
        ggargs$device <- cairo_ps
    }
    ## Save plot
    extrafont::loadfonts(device = font.device, quiet = TRUE)
    do.call(ggplot2::ggsave, ggargs)
    ## Embed fonts
    ## embed_fonts(full_file_name, outfile = full_file_name, options = "-dEPSCrop")
    ## Report that plot is saved
    message(paste0(full.file.name, " saved to disk"))
    ## Convert to eps
    if (convert.pdf.to.eps & device == "pdf") {
        eps.name <- paste0(file.name, ".eps")
        ConvertToEps <- function() {
            system(paste("pdftops -eps", full.file.name, eps.name))
            message(paste0(full.file.name, " converted to ", eps.name))
        }
        tryCatch(ConvertToEps(),
                 warning = function(w) print(w),
                 error = function(e) print(e))
    }
}

