#' AddStarCaption
#'
#' Adds caption*
#' @param xtable Character vector of length 1. The xtable object in latex format. No default..
#' @param star.caption Character vector of length 1. Caption to be added as caption*. No default.
#' @export
AddStarCaption <- function(xtable, star.caption) {
    ## Add star caption to xtable
    table.with.added.caption <- sub("\\end{table}",
                                    paste0("\\caption*{", star.caption, "} \n\\end{table}"),
                                    xtable,
                                    fixed = TRUE)
    return(table.with.added.caption)
}
