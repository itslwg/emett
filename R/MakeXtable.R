#' Make Xtable
#'
#' Creates and returns a table using xtable
#' @param df Data frame. A data frame. No default.
#' @param file.dir Character vector of length 1. The file name of the table. No default. 
#' @param save.table Logical. If TRUE save the xtable. Defaults to TRUE.
#' @param return.table Logical. If TRUE xtable print is returned. Defaults to FALSE.
#' @param ... Additional arguments for xtable::xtable. 
#' @export
MakeXtable <- function(df, file.dir, save.table = TRUE,
                       return.table = FALSE, ...) {
    ## Error handling
    if (!is.data.frame(df))
        stop("df must be of type data frame")
    tex.table <- xtable::print.xtable(xtable::xtable(df,
                                                     ...),
                                      type = "latex",
                                      booktabs = TRUE,
                                      table.placement = "!ht",
                                      include.colnames = TRUE,
                                      caption.placement = "top",
                                      print.results = FALSE,
                                      add.to.row = "apa")
    if (save.table)
        write(tex.table, file = file.dir)
    if (return.table)
        return (tex.table)
}
