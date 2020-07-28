#' Format Xtable
#'
#' Adds additional environments to the xtable.
#' @param tex.table Character vector of length 1. The xtable output string. No default.
#' @export
FormatXtable <- function(tex.table, substring.to.comment = NULL,
                         footnote = NULL) {
    ## Error handling
    if (is.character(tex.table) & bengaltiger::IsLength1(tex.table))
        stop("tex.table must be a character vector of length 1.")
    tex.table <- sub("\\begin{tabular}",
                     paste0("\\begin{adjustbox}{max width = \\linewidth} \n",
                            "\\begin{threeparttable} \n",
                            "\\begin{tabular} \n"),
                     tex.table,
                     fixed = TRUE)
    if (!is.null(substring.to.comment))
        tex.table <- gsub(substring.to.comment, paste(substring.to.comment, "\\tnote{*}"),
                          tex.table)
    if (!is.null(footnote)) {
        footnote <- ifelse(is.null(substring.to.comment),
                           sprintf("\\item[*] %s \n", footnote),
                           sprintf("\\item %s \n", footnote))
        table.notes <- paste0("\\begin{tablenotes} \\footnotesize \n",
                              footnote,
                              "\n",
                              "\\end{tablenotes} \n")   
    }
    ## Add end to tabular and adjustbox environments
    the_xtable <- sub("\\end{tabular}",
                      paste0("\\addlinespace \n",
                             "\\end{tabular} \n",
                             table.notes,
                             "\\end{threeparttable} \n",
                             "\\end{adjustbox}"),
                      the_xtable,
                      fixed = TRUE)
    return (the_xtable)
}
