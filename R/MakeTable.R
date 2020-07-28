#' Make Table
#'
#' Wrapper for kableExtra::kable.
#' @param df Data frame. A data frame. No default.
#' @param table.name Character vector of length 1. Name/label of the table. Note that this is also Used as the file name if saved to disk. No default.
#' @param footnote Character vector of length 1. Footnote for the table. Defaults to NULL.
#' @param return.table Logical. If TRUE xtable print is returned. Defaults to FALSE.
#' @param save.to.disk Logical. If TRUE the table is saved to disk. Defaults to FALSE.
#' @param file.format Character vector of length 1.The format in which to save the table to disk. Has to be one of c("pdf", "rmd", "docx"). Defaults to "docx".
#' @param ... Additional arguments for kableExtra::kable.
#' @export
MakeTable <- function(df, table.name, footnote = NULL,
                      return.table = FALSE, save.to.disk = FALSE,
                      file.format = "docx", ...) {
    ## Error handling
    if (!is.data.frame(df))
        stop("df must be of type data frame")
    if (!(file.format %in% c("docx", "rmd", "pdf")) | !bengaltiger::IsLength1(file.format)) 
        stop("file.format has to be one of docx, rmd, or pdf")
    preamble <- "library(kableExtra)\nlibrary(knitr)\n"
    call <- paste0(preamble, getCall(...)) # "%>% kable_styling(position = \"center\")")
    print (call)
    if (!is.null(footnote))
        call <- paste(call, "%>% footnote(symbol = footnote)")
    if (save.to.disk) {
        file.name <- paste0(table.name, ".rmd")
        md.call <- paste0("---\n",
                          "header-includes:\n",
                          "- \\usepackage{booktabs}\n",
                          "---\n",
                          "```{r echo = FALSE, results = 'asis'} \n",
                         call, "\n",
                         "```")
        write(md.call, file.name)
        if (file.format != "rmd") {
            output.format.list <- list(docx = "word_document",
                                       pdf = "pdf_document",
                                       html = "html_document")
            rmarkdown::render(file.name, output_format = output.format.list[[file.format]],
                              envir = new.env())
            file.remove(file.name)
        }
    }
    if (return.table)
        return(tbl)
}
getCall <- function(...) {
    Call  <- function(x, format, ...) {
        mc <- substitute(list(df, "latex", ...), parent.frame())
        mc[[1]] <- match.call()[-1][[1]][[1]]
        return (mc)
    }
    test <- Call(kable(...))
    return (deparse(test))
}
