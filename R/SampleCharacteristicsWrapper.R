#' Sample Characterstics Wrapper
#'
#' A wrapper for bengaltiger::CreateSampleCharacteristicsTable. Includes the variables listed in the data
#' dictionary.
#' @param study.sample Data frame. The study sample. No default.
#' @param data.dictionary List. The data dictionary entries. No default.
#' @param ... Additional arguments for bengaltiger::CreateSampleCharacteristicsTable. 
#' @export
SampleCharactersticsWrapper <- function(study.sample, data.dictionary,
                                        ...) {
    ## Error handling
    incl <- names(grep("Yes", sapply(data.dictionary, "[[", "incl"), value = TRUE))
    variables <- colnames(study.sample)[colnames(study.sample) %in% incl]
    sample.characteristics.table <- bengaltiger::CreateSampleCharacteristicsTable(study.sample,
                                                                                  variables = variables,
                                                                                  ...)
    return (sample.characteristics.table)
}
