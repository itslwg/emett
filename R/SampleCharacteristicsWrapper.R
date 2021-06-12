#' Sample Characterstics Wrapper
#'
#' A wrapper for bengaltiger::CreateSampleCharacteristicsTable. Includes the variables listed in the data
#' dictionary.
#' @param study.sample Data frame. The study sample. No default.
#' @param data.dictionary List. The data dictionary entries. No default.
#' @param settings List. Settings for the different outcomes in the study. No default. 
#' @param ... Additional arguments for bengaltiger::CreateSampleCharacteristicsTable. 
#' @export
SampleCharactersticsWrapper <- function(study.sample, data.dictionary,
                                        settings, ...) {
    ## Subset variable names to be included
    incl <- data.dictionary %>%
        purrr::map( ~.x[["incl"]] == "Yes") %>%
        purrr::keep( ~.x) %>%
        names()
    variables <- colnames(study.sample)[colnames(study.sample) %in% incl]
    ## Create multiple table-ones, for multiple outcomes
    variables.to.drop <- settings$variables.to.drop
    outcome.variable.name <- settings$outcome.label
    variables <- c(variables[!(variables %in% variables.to.drop)], "partition")
    ## Move outcome variable name to the end
    variables <- variables[!(variables %in% outcome.variable.name)]
    variables <- c(variables, outcome.variable.name)
    partitions <- PartitionSample(
        study.sample,
        outcome.variable.name,
        n.partitions=3,
        extract.outcome=FALSE
    )
    column.order <- c(
        "Characteristic",
        "Level",
        "train",
        "validation",
        "test",
        "Overall"
    )
    ## Create table-one and save to results
    tblone <- do.call(rbind, partitions) %>%
        dplyr::mutate(partition=gsub("\\..*", "", rownames(.))) %>%
        bengaltiger::CreateSampleCharacteristicsTable(
                         variables=variables,
                         group="partition",
                         codebook=data.dictionary,
                         codebook.options=list(
                             full.label.entry="l",
                             abbreviated.label.entry="al"
                         ),
                         only.codebook.variables=FALSE,
                         ) %>%
        dplyr::select(all_of(column.order)) %>%
        dplyr::rename(Train=train, Validation=validation, Test=test)
    ## Extract missing values from each variable
    n <- nrow(tblone)
    missing.column <- rep("0 (0)", n)
    indices <- grep(" ", tblone[, "Characteristic"])  # Indexes where not missing (i.e. not levels)
    miss <- sapply(variables[!(variables %in% "partition")], function(var) {
        subset <- study.sample[, var]
        return (sum(is.na(subset)))
    })
    ## Totalling
    total.missing <- sum(miss)
    total.missing.perc <- round(total.missing / nrow(study.sample) * 100, 1)
    total.missing.fmt <- paste(total.missing, paste0("(", total.missing.perc, ")"))
    ## Each variable
    missing.perc <- (miss / nrow(study.sample)) * 100
    missing.vals.fmt <- paste(round(miss), paste0("(", round(missing.perc, 1), ")"))
    missing.vals.fmt <- c(total.missing.fmt, missing.vals.fmt) ## Attach total
    missing.column[indices] <- missing.vals.fmt
    missing.column[grep(" ", tblone[, "Characteristic"], invert = TRUE)] <- " "
    ## Attach to sample characteristics
    tblone <- cbind(tblone, mv = missing.column)
    colnames(tblone)[colnames(tblone) == "mv"] <- "Missing values, n (%)"
    ## Save to results
    suppressWarnings({
        bengaltiger::SaveToResults(tblone, paste0(outcome.variable.name, ".formatted.sample.characteristics"))  
    })
}
