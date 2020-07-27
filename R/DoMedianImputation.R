#' Do median imputation function
#'
#' Performs median imputation on variables with missing data
#' @param study.sample Data frame. The study sample. No default.
#' @param missing.indicator Character vector of length 1. Used to idenfity variables with missing data to be imputed. Defaults to "_missing"
#' @export
DoMedianImputation <- function(study.sample, missing.indicator = "_missing") {
    ## Error handling
    if (!is.data.frame(study.sample))
        stop("Study data is not a data frame")
    if (length(missing.indicator) != 1)
        stop("Missing indicator length is not 1")
    ## Find variables to impute
    variables.to.impute <- sub(missing.indicator, "", grep(missing.indicator, colnames(study.sample), value = TRUE))
    ## Impute those variables
    study.sample[, variables.to.impute] <- lapply(study.sample[, variables.to.impute], function(var) {
        if (is.factor(var))
            most.common.value <- names(which.max(table(var))) # If the variable is a factor replace missing with the most common level
        if (is.numeric(var))
            most.common.value <- median(var, na.rm = TRUE) # If the variable is numeric replace with median
        var[is.na(var)] <- most.common.value 
        return(var)
    })
    return(study.sample)
}
