#' Add missing indicator variables
#'
#' Creates missing indicator variables for each predictor variable (feature) with missing values
#' @param study.sample Data Frame. The study sample. No default
#' @param outcome.name String. The name of outcome variable. Defaults to "s30d"
#' @param features Character vector. The names of the feature variables. Defaults to NULL, in which case all columns except outcome.name and those potentially listed in excluded.columns are treated as features.
#' @param excluded.columns Character vector. The names of columns that should not be treated as features. Defaults to NULL and is then ignored. Is also ignored if features is specified.
#' @export
AddMissingIndicatorVariables <- function(study.sample, outcome.name = "s30d",
                                         features = NULL, excluded.columns = c("tc")) {
    ## Test that all columns specified in outcome.name, features and excluded
    ## features (if any) are in study.data
    column.names <- colnames(study.sample)
    stopifnot(all(c(outcome.name, features, excluded.columns) %in% column.names))
    ## Create vector of features
    if (is.null(features))
        features <- column.names[!(column.names %in% c(outcome.name, excluded.columns))]
    ## Get feature data only
    feature.data <- study.sample[, features]
    ## Define function to get either number of missing values or indicator variable
    GetMissingInfo <- function(x, n = TRUE, variable = FALSE) {
        n.missing <- sum(is.na(x))
        indicator.variable <- NULL
        if (n.missing > 0)
            indicator.variable <- as.numeric(!is.na(x))
        if (n & !variable)
            return(n.missing)
        if (variable)
            return(indicator.variable)
    }
    ## Count number of missing values in each feature
    missing.data.counts <- lapply(feature.data, GetMissingInfo)
    missing.data.matrix <- do.call(rbind, missing.data.counts)
    ## Create a dummy variable for each feature with missing
    indicator.variables <- lapply(feature.data, GetMissingInfo, variable = TRUE)
    indicator.data <- do.call(cbind, indicator.variables)
    colnames(indicator.data) <- paste0(colnames(indicator.data), "_missing")
    study.sample <- data.frame(study.sample, indicator.data)
    return(study.sample)
}
