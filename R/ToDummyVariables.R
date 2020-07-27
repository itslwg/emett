#' To dummy variables
#'
#' This function transforms factor variables to dummy variables.
#' @param study.sample data.frame. The study.sample. No default.
#' @param not.feature.variables Character vector. Variables in study.sample not to be treated as features. Defaults to c("s30d", "tc", "composite").
#' @export
ToDummyVariables <- function(study.sample, not.feature.variables = c("s30d", "tc", "composite")) {
    ## Error handling
    if (!is.data.frame(study.sample))
        stop("study.sample must be a data frame")
    ## Transform the factor feature variables to dummy variables
    features <- study.sample[, !(names(study.sample) %in% not.feature.variables)]
    dummies <- caret::dummyVars(formula = ~., data = features)
    dummy.features <- predict(dummies, newdata = features)
    study.sample <- cbind(dummy.features, study.sample[, not.feature.variables])
    return(study.sample)
}
