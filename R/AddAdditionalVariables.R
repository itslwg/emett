#' AddAdditionalVariables
#'
#' Adds time between injury and arrival, ICU, ISS, and the composite outcome as presented in
#' the thesis published by C Lindgren.
#' @param study.sample data.frame The study sample data frame. No default.
#' @param data.dictionary List. Data dictionary entry. No default.
#' @export
AddAdditionalVariables <- function(study.sample, data.dictionary,
                                   variables.to.drop = c("icu48h",
                                                         "majors24h",
                                                         "iss15")) {
    ## Error handling
    funs <- c("AddTimeBetweenInjuryAndArrival",
              "PredictionModelsAddICU",
              "PredictionModelsAddMajorS24h",
              "PredictionModelsAddISS",
              "PredictionModelsCreateComposite")
    for (f in funs) {
        if (f == "AddTimeBetweenInjuryAndArrival") {
            study.sample <- get(f)(study.sample, data.dictionary)   
        } else {
            study.sample <- get(f)(study.sample)   
        }
    }
    study.sample <- study.sample %>% dplyr::select(-variables.to.drop)
    
    return (study.sample)
}

