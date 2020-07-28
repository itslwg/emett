#' PrepareSample
#'
#' Reads, merges, and prepares the data.
#' @param study.variables Character vector. Variables to use in the study. No default.
#' @export
PrepareSample<- function(study.variables) {
    ## Get data dictionary
    data.dictionary <- GetDataDictionary()
    ## Import study data
    study.sample <- ImportAndMergeData()
    study.sample <- PrepareStudySample(study.sample = study.sample[, study.variables],
                                       data.dictionary = data.dictionary)
    ## Create bengaltiger study sample (Not included the correct inclusion.criteria functions)
    inclusion.criteria <- c(OnlyInformedConsent, bengaltiger::OnlyAdults, OnlyPatientsWithCompleteOutcome)
    study.sample <- bengaltiger::CreateStudySample(study.data = study.sample, complete.cases = FALSE,
                                                   relevant.variables = names(study.sample),
                                                   inclusion.criteria = inclusion.criteria)
    flowchart <- bengaltiger::CreateFlowchart(compile.flowchart=TRUE)
    ## Prepare the study sample, i.e. redefine levels of factors using the data dictionary and set missing
    study.sample <- CollapseMechanismOfInjury(study.sample = study.sample)
    study.sample <- SetToOutcome(study.sample)
    ## Add additional features and group/collapse mechanism of injury
    study.sample <- AddAdditionalVariables(study.sample, data.dictionary)
    ## Remove informed conxsent variable from study sample
    study.sample[, "ic"] <- NULL
    ## Create sample characteristics
    sample.characteristics.table <- SampleCharactersticsWrapper(study.sample, data.dictionary,
                                                                save.to.disk = TRUE, file.format = "pdf")
    ## Transform features to dummy variables
    study.sample <- ToDummyVariables(study.sample = study.sample)
    ## Add missing indicator variables
    study.sample <- AddMissingIndicatorVariables(study.sample = study.sample)
    ## Do median imputation, i.e. impute continuous with median and factors with mode
    study.sample <- DoMedianImputation(study.sample = study.sample)

    return (study.sample)
}
