#' Prepare Sample
#'
#' Reads, merges, and prepares the data.
#' @param study.variables Character vector. Variables to use in the study. No default.
#' @param settings List.
#' @export
PrepareSample <- function(study.variables, settings) {
    ## Get data dictionary
    data.dictionary <- GetDataDictionary()
    ## Import study data
    study.sample <- ImportAndMergeData()
    study.sample <- PrepareStudySample(study.sample = study.sample[, study.variables],
                                       data.dictionary = data.dictionary)
    ## Create bengaltiger study sample (Not included the correct inclusion.criteria functions)
    inclusion.criteria <- c(
        OnlyInformedConsent,
        bengaltiger::OnlyAdults,
        OnlyCompleteTriageCategory
    )
    ico <- ifelse(settings$outcome.label == "s30d", OnlyCompleteS30d, OnlyCompleteCompositeOutcome)
    inclusion.criteria <- c(inclusion.criteria, ico)
    ## Add additional features and group/collapse mechanism of injury
    study.sample <- AddAdditionalVariables(study.sample, data.dictionary)
    ## Prepare the study sample, i.e. redefine levels of factors using the data dictionary and set missing
    study.sample <- CollapseMechanismOfInjury(study.sample = study.sample)
    suppressWarnings({
        bengaltiger::SaveToResults(study.sample, "raw.study.sample")  
    })
    suppressWarnings({
        study.sample <- bengaltiger::CreateStudySample(study.data = study.sample, complete.cases = FALSE,
                                                       relevant.variables = names(study.sample),
                                                       inclusion.criteria = inclusion.criteria)  
    })
    study.sample <- SetToOutcome(study.sample)
    ## Create a study flowchart
    flowchart <- bengaltiger::CreateFlowchart(compile.flowchart=TRUE,
                                              flowchart.file.path=paste0(
                                                  "./figures/",
                                                  settings$outcome.label,
                                                  ".flowchart.tex"
                                              ),
                                              print.tikz=FALSE,
                                              intern=TRUE)
    ## Extract descriptive statistics from flowchart
    ns <- ExtractDescriptiveStatistics(settings = settings)
    ## Remove informed conxsent variable from study sample
    study.sample[, "ic"] <- NULL
    ## Save the sample pre data-preprocessing
    saveRDS(
        object=study.sample,
        file="raw.study.sample.Rds"
    )
    ## Create sample characteristics
    sample.characteristics.table <- SampleCharactersticsWrapper(study.sample, data.dictionary, settings)
    ## Transform features to dummy variables
    study.sample <- ToDummyVariables(study.sample = study.sample)
    ## Add missing indicator variables
    study.sample <- AddMissingIndicatorVariables(study.sample = study.sample)
    ## Do median imputation, i.e. impute continuous with median and factors with mode
    study.sample <- DoMedianImputation(study.sample = study.sample)
    return (study.sample)
}
