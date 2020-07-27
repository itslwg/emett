#' RunStudy
#'
#' Conducts data fetching and preparation, modelling, predictions, and summarizes results.
#' @export
RunStudy <- function(verbose = TRUE) {
    library(devtools)
    devtools::load_all()
    print_trace_back <- function() {
        print(rlang::trace_back(bottom = sys.frame(-1)))
    }
    options(error = print_trace_back)
    if (verbose)
        message("Preparing the sample...")
    ## Set parameters that are default in make.study
    n.bootstrap.samples <- 2
    n.partitions <- 3
    ## Define study name, authors, description of the study, and relevant variables
    study.name <- "SuperLearner vs Clinicians"
    authors <- c("Martin Gerdin Wärnberg", "Ludvig Wärnberg Gerdin")
    description <- "A comparison of triage conducted by clinicians and the ensemble algorithm SuperLearner."
    study.variables <- c("age", "moi", "sex", "mot", "tran", "s30d", "egcs", "mgcs", "vgcs", "avpu", "hr",
                            "sbp", "dbp", "spo2", "rr", "tc", "ic", "doar", "toar", "doi", "toi", "s24h", "hd",
                            "taicu", "daicu", "tos", "dos", "nomesco", "snomed", "s", "iss")
    pretty.names <- c("SuperLearner", "Random Forest")
    study.sample <- PrepareSample(
        study.variables=study.variables
    )
    settings <- list(
        s30d=list(
            outcome.label="s30d",
            variables.to.drop=c(
                "nomesco",
                "snomed",
                "s",
                "iss",
                "taicu",
                "daicu",
                "composite"
            )
        )
        # composite=list(
        #     outcome.label="composite",
        #     variables.to.drop=c("s30d")
        # )
    )
    ## Run modelling on the outcomes of interest    
    statistics <- lapply(settings, function(analysis.settings) {
        variables.to.drop <- analysis.settings$variables.to.drop
        print(variables.to.drop)
        print(grepl(paste0("\\b", variables.to.drop, "\\b", collapse="|"), names(study.sample)))
        print(names(study.sample))
        outcome.variable.name <- analysis.settings$outcome.label
        study.sample=study.sample[, !grepl(paste0("\\b", variables.to.drop, "\\b", collapse="|"),
                                           names(study.sample), fixed=TRUE, names(study.sample))]
        print(str(study.sample))
        return (RunModelling(study.sample, outcome.variable.name))
    })
    ## Summarize the results
                                        # SummarizeResults()
    print ("Study analysis complete.")
}

