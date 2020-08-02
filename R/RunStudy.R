#' RunStudy
#'
#' Conducts data fetching and preparation, modelling, predictions, and summarizes results.
#' @export
RunStudy <- function(verbose = TRUE) {
    print_trace_back <- function() {
        print(rlang::trace_back(bottom = sys.frame(-1)))
    }
    options(error = print_trace_back)
    if (verbose)
        message("\n~~~~~~~~~~~~~~~~~~~~~~~\nPreparing the sample...\n~~~~~~~~~~~~~~~~~~~~~~~\n")
    ## Set parameters that are default in make.study
    n.bootstrap.samples <- 1
    n.partitions <- 3
    study.variables <- c("age", "moi", "sex", "mot", "tran", "s30d", "egcs", "mgcs", "vgcs", "avpu", "hr",
                            "sbp", "dbp", "spo2", "rr", "tc", "ic", "doar", "toar", "doi", "toi", "s24h", "hd",
                            "taicu", "daicu", "tos", "dos", "nomesco", "snomed", "s", "iss")
    pretty.model.names <- c("SuperLearner", "Random Forest")
    ## Fetch and merge datasets and conduct feature engineering
    study.sample <- PrepareSample(study.variables=study.variables)
    ## Run modelling on the outcomes of interest
    settings <- list(
        s30d=list(
            outcome.label="s30d",
            variables.to.drop=c(
                "s24h",
                "composite"
            )
        ),
        composite=list(
            outcome.label="composite",
            variables.to.drop=c(
                "s30d",
                "s24h"
            )
        )
    )
    statistics <- lapply(settings, function(s) {
        variables.to.drop <- s$variables.to.drop
        outcome.variable.name <- s$outcome.label
        study.sample=study.sample[
          , !grepl(paste0(variables.to.drop, collapse="|"), names(study.sample))
        ]
        return (
            RunModelling(study.sample, outcome.variable.name,
                         n.bootstrap.samples=n.bootstrap.samples)
        )
    })
    ## Summarize the results
    ## SummarizeResults(statistics)
    message("Study analysis complete.")
}

