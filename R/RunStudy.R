#' RunStudy
#'
#' Conducts data fetching and preparation, modelling, predictions, and summarizes results.
#' @export
RunStudy <- function(verbose = TRUE) {
    options(knitr.table.format = "latex") 
    print_trace_back <- function() {
        print(rlang::trace_back(bottom = sys.frame(-1)))
    }
    options(error = print_trace_back)
    if (verbose)
        message("\n~~~~~~~~~~~~~~~~~~~~~~~\nPreparing the sample...\n~~~~~~~~~~~~~~~~~~~~~~~\n")
    ## Set parameters that are default in make.study
    n.bootstrap.samples <- 2
    n.partitions <- 3
    study.variables <- c("age", "moi", "sex", "mot", "tran", "s30d", "egcs", "mgcs", "vgcs", "avpu", "hr",
                            "sbp", "dbp", "spo2", "rr", "tc", "ic", "doar", "toar", "doi", "toi", "s24h", "hd",
                            "taicu", "daicu", "tos", "dos", "nomesco", "snomed", "s", "iss")
    pretty.model.names <- c("SuperLearner", "Random Forest")
    settings <- list(
        s30d.results=list(
            outcome.label="s30d",
            variables.to.drop=c(
                "s24h",
                "composite"
            )
        ),
        composite.results=list(
            outcome.label="composite",
            variables.to.drop=c(
                "s30d",
                "s24h",
                "composite_missing"
            )
        )
    )
    ## Fetch, merge datasets and conduct feature engineering
    study.sample <- PrepareSample(
        study.variables=study.variables,
        settings=settings
    )
    partitions <- list()
    modelling.list <- lapply(settings, function(s) {
        variables.to.drop <- s$variables.to.drop
        outcome.variable.name <- s$outcome.label
        study.sample <- study.sample[
          , !grepl(paste0(variables.to.drop, collapse="|"), names(study.sample))
        ]
        partitions[[paste0("partitions.", outcome.variable.name)]] <<- PartitionSample(
            study.sample=study.sample,
            outcome.variable.name=outcome.variable.name,
            n.partitions=3
        )
        m <- RunModelling(study.sample, outcome.variable.name,
                          n.bootstrap.samples=n.bootstrap.samples,
                          save.samples = FALSE)
        ts <- list(test.sample=PartitionSample(study.sample, outcome.variable.name, n.partitions)$test$x)
        r <- c(m, ts)
        
        return (r)
    })
    saveRDS(partitions, "./partitions.Rds")
    ## Summarize the results
    SummarizeResults()
    ## Render manuscript
    results <- readRDS("results.Rds")
    rmarkdown::render("./manuscript.Rmd")
    message("Study analysis complete.")
}

